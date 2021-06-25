*          DATA SET ACREPX402A AT LEVEL 081 AS OF 05/01/02                      
*PHASE ACX402A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SECRET                                                                 
         TITLE 'STAMP PIDELS - TEMPO/TIME'                                      
***********************************************************************         
* DO NOT DELETE!!!!!!!!!!!!!  PROGRAM CURRENTLY BEING USED!!!!!!!!!!! *         
***********************************************************************         
*      OPT1:'Y'= UPDATE ALL RECORDS                                   *         
*                                                                     *         
*      OPT2:'A'= STAMP TEMPO X-REF/TIME RECORDS                       *         
*           '1'= STAMP ONLY TEMPO X-REF                               *         
*           '2'= STAMP ONLY TIME RECORDS                              *         
*                                                                     *         
*      OPT3:'Y'= ONLY CHANGE NUMBER WITH THE WRONG NUMBER             *         
*                                                                     *         
*      OPT4:'Y'= DELETE PIDS THAT DO NOT HAVE ANY MATCH IN SECURITY   *         
*                                                                     *         
***********************************************************************         
ACX402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACX4**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACX4D,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
EXIT     XIT1  1                                                                
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         BAS   RE,GETSEC           GET SECURITY ID                              
*                                                                               
         USING PERTABD,R6                                                       
         L     R6,APERTAB                                                       
         XC    PERTAB#,PERTAB#                                                  
*                                                                               
         LR    RE,R6               RE=A(PERSON/PID TABLE)                       
         LA    RF,PERTBLNQ         RF=(LENGTH OF PERSON TABLE)                  
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR PERSON TABLE                           
*                                                                               
         USING PERRECD,R4                                                       
         LA    R4,SVKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,RCCOMPFL                                                 
         MVC   PERKCODE,QAPPL      PERSON CODE - IF ANY                         
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     REQF20                                                           
*                                                                               
REQF10   GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
REQF20   LA    R1,2                                                             
         CLC   QAPPL,SPACES                                                     
         BNH   *+8                                                              
         LA    R1,10                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),SVKEY                                                   
         BNE   REQF30                 CAN'T FIND THIS BUD                       
*                                                                               
         LA    R4,IO               SET R4 TO POINT TO THE RECORD                
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         USING PIDELD,R2                                                        
         LR    R2,R4                                                            
         MVI   ELCODE,PIDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   REQF10                                                           
*                                                                               
         MVC   SVPID,PIDNO         PID NUMBER                                   
         MVC   SVPERCDE,PERKCODE   PERSON CODE                                  
         BAS   RE,GETPID#          GET PID NUMBER                               
         BE    REQF10              FIND PID IN SEC - SKIP IT                    
*                                                                               
         MVC   PERCODE,SVPERCDE    PERSON CODE  TO TABLE                        
         MVC   PERPID,SVPID        PID NUMBER   TO TABLE                        
         MVC   PERDA,DSKADR        IF GETREC BEFORE THIS IS NOT PERSON          
*                                  RECORD THEN WRONG DA IN PERDA                
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,PERTAB#                                                     
         LA    R1,1(R1)                                                         
         CHI   R1,PERTBLNQ         DONT OVER RUN THE TABLES END                 
         BL    *+6                                                              
         DC    H'0'                IF IT DIES HERE-INCREASE TABLE               
         STCM  R1,3,PERTAB#                                                     
         LA    R6,PERLNQ(R6)                                                    
         B     REQF10                                                           
*                                                                               
* PRINT OUT PERCODE AND PERSONAL ID#, PERSON DISK ADDRESS                       
*                                                                               
REQF30   CLI   QOPT4,C'Y'                                                       
         BNE   TIME                                                             
         BAS   RE,LOOKTMS          LOOK AHEAD AT THE TIME RECORDS               
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,PERTAB#                                                     
         BZ    EXIT                                                             
*                                                                               
         USING PERTABD,R6                                                       
         L     R6,APERTAB                                                       
REQF40   MVC   P(L'PERCODE),PERCODE                                             
         SR    R7,R7                                                            
         ICM   R7,3,PERPID                                                      
         EDIT  (R7),(10,P+20)                                                   
*                                                                               
         TM    PERSTAT,PERTEMPO    DOES THIS PID HAVE A TEMPO T/S               
         BNO   *+10                                                             
         MVC   P+62(9),=CL9'**TEMPO**'                                          
         GOTO1 HEXOUT,DMCB,PERDA,P+82,4,0,8    DISK ADDRESS PERSN REC           
*                                                                               
         GOTO1 ACREPORT                                                         
         LA    R6,PERLNQ(R6)                                                    
         BCT   R2,REQF40                                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* UPDATE TIMESHEET TEMPO X-REF RECORDS                                *         
***********************************************************************         
         SPACE 1                                                                
TEMPO    CLI   QOPT4,C'Y'                                                       
         BNE   TIME                                                             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,2                                                       
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
*                                                                               
         ZAP   CHGCNT,=P'0'                                                     
         ZAP   DUMPCNT,=P'0'                                                    
         USING TSXRECD,R4                                                       
         LA    R4,SVKEY                                                         
         XC    TSXKEY,TSXKEY                                                    
         MVI   TSXKTYP,TSXKTYPQ    X'3E'                                        
         MVI   TSXKSUB,TSXKSUBQ    X'13'                                        
         MVC   TSXKCPY,RCCOMPFL                                                 
         MVC   TSXKPER,QAPPL       PERSON CODE - IF ANY                         
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     TEMP20                                                           
*                                                                               
TEMP10   GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
TEMP20   LA    R1,3                                                             
         CLC   QAPPL,SPACES                                                     
         BNH   *+8                                                              
         LA    R1,11                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),SVKEY                                                   
         BNE   TEMPX                                                            
*                                                                               
         LA    R4,IO                                                            
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         USING PIDELD,R2                                                        
         LR    R2,R4                                                            
         MVI   ELCODE,PIDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   TEMP10                                                           
*                                                                               
         USING PERTABD,R6                                                       
         L     R6,APERTAB                                                       
         SR    R0,R0                                                            
         ICM   R0,3,PERTAB#                                                     
TEMP30   CLC   PERCODE,TSXKPER     PERSON CODE                                  
         BE    TEMP40                                                           
TEMP35   LA    R6,PERLNQ(R6)                                                    
         BCT   R0,TEMP30                                                        
         B     TEMP10              NOT IN TABLE                                 
*                                                                               
*        OC    PIDNO,PIDNO                                                      
*        BZ    TEMP10                                                           
*        CLC   PIDNO,PERPID        SAME NUMBERS?                                
*        BNE   TEMP35                                                           
TEMP40   CLI   QOPT3,C'Y'          ONLY CHANGE TMS RECORDS                      
         BNE   *+12                                                             
         TM    PERSTAT,PERTEMPO    ARE THESE TEMPO RECORDS?                     
         BO    TEMP10                                                           
*                                                                               
         MVC   PCODE,TSXKPER       PERSON CODE TO PRINT LINE                    
         GOTO1 DATCON,DMCB,(1,TSXKEND),(X'20',PTEMEND)                          
         MVC   PTEMODS,TSXKODS     MOVE OFF/DEPT/SUBD TO PRINT LINE             
         SR    R3,R3                                                            
         ICM   R3,3,PIDNO                                                       
         EDIT  (R3),PTEMOPID                                                    
*                                                                               
         MVC   MSG,=CL10'GET TEMPO'                                             
         SR    R3,R3                                                            
         ICM   R3,3,TSXRLEN                                                     
         GOTO1 DUMP,DMCB,IO,(R3)                                                
*                                                                               
         AP    CHGCNT,=P'1'                                                     
         XC    PIDNO,PIDNO         CLEAR PID NUMBER                             
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,PIDNO                                                       
         EDIT  (R3),PTEMNPID                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   MSG,=CL10'PUT TEMPO'                                             
         SR    R3,R3                                                            
         ICM   R3,3,TSXRLEN                                                     
         GOTO1 DUMP,DMCB,IO,(R3)                                                
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   TEMP10                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    TEMP10                                                           
         GOTO1 =A(DMPUTREC),DMCB,(RC)                                           
         B     TEMP10                                                           
*                                                                               
TEMPX    DS    0H                                                               
         MVC   P+1(39),=C'TOTAL OF TEMPO X-REF RECORDS CHANGED : '              
         EDIT  (P8,CHGCNT),(10,P+40),ZERO=NOBLANK                               
         GOTO1 ACREPORT                                                         
         DROP  R2,R4,R6,R7                                                      
         EJECT                                                                  
***********************************************************************         
* UPDATE TIME RECORDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
TIME     CLI   QOPT4,C'Y'          DO THEY WANT TO CHANGE PIDELS?               
         BNE   PERSON                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,3                                                       
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
*                                                                               
         ZAP   CHGCNT,=P'0'                                                     
         ZAP   DUMPCNT,=P'0'                                                    
*                                                                               
         USING TSWRECD,R4                                                       
         LA    R4,SVKEY                                                         
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,RCCOMPFL                                                 
         MVC   TSWKPER,QAPPL       PERSON CODE - IF ANY                         
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     TIME20                                                           
*                                                                               
TIME10   GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
TIME20   LA    R1,3                                                             
         CLC   QAPPL,SPACES                                                     
         BNH   *+8                                                              
         LA    R1,11                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),SVKEY                                                   
         BNE   TIMEX                                                            
*                                                                               
         LA    R4,IO                                                            
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         USING TIMRECD,R4                                                       
*        LA    R4,IO                                                            
*                                                                               
         BAS   RE,SETCDE                                                        
*                                                                               
         USING PIDELD,R2                                                        
         LR    R2,R4                                                            
         MVI   ELCODE,PIDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   TIME10                                                           
*                                                                               
         USING PERTABD,R6                                                       
         L     R6,APERTAB                                                       
         SR    R0,R0                                                            
         ICM   R0,3,PERTAB#                                                     
TIME30   CLC   PERCODE,LEVDCDE     PERSON CODE                                  
         BE    TIME40                                                           
TIME35   LA    R6,PERLNQ(R6)                                                    
         BCT   R0,TIME30                                                        
         B     TIME10              NOT IN TABLE                                 
*                                                                               
TIME40   DS    0H                                                               
         CLC   PIDNO,PERPID        IF NUMBERS ARE THE SAME SKIP                 
         BNE   TIME35                                                           
         CLI   QOPT3,C'Y'                                                       
         BNE   *+12                                                             
         TM    PERSTAT,PERTEMPO                                                 
         BO    TIME10                                                           
*                                                                               
         MVC   MSG,=CL10'GET TIME'                                              
         SR    R3,R3                                                            
         ICM   R3,3,TIMRLEN                                                     
         GOTO1 DUMP,DMCB,IO,(R3)                                                
*                                                                               
         MVC   PCODE,LEVDCDE       PERSON CODE TO PRINT LINE                    
         MVC   PTIMACC,TIMKULA     ACCOUNT TO PRINT LINE                        
         MVC   PTIMOFF,TIMKOFF     OFF TO PRINT LINE                            
         MVC   PTIMCAC,TIMKULC     CONTRA ACCOUNT TO PRINT LINE                 
         GOTO1 DATCON,DMCB,(1,TIMKPEDT),(X'20',PTIMEND)                         
         SR    R3,R3                                                            
         ICM   R3,3,PIDNO                                                       
         EDIT  (R3),PTIMOPID                                                    
*                                                                               
         AP    CHGCNT,=P'1'                                                     
         BAS   RE,DELELM           DELETE X'D8' ELEMENT ROUTINE                 
*                                                                               
         SR    R3,R3                                                            
         EDIT  (R3),PTIMNPID                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   MSG,=CL10'PUT TIME'                                              
         SR    R3,R3                                                            
         ICM   R3,3,TIMRLEN                                                     
         GOTO1 DUMP,DMCB,IO,(R3)                                                
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   TIME10                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    TIME10                                                           
         GOTO1 =A(DMPUTREC),DMCB,(RC)                                           
         B     TIME10                                                           
*                                                                               
TIMEX    DS    0H                                                               
         MVC   P+1(39),=C'TOTAL OF TIME RECORDS CHANGED        : '              
         EDIT  (P8,CHGCNT),(10,P+40),ZERO=NOBLANK                               
         GOTO1 ACREPORT                                                         
         DROP  R2,R4,R6,R7                                                      
***********************************************************************         
* UPDATE PERSON RECORDS                                               *         
***********************************************************************         
         SPACE 1                                                                
PERSON   CLI   QOPT4,C'Y'                                                       
         BNE   PASSIVE                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,4                                                       
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
*                                                                               
         ZAP   CHGCNT,=P'0'                                                     
         ZAP   DUMPCNT,=P'0'                                                    
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,PERTAB#                                                     
         BZ    PERSX                                                            
*                                                                               
         USING PERTABD,R3                                                       
         L     R3,APERTAB                                                       
*                                                                               
PERS10   CLI   QOPT3,C'Y'                                                       
         BNE   *+12                                                             
         TM    PERSTAT,PERTEMPO                                                 
         BO    PERS40                                                           
*                                                                               
         USING PERRECD,R4                                                       
         LA    R4,SVKEY                                                         
         XC    PERKEY,PERKEY                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F                                         
         MVC   PERKCPY,RCCOMPFL                                                 
         MVC   PERKCODE,PERCODE    PERSON CODE                                  
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         CLC   IOKEY((PERKCODE-PERKEY)+L'PERKCODE),SVKEY                        
         BNE   PERS40                                                           
*                                                                               
         LA    R4,IO                                                            
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         XC    FLAG,FLAG                                                        
*                                                                               
         USING PIDELD,R2                                                        
         LR    R2,R4                                                            
         MVI   ELCODE,PIDELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MSG,=CL10'GET PERS'                                              
         SR    R5,R5                                                            
         ICM   R5,3,PERRLEN                                                     
         GOTO1 DUMP,DMCB,IO,(R5)                                                
*                                                                               
PERS20   CLC   PIDNO,PERPID        IF NUMBERS ARE THE SAME SKIP                 
         BNE   PERS40                                                           
*                                                                               
         CLC   PERDA,DSKADR        DOUBLE CHECK FOR CORRECT RECORD              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PCODE,PERKCODE      PERSON CODE TO PRINT LINE                    
         SR    R1,R1                                                            
         ICM   R1,3,PIDNO                                                       
         EDIT  (R1),PPEROPID                                                    
*                                                                               
PERS30   MVC   MSG,=CL10'GET PERS'                                              
         SR    R5,R5                                                            
         ICM   R5,3,PERRLEN                                                     
         GOTO1 DUMP,DMCB,IO,(R5)                                                
*                                                                               
         BAS   RE,DELELM                                                        
         AP    CHGCNT,=P'1'                                                     
*                                                                               
         SR    R1,R1                                                            
         EDIT  (R1),PPERNPID                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   MSG,=CL10'PUT PERS'                                              
         SR    R5,R5                                                            
         ICM   R5,3,PERRLEN                                                     
         GOTO1 DUMP,DMCB,IO,(R5)                                                
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   PERS40                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    PERS40                                                           
         GOTO1 =A(DMPUTREC),DMCB,(RC)                                           
*                                                                               
PERS40   LA    R3,PERLNQ(R3)                                                    
         BCT   R6,PERS10                                                        
*                                                                               
PERSX    DS    0H                                                               
         MVC   P+1(39),=C'TOTAL OF PERSON RECORDS CHANGED        : '            
         EDIT  (P8,CHGCNT),(10,P+42),ZERO=NOBLANK                               
         GOTO1 ACREPORT                                                         
         DROP  R2,R3,R4,R7                                                      
         EJECT                                                                  
***********************************************************************         
* READ FOR NEW PASSIVE POINTER (DELETE OLD ONE AND ADD NEW ONE)       *         
***********************************************************************         
         SPACE 1                                                                
PASSIVE  CLI   QOPT4,C'Y'          DO WE HAVE OVERRIDES                         
         BNE   EXIT                                                             
         MVI   QOPT7,C'Y'          FORCE PRINTABLES                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
*                                                                               
         ZAP   ADDCNT,=P'0'                                                     
         ZAP   DELCNT,=P'0'                                                     
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   MAXDUMP,=P'50'                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,PERTAB#                                                     
         BZ    PERSX                                                            
*                                                                               
         USING PERTABD,R3                                                       
         L     R3,APERTAB                                                       
*                                                                               
PASS10   CLI   QOPT3,C'Y'          IF Y DON'T CHANGE TEMPO RECORDS              
         BNE   *+12                                                             
         TM    PERSTAT,PERTEMPO                                                 
         BO    PASS20                                                           
*                                                                               
         LA    RE,IO               ALWAYS CLEAR CLEAR IO                        
         LA    RF,IOLNQ                                                         
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING PIDRECD,R4                                                       
         LA    R4,SVKEY                                                         
         XC    SVKEY,SVKEY         VALIDATE NOT USED                            
         MVC   PIDKEY,SPACES                                                    
         MVI   PIDKTYP,PIDKTYPQ    X'3E'                                        
         MVI   PIDKSUB,PIDKSUBQ    X'12'                                        
         MVC   PIDKCPY,RCCOMPFL                                                 
         MVC   PIDKPID,PERPID      PID FROM TABLE                               
         MVC   PIDKPER,PERCODE                                                  
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         CLC   SVKEY,IO                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PIDRECD,R4                                                       
         LA    R4,IOKEY                                                         
         CLC   PIDKDA,PERDA                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MSG,=CL10'GET PASS'                                              
         GOTO1 DUMP,DMCB,IO,L'PIDKEY+L'PIDKSTA+L'PIDKDA                         
*                                                                               
         OI    PIDKSTAT,X'80'      DELETE OLD RECORD AND ADD NEW                
         MVC   MSG,=CL10'DEL PASS'                                              
         GOTO1 DUMP,DMCB,IO,L'PIDKEY+L'PIDKSTA+L'PIDKDA                         
*                                                                               
         AP    DELCNT,=P'1'                                                     
         CLI   QOPT1,C'Y'                                                       
         BNE   PASS20                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    PASS20                                                           
         GOTO1 =A(DMWRTDR),DMCB,(RC)                                            
*                                                                               
PASS20   LA    R3,PERLNQ(R3)                                                    
         BCT   R0,PASS10                                                        
                                                                                
PASSX    DS    0H                                                               
         MVC   P+1(39),=C'TOTAL OF PASSIVE PONTERS DELETED       : '            
         EDIT  (P8,DELCNT),(10,P+42),ZERO=NOBLANK                               
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R3,R4,R7                                                         
         EJECT                                                                  
***********************************************************************         
* DELETE ELEMTENT                                                     *         
***********************************************************************         
         SPACE 1                                                                
DELELM   NTR1                                                                   
         LA    R4,IO                                                            
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'D8',(R4)),0,0,0                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
GETLEVS  NTR1                                                                   
         LA    R4,SVKEY            GET HEIRARCHY LEVELS                         
         MVC   SVKEY,SPACES                                                     
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),=C'1R'                                                
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   IOKEY(3),SVKEY                                                   
         BNE   GETLX                                                            
*                                                                               
         LA    R4,IO               SET R4 TO POINT TO THE RECORD                
         MVC   DSKADR,ACTKDA       SAVE OFF DISK ADDRESS FOR LATER PUT          
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         USING ACLELD,R2                                                        
         LR    R2,R4                                                            
         MVI   ELCODE,ACLELQ       X'16' - ACCOUNTS LENGTHS ELEMENT             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    LEVELS,LEVELS                                                    
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         EX    R1,*+4                                                           
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
         DROP  R2                                                               
*                                                                               
*              CONVERT 1,3,5,12 -> 1,2,2,7                                      
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GETL10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GETL20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GETL10                                                        
         B     GETLX                                                            
*                                                                               
GETL20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GETLX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
*     R4 = A(TIME RECORD)                                            *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMRECD,R4                                                       
SETCDE   NTR1                                                                   
         MVC   LEVSCDE(LVCDLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,TIMKACT          FULL ACCOUNT CODE                            
         LA    R2,LEVSCDE          FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,R2),0(R1)                                                    
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R2,L'LEVSCDE(R2)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GET SECURITY ID - READ CONTROL FILE CT5 RECORD                     *          
**********************************************************************          
         SPACE 1                                                                
         USING CT5REC,R4                                                        
GETSEC   NTR1                                                                   
         LA    R4,SVKEY1                                                        
         XC    SVKEY1,SVKEY1                                                    
         MVI   CT5KTYP,CT5KTYPQ    C'5' - SYSTEM ACCESS RECORDS                 
         MVC   CT5KALPH,ALPHAID                                                 
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY1(CT5LEN-CT5KEY),SVKEY1                                     
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
*                                                                               
         LA    R4,IO1                                                           
         LA    R2,CT5DATA                                                       
         MVC   SECALPHA,ALPHAID    DEFAULT TO ALPHA ID                          
GETS10   CLI   0(R2),0                                                          
         BE    GETSX                                                            
         CLI   0(R2),X'B8'         SECURITY ALPHA ID ELM                        
         BE    GETS20                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETS10                                                           
*                                                                               
         USING CTSEAD,R2                                                        
GETS20   MVC   SECALPHA,CTSEAAID   SECURITY ID                                  
*                                                                               
GETSX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* GET PID NUMBER                                                     *          
*     READ SECURITY SA0REC AND SAPEREC                               *          
**********************************************************************          
         SPACE 1                                                                
         USING SA0REC,R3                                                        
GETPID#  NTR1                                                                   
         LA    R3,SVKEY1                                                        
         XC    SVKEY1,SVKEY1                                                    
         MVI   SA0KTYP,SA0KTYPQ    C'0'-PERSONAL AUTHORIZATION RECORDS          
         MVC   SA0KAGY,SECALPHA    ALPHA/SECURITY ID                            
         MVC   SA0KNUM,SVPID                                                    
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY1(SA0LEN-SA0KEY),SVKEY1                                     
         BNE   GETPNO                                                           
*                                                                               
         LA    R3,IO1                                                           
         LA    R2,SA0DATA                                                       
GETP10   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'C3'         PERSON PERSONAL-ID                           
         BE    GETP20                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETP10                                                           
*                                                                               
         USING SAPALD,R2                                                        
GETP20   MVC   SVPERNAM,SAPALPID   PERSON ID NAME                               
         DROP  R2                                                               
*                                                                               
* CHECK SECURITY PERSON RECORD                                                  
*                                                                               
         USING SAPEREC,R3                                                       
         LA    R3,SVKEY1                                                        
         XC    SVKEY1,SVKEY1                                                    
         MVI   SAPETYP,SAPETYPQ    C'F' - PERSON RECORD                         
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECALPHA    AGENCY ALPHA/SECURITY ID                     
         MVC   SAPEPID,SVPERNAM    PERSON ID                                    
*                                                                               
         GOTO1 =A(DMCTFIL),DMCB,(RC)   READ HIGH                                
         CLC   IOKEY1(SAPEDEF-SAPEKEY),SVKEY1                                   
         BNE   GETPNO              IF NO SEC SIDE PUT IN TABLE                  
*                                                                               
GETPYES  SR    RC,RC                                                            
GETPNO   LTR   RC,RC                                                            
GETPX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         CP    DUMPCNT,MAXDUMP                                                  
         BH    DUMPX                                                            
         AP    DUMPCNT,=P'1'                                                    
*                                                                               
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOOP THROUGH TIME RECORDS AND MARK PID TABLE IF T/S IS TMS OR TEMPO *         
***********************************************************************         
         SPACE 1                                                                
LOOKTMS  NTR1                                                                   
         BAS   RE,GETLEVS          GET LEVS OF LEDGER                           
*                                                                               
         USING TSWRECD,R4                                                       
         LA    R4,SVKEY                                                         
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,RCCOMPFL                                                 
         MVC   TSWKPER,QAPPL       PERSON CODE - IF ANY                         
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         B     LOOKT20                                                          
*                                                                               
LOOKT10  GOTO1 =A(DMSEQDR),DMCB,(RC)    READ SEQUENTIAL                         
LOOKT20  LA    R1,3                                                             
         CLC   QAPPL,SPACES                                                     
         BNH   *+8                                                              
         LA    R1,11                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),SVKEY                                                   
         BNE   LOOKTX                                                           
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         USING TIMRECD,R4                                                       
         LA    R4,IO                                                            
*                                                                               
         BAS   RE,SETCDE                                                        
*                                                                               
         USING PIDELD,R2                                                        
         LR    R2,R4                                                            
         MVI   ELCODE,PIDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   LOOKT10                                                          
*                                                                               
         USING PERTABD,R6                                                       
         L     R6,APERTAB                                                       
         SR    R0,R0                                                            
         ICM   R0,3,PERTAB#                                                     
         BZ    LOOKTX                                                           
LOOKT30  CLC   PERCODE,LEVDCDE     PERSON CODE                                  
         BE    LOOKT50                                                          
LOOKT40  LA    R6,PERLNQ(R6)                                                    
         BCT   R0,LOOKT30                                                       
         B     LOOKT10             NOT IN TABLE                                 
*                                                                               
LOOKT50  CLC   PIDNO,PERPID        MAKE SURE THE NUMBERS ARE THE SAME           
         BNE   LOOKT40                  IF NOT LOOP THRU PERSON TABLE           
*                                                                               
         LR    R2,R4                                                            
         MVI   ELCODE,TIMELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                IT MUST HAVE AN 8B ELEMENT                   
*                                                                               
         USING TIMELD,R2                                                        
LOOKT60  CLI   0(R2),0             AT THE END OF THE RECORD?                    
         BE    LOOKT10                                                          
         CLI   0(R2),TIMELQ        ARE WE AT A X'8B'                            
         BNE   LOOKT70                                                          
         TM    TIMSTAT,TIMTEMPO    IS THE LINE A TEMPO LINE?                    
         BNO   LOOKT70                                                          
         OI    PERSTAT,PERTEMPO    PERSON HAS TEMPO TIME                        
*                                                                               
LOOKT70  SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     LOOKT60                                                          
*                                                                               
LOOKTX   B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R2,DISP2,ELCODE                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
APERTAB  DC    A(PERTAB)                                                        
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
HEXO     DC    V(HEXOUT)                                                        
VSECRET  DC    V(SECRET)                                                        
*                                                                               
ACCMST   DC    CL8'ACCMST  '                                                    
*                                                                               
DISP2    DC    H'56'                                                            
DMPSW    DC    C'N'                                                             
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'10'                                                          
FIXRECS  DC    PL4'0'                                                           
FIXELS   DC    PL4'0'                                                           
DRNODB   DC    PL4'0'                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
PERTAB   DS    XL11250                                                          
PERTBLNQ EQU   (*-PERTAB)/PERLNQ                                                
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMCTFIL  NMOD1 0,CTF               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',SVKEY1,IOKEY1                
         B     DMX                                                              
*                                                                               
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMADDDR  NMOD1 0,ADD               ADD KEY TO DIR                               
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   DSKADR,ACCKDA       SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',DSKADR,IO,DMWORK                  
         B     DMX                                                              
         DROP  R3                                                               
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',DSKADR,IO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PERSON/PID # TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
PERTABD  DSECT                                                                  
PERCODE  DS    CL8                                                              
PERPID   DS    XL2                                                              
PERSTAT  DS    XL1                 PERSON TABLE STATUS BYTE                     
PERTEMPO EQU   X'80'               T/S IS A TEMPO TIMESHEET                     
PERDA    DS    XL4                 DISK ADDRESS FOR PASSIVE POINTER             
PERLNQ   EQU   *-PERTABD                                                        
         EJECT                                                                  
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PL       DS    0C                                                               
         DS    CL5                                                              
PCODE    DS    CL8                 PERSON CODE                                  
         DS    CL5                 TEMPO XREF REPORT                            
PTEMODS  DS    CL8                     OFFICE-DEPT-SUBDEPT                      
         DS    CL5                                                              
PTEMEND  DS    CL6                     TIMESHEET END DATE                       
         DS    CL5                                                              
PTEMOPID DS    CL5                     OLD PID NUMBER                           
         DS    CL5                                                              
PTEMNPID DS    CL5                     NEW PID NUMBER                           
         DS    CL5                                                              
         ORG   PTEMODS             TIME RECORD                                  
PTIMACC  DS    CL14                    ACCOUNT CODE                             
         DS    CL5                                                              
PTIMOFF  DS    CL2                     OFFICE                                   
         DS    CL5                                                              
PTIMCAC  DS    CL14                    CONTRA ACCOUNT CODE                      
         DS    CL5                                                              
PTIMEND  DS    CL6                     TIMESHEET END DATE                       
         DS    CL5                                                              
PTIMOPID DS    CL5                     OLD PID NUMBER                           
         DS    CL5                                                              
PTIMNPID DS    CL5                     NEW PID NUMBER                           
         DS    CL5                                                              
         ORG   PTEMODS             PERSON RECORD                                
PPEROPID DS    CL5                     OLD PID NUMBER                           
         DS    CL5                                                              
PPERNPID DS    CL5                     NEW PID NUMBER                           
         DS    CL5                                                              
PLNQ     EQU   *-PL                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACX4D    DSECT                                                                  
DSKADR   DS    F                                                                
ASECBLK  DS    A                   SECRET BLOCK                                 
MSG      DS    CL10                                                             
ELCODE   DS    CL1                                                              
ELEM     DS    CL256                                                            
SVKEY    DS    CL42                                                             
SVKEY1   DS    CL42                                                             
CHGCNT   DS    PL8                                                              
ADDCNT   DS    PL8                                                              
DELCNT   DS    PL8                                                              
*                                                                               
FLAG     DS    XL1                                                              
FLGNEW   EQU   X'80'                                                            
*                                                                               
SVPID    DS    XL2                 SAVE AREA FOR PID NUMBER                     
SVPERCDE DS    CL8                 SAVE AREA FOR PERSON CODE                    
SVPERNAM DS    CL8                 PERSONAL ID NAME                             
*                                                                               
PERTAB#  DS    XL2                                                              
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LVCDLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
SECALPHA DS    CL2                 SECURITY ALPHA ID                            
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IOAREA   DS    CL2000                                                           
IOLNQ    EQU   *-IO                                                             
*                                                                               
IO1      DS    0CL2042                                                          
IOKEY1   DS    CL42                                                             
IOAREA1  DS    CL2000                                                           
IOLNQ1   EQU   *-IO1                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081ACREPX402A05/01/02'                                      
         END                                                                    
