*          DATA SET DDMITMCTL  AT LEVEL 043 AS OF 07/31/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DDMITMA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE SCANNER                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DDWTO                                                                  
         TITLE 'MITM Controller'                                                
         PRINT NOGEN                                                            
MITMCTL  CSECT                                                                  
         NBASE 0,*MITMCT*,A(WORKAREA),RA,R9                                     
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         SAM31                                                                  
*                                                                               
         STM   R9,RB,MITMBASE                                                   
*                                                                               
         BRAS  RE,INIT                                                          
*                                                                               
         MVC   RETCODE,=F'16'                                                   
         BRAS  RE,CARDREAD         READ AND VALIDATE INPUT CARDS                
         JNE   XBASE                                                            
*                                                                               
         BRAS  RE,SETOPS           SET OPERATOR COMMS                           
*                                                                               
         BRAS  RE,GETSTOR          ACQUIRE STORAGE FOR SUBTASKS                 
*                                                                               
         MVC   RETCODE,=F'8'                                                    
         BRAS  RE,ATTSRVS          ATTACH SERVERS                               
         JNE   XBASE                                                            
*                                                                               
         XC    RETCODE,RETCODE                                                  
         BRAS  RE,MAIN             WAIT FOR OPERATOR                            
*                                                                               
XBASE    DS    0H                                                               
         CLOSE SYSPRINT                                                         
*                                                                               
         XBASE RC=RETCODE                                                       
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         OPEN  (SYSPRINT,OUTPUT)                                                
         MVI   PCC,C' '                                                         
         MVC   PLINE,SPACES                                                     
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ACQUIRE STORAGE FOR SUBTASKS                                        *         
***********************************************************************         
*                                                                               
GETSTOR  NTR1                                                                   
*                                                                               
*        FIRST LOOK FOR ACTIVE SES AND INITIALISE BLOCK FOR EACH                
*                                                                               
         LARL  R3,ATTBLK                                                        
         USING ATTBLKD,R3                                                       
         SR    R2,R2                                                            
ATTS02   CLC   =X'FFFFFFFFFFFFFFFF',0(R3)  END OF LIST?                         
         JE    ATTS08              YES                                          
*                                                                               
         CLI   ATTBSE#,0           MENTIONED ON SE CARD?                        
         JE    ATTS06              NO, NEXT                                     
         LA    R2,1(,R2)           COUNT                                        
*                                                                               
         L     R0,=A(ATTDLQ)       ACQUIRE TASK BLOCK                           
         STORAGE OBTAIN,LENGTH=(R0),BNDRY=PAGE                                  
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                DIE FOR NOW IF NOT ENOUGH STORAGE            
         LR    R4,R1               R1 = A(OBTAINED STORAGE)                     
*                                                                               
         ST    R4,ATTBPTR          SAVE ADDRESS OF BLOCK                        
         USING ATTD,R4                                                          
         XC    ATTD(ATTDL1Q),ATTD  CLEAR COMMON PART                            
         MVC   ATTDSPCE,DSPACE                                                  
         MVC   ATTALET,DSPALET                                                  
         MVC   ATTPSCAN,=A(SCANNED_PARAMETERS)  A(SCANNER BLOCK)                
         MVC   ATTSE,ATTBSE#                                                    
         LA    R0,ATTSE                                                         
         O     R0,=X'80000000'     (IN CASE THIS IS EVER IN HIGHCORE)           
         GOTO1 =V(HEXOUT),DMCB,(R0),ATTSEC,1,0                                  
         CLC   =F'2',DMCB+16                                                    
         JE    *+6                                                              
         DC    H'0'                BAD CALL TO HEXOUT                           
         MVC   ATTEYE,=CL16'**ATTBLK*SE#XX**'                                   
         MVC   ATTEYE+12(2),ATTSEC                                              
         XC    ATTSYSN,ATTSYSN     GET SYSTEM NAME FROM DSPACE                  
         STAR  LABEL=Y,CLEAR=Y,ARS=ON                                           
         LAM   AR5,AR5,DSPALET                                                  
         SR    R5,R5                                                            
         L     R1,0(,R5)           A(ECB TABLE) = UPPER LIMIT                   
         ICM   R5,1,ATTSE                                                       
         MHI   R5,L'DSPHDR         SE# * 64 (L'RESOURCE HEADER)                 
         CR    R5,R1               ENSURE NOT PASSED UPPER LIMIT                
         JNL   *+10                                                             
         MVC   ATTSYSN,DSPNAME-DMSPACED(R5)                                     
         REAR  ARS=OFF                                                          
         CLI   ATTSYSN,C' '        DID WE FIND A SYSTEM NAME?                   
         JH    *+16                                                             
         MVC   ATTSYSN(3),=C'SE:'  NO, SET A DEFAULT                            
         MVC   ATTSYSN+3(2),ATTSEC                                              
         MVC   ATTTRACE,TRACE                                                   
*                                  !!!! fix this length !!!                     
         L     R0,=A(ATTDLQ)       ACQUIRE 31-BIT TASK BLOCK                    
         STORAGE OBTAIN,LENGTH=(R0),LOC=ANY,BNDRY=PAGE                          
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                DIE FOR NOW IF NOT ENOUGH STORAGE            
         LR    R5,R1               R1 = A(OBTAINED STORAGE)                     
*                                                                               
         ST    R5,ATT31STO         SAVE A(ACQUIRED STORAGE)                     
*                                                                               
         USING ATT31_BLKD,R5                                                    
         MVC   ATT31EYE,=CL16'**ATT31**SE#XX**'                                 
         MVC   ATT31EYE+12(2),ATTSEC                                            
         MVC   ATT31TABLES_EYE,=CL16'**TABS***SE#XX**'                          
         MVC   ATT31TABLES_EYE+12(2),ATTSEC                                     
         DROP  R4                                                               
*                                                                               
         LA    RF,ATT31TABLES      START OF TABLE AREA                          
         MVC   CPYTABH_EYE-CPYTABHD(,RF),=CL16'** ACC COMPANY *'                
         ST    RF,ATT31_ACC_COMPANY_TABLE  SAVE A(ACC COMPANY TABLE)            
         DROP  R5                                                               
*                                                                               
ATTS06   LA    R3,ATTBLKLQ(,R3)    BUMP TO NEXT SUBTASK                         
         J     ATTS02                                                           
         DROP  R3                                                               
*                                                                               
ATTS08   LTR   R2,R2                                                            
         JNZ   ATTS10                                                           
         MVC   P(22),=C'No Systems to process!'                                 
         BRAS  RE,PRNT                                                          
         J     EXITL                                                            
*                                                                               
ATTS10   DS    0H                                                               
         J     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE WITH SYSTEM-SPECIFIC DATA                                *         
***********************************************************************         
*                                                                               
INITSYS  NTR1                                                                   
         USING ATTD,R4                                                          
*                                                                               
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),ATTSE       SET SYSTEM NUMBER IN UTL                     
*                                                                               
         CLI   MAJORSYS,X'0A'      IS THIS CONTROL                              
         JE    INITSYSA                                                         
*                                                                               
         CLI   MAJORSYS,X'06'      IS THIS AN ACC INSTANCE?                     
         JNE   INITSYSX            NO: NOTHING TO DO (YET)                      
*                                                                               
INITSYS6 LOAD  EPLOC=INITMOD                                                    
         ST    R0,AINITMOD         SAVE A(LOADED MODULE)                        
*                                                                               
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),ATTSE       SET SYSTEM NUMBER IN UTL                     
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'ACC',ACFLIST                  
*                                                                               
         LA    R1,DMCB             BUILD PARAMETER LIST                         
         USING ATTINITD,R1                                                      
         MVC   ATTINIT_VDATAMGR,=V(DATAMGR)                                     
         ST    R3,ATTINIT_ATTBLK                                                
         DROP  R1                                                               
         L     RF,AINITMOD         A(LOADED MODULE)                             
         BASR  RE,RF               CALL INITIALIZATION MODULE                   
         DROP  R4                                                               
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMCLSE'),=C'ACC',0                        
*                                                                               
         DELETE EPLOC=INITMOD                                                   
         J      INITSYSX                                                        
                                                                                
***********************************************************************         
* CONTROL SYSTEM INITIALISATION                                       *         
***********************************************************************         
         USING ATTD,R4                                                          
INITSYSA MVC   ATTADMGR,=V(DATAMGR)   DMGR STRICTLY FOR CONTROL ONLY            
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CON',CTFLIST                  
*                                                                               
INITSYSX DS    0H                                                               
         J     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ATTACH SERVERS (ONE SUBTASK PER ACTIVE SE)                          *         
***********************************************************************         
*                                                                               
ATTSRVS  NTR1                                                                   
*                                                                               
         LARL  R3,ATTBLK                                                        
         USING ATTBLKD,R3                                                       
ATTS12   CLC   =X'FFFFFFFFFFFFFFFF',0(R3)  END OF LIST?                         
         JE    EXITOK                                                           
*                                                                               
         CLI   ATTBSE#,0           MENTIONED ON SE CARD?                        
         JE    ATTS16              NO, NEXT                                     
         L     R4,ATTBPTR          GET ADDRESS OF BLOCK                         
         BRAS  RE,ATTACH                                                        
*                                                                               
ATTS16   DS    0H                                                               
         LA    R3,ATTBLKLQ(,R3)    BUMP TO NEXT SUBTASK                         
         J     ATTS12                                                           
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SUBROUTINE TO ATTACH A TASK. R4=A(ATTD)                                
***********************************************************************         
*                                                                               
ATTACH   NTR1                                                                   
*                                                                               
         USING ATTD,R4                                                          
*                                                                               
         BRAS  RE,INITSYS          PERFORM SE-SPECIFIC INITIALIZATION           
*                                                                               
         MVC   P(10),=C'Attaching:'                                             
         CLI   ATTRSTRT,0          RESTART?                                     
         JE    *+10                                                             
         MVC   P(11),=C'Restarting:'                                            
         MVC   P+12(L'MITMSRV),MITMSRV                                          
         MVC   P+22(3),=C'SE:'                                                  
         MVC   P+25(2),ATTSEC                                                   
         MVC   P+28(7),ATTSYSN                                                  
         BRAS  RE,PRNT                                                          
*                                                                               
         MVI   ATTFLAGS,ATTFGO+ATTFINI                                          
         XC    ATTECBO,ATTECBO     ECB (MAIN TO SUBTASK)                        
         LA    R5,ATTECBI          ECB (SUBTASK TO MAIN)                        
         LARL  R2,ATTESTAI         A(ESTAI ROUTINE)                             
*                                  R3 = A(ATTBLK ENTRY). THIS IS THE            
*                                   PARAMETER TO THE SUBTASK, AND ALSO          
*                                   TO THE ESTAI ROUTINE.                       
         ATTACHX EPLOC=MITMSRV,MF=(E,(R3)),SZERO=NO,ALCOPY=YES,        +        
               ECB=(R5),ESTAI=((R2),(R3))                                       
         ST    R1,ATTTCB                                                        
         J     EXITOK                                                           
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMS                                               *         
***********************************************************************         
*                                                                               
SETOPS   NTR1                                                                   
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         LTR   R2,R2               CIB ISN'T SET...                             
         JZ    SETOP2              ...UNLESS IT'S A STARTED TASK                
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         JNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN PROCESS                                                        *         
***********************************************************************         
*                                                                               
MAIN     NTR1                                                                   
*                                                                               
MAIN02   CLI   TRACE,C'N'                                                       
         JE    MAIN03                                                           
         MVC   P(25),=C'Main task going into wait'                              
         BRAS  RE,PRNT                                                          
*                                                                               
MAIN03   DS    0H                                                               
         LARL  R3,ATTBLK           BUILD ECB LIST                               
         USING ATTBLKD,R3                                                       
         USING ATTD,R4                                                          
         LARL  R5,ECBLST                                                        
MAIN04   CLC   =X'FFFFFFFFFFFFFFFF',0(R3)                                       
         JE    MAIN08                                                           
         CLI   ATTBSE#,0           MENTIONED ON SE CARD?                        
         JE    MAIN06              NO, NEXT                                     
         L     R4,ATTBPTR          GET ADDRESS OF BLOCK                         
         TM    ATTFLAGS,ATTFGO                                                  
         JZ    MAIN06                                                           
         TM    ATTECBI,POSTED      HAS THIS TASK POSTED ALREADY?                
         JO    MAIN10              YES, NO NEED TO WAIT                         
         LA    RE,ATTECBI          SUBTASK ECB                                  
         ST    RE,0(,R5)                                                        
         LA    R5,4(,R5)                                                        
*                                                                               
MAIN06   DS    0H                                                               
         LA    R3,ATTBLKLQ(,R3)    BUMP TO NEXT SUBTASK                         
         J     MAIN04                                                           
         DROP  R4                                                               
         DROP  R3                                                               
*                                                                               
MAIN08   DS    0H                                                               
         MVC   0(4,R5),AOPERECB    OPERATOR ECB                                 
         L     R1,=A(10*60*100)    SET 10 MINUTE WAIT SO DON'T WAIT TOO         
         ST    R1,TIMERINT         LONG (ELSE MVS TERMINATES US).               
         BRAS  RE,SETWAIT                                                       
         LA    R1,TIMERECB                                                      
         ST    R1,4(,R5)                                                        
         OI    4(R5),X'80'         THAT WAS LAST ECB                            
*                                                                               
         LARL  R1,ECBLST                                                        
         WAIT  ECBLIST=(1),LONG=YES                                             
*                                                                               
MAIN10   DS    0H                                                               
         MVI   OPERSTOP,C'N'                                                    
         L     R1,AOPERECB                                                      
         TM    0(R1),POSTED        DID THE OPERATOR INTERRUPT?                  
         JZ    MAIN12              NO, SKIP                                     
         BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
         JE    MAIN12              CC EQUAL IF DONE (NOT STOP COMMAND)          
         MVI   OPERSTOP,C'Y'       INDICATE STOP REQUESTED                      
*                                                                               
MAIN12   DS    0H                                                               
         LARL  R3,ATTBLK           LOOK FOR (AB)ENDED SUBTASK(S)                
         USING ATTBLKD,R3                                                       
         USING ATTD,R4                                                          
MAIN14   CLC   =X'FFFFFFFFFFFFFFFF',0(R3)                                       
         JE    MAIN22                                                           
         CLI   ATTBSE#,0           MENTIONED ON SE CARD?                        
         JE    MAIN18              NO, NEXT                                     
         L     R4,ATTBPTR          GET ADDRESS OF BLOCK                         
         TM    ATTECBI,POSTED      THIS SUBTASK POSTED?                         
         JZ    MAIN18                                                           
*                                                                               
         XC    ATTECBI,ATTECBI                                                  
         DETACH ATTTCB             REMOVE TASK                                  
         XC    ATTTCB,ATTTCB                                                    
         BRAS  RE,STATUS           PRINT STATUS                                 
         TM    ATTFLAGS,ATTFABND   HAS IT ABENDED?                              
         JZ    MAIN16              NO                                           
*                                                                               
         MVC   P(3),=C'SE:'                                                     
         MVC   P+3(2),ATTSEC                                                    
         MVC   P+6(7),ATTSYSN                                                   
         MVC   P+14(8),=C'Abended.'                                             
         TM    ATTFLAGS,ATTFSEND                                                
         JZ    *+10                                                             
         MVC   P+23(33),=C'WARNING. Data SEND still pending!'                   
         BRAS  RE,PRNT                                                          
         LH    R1,ATTABNCT         NUMBER OF ABENDS FOR THIS SE                 
         LA    R1,1(,R1)                                                        
         STH   R1,ATTABNCT                                                      
         CLI   SUBABEND,C'E'       EOJ ON ANY ABEND?                            
         JE    MAIN34              YES, JUST EXIT                               
*                                                                               
         XC    ATTTCB,ATTTCB                                                    
         MVI   ATTFLAGS,0                                                       
         C     R1,MAXABEND         ABEND LIMIT EXCEEDED?                        
         JNH   MAIN20                                                           
         MVC   P(3),=C'SE:'                                                     
         MVC   P+3(2),ATTSEC                                                    
         MVC   P+6(7),ATTSYSN                                                   
         MVC   P+14(40),=C'Maximum ABENDs exceeded for this system.'            
         BRAS  RE,PRNT                                                          
         J     MAIN18                                                           
*                                                                               
MAIN16   DS    0H                                                               
         MVC   P(3),=C'SE:'                                                     
         MVC   P+3(2),ATTSEC                                                    
         MVC   P+6(7),ATTSYSN                                                   
         MVC   P+14(20),=C'Stopped by Operator.'                                
         TM    ATTFLAGS,ATTFCLSE   WAS IT CLOSED                                
         JO    *+10                                                             
         MVC   P+14(20),=C'Stopped : IP errors.'                                
         TM    ATTFLAGS,ATTFSEND                                                
         JZ    *+10                                                             
         MVC   P+35(22),=C'WARNING. Send Pending!'                              
         MVC   WORK(80),P                                                       
         BRAS  RE,PRNT                                                          
         LA    R1,WORK+79          ALWAYS WTO. WAS OPERATOR REQUEST.            
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   1(2,R1),=C'//'                                                   
         GOTO1 =V(DDWTO),DMCB,WORK,0                                            
         NI    ATTFLAGS,255-ATTFCLSE                                            
*                                                                               
MAIN18   DS    0H                                                               
         LA    R3,ATTBLKLQ(,R3)    BUMP TO NEXT SUBTASK                         
         J     MAIN14                                                           
         DROP  R3                                                               
*                                                                               
MAIN20   DS    0H                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR STOP REQUESTED?                     
         JE    MAIN12              YES, DON'T RESTART                           
         MVI   ATTFLAGS,ATTFGO     RESET                                        
         MVI   ATTRSTRT,2          INDICATE RESTART AFTER FAIL                  
         BRAS  RE,ATTACH           TRY TO RESTART TASK                          
         J     MAIN12              BACK TO BEGINNING SO GET ALL                 
         DROP  R4                                                               
*                                                                               
MAIN22   DS    0H                                                               
         CLI   OPERSTOP,C'Y'       OPERATOR STOP REQUESTED?                     
         JNE   MAIN02              NO, GO BACK FOR ANOTHER WAIT                 
*                                                                               
         MVC   P(23),=C'Operator requested Stop'                                
         BRAS  RE,PRNT                                                          
*                                                                               
MAIN24   DS    0H                                                               
         LARL  R3,ATTBLK           ALL SUBTASKS ENDED?                          
         USING ATTBLKD,R3                                                       
         SR    R5,R5                                                            
         USING ATTD,R4                                                          
MAIN26   CLC   =X'FFFFFFFFFFFFFFFF',0(R3)                                       
         JE    MAIN32                                                           
         CLI   ATTBSE#,0           MENTIONED ON SE CARD?                        
         JE    MAIN30              NO, NEXT                                     
         L     R4,ATTBPTR          GET ADDRESS OF BLOCK                         
         OC    ATTTCB,ATTTCB       SUBTASK ACTIVE?                              
         JZ    MAIN30              NO, NEXT                                     
*                                                                               
         TM    ATTFLAGS,ATTFABND   HAS IT ABENDED                               
         JO    MAIN28              YES, MAKE SURE IT DETACHED                   
         TM    ATTFLAGS,ATTFGO     OK TO DETACH?                                
         JZ    MAIN28              YES, DO IT                                   
         AHI   R5,1                COUNT STILL WAITING                          
         TM    ATTFLAGS,ATTFCLSE   HAVE WE TOLD IT TO CLOSE YET?                
         JO    MAIN30              YES, SKIP                                    
         POST  ATTECBO             SUBTASK WILL SHUT DOWN                       
         OI    ATTFLAGS,ATTFCLSE                                                
         J     MAIN30                                                           
*                                                                               
MAIN28   DS    0H                                                               
         DETACH ATTTCB             REMOVE TASK                                  
         XC    ATTTCB,ATTTCB                                                    
         NI    ATTFLAGS,255-ATTFCLSE                                            
         BRAS  RE,STATUS           PRINT STATUS                                 
*                                                                               
MAIN30   DS    0H                                                               
         LA    R3,ATTBLKLQ(,R3)    BUMP TO NEXT SUBTASK                         
         J     MAIN26                                                           
         DROP  R4                                                               
         DROP  R3                                                               
*                                                                               
MAIN32   DS    0H                                                               
         LTR   R5,R5               R5=0 IF ALL ACTIVE SUBTASKS ENDED            
         JZ    MAIN34                                                           
         MVC   P(33),=C'Waiting for NNN subtask(s) to end'                      
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+12(3),DUB+6(2)                                                 
         BRAS  RE,PRNT                                                          
         LA    R1,2*100            SET 2 SECOND WAIT                            
         ST    R1,TIMERINT                                                      
         BRAS  RE,SETWAIT                                                       
         LA    R1,TIMERECB         TIMER ECB                                    
         WAIT  ECB=(1)                                                          
         J     MAIN24                                                           
*                                                                               
MAIN34   DS    0H                                                               
         MVC   P(6),=C'Ending'                                                  
         BRAS  RE,PRNT                                                          
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK OPERATOR INTERRUPT                                            *         
***********************************************************************         
*                                                                               
CHKOPER  NTR1                                                                   
*                                                                               
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         JE    EXITL               YES, EXIT LOW                                
*                                                                               
         CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         JE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1               GET DATA LEN IN R2                           
         ICM   R1,3,CIBDATLN                                                    
         JNZ   CHEKO02                                                          
         MVC   P(22),=C'Null operator command?'                                 
         BRAS  RE,PRNT                                                          
         J     CHEKRSET            CC EQ IF VALID AND PROCESSED                 
*                                                                               
CHEKO02  LR    R3,R1                                                            
         MVC   CARD,SPACES         MOVE DATA TO CARD AND VALIDATE               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   CARD(0),CIBDATA                                                  
         MVC   P(17),=C'Operator command:'                                      
         MVC   P+18(L'CARD),CARD                                                
         BRAS  RE,PRNT                                                          
*                                                                               
         BRAS  RE,PROCOPER         VALIDATE OPERATOR INPUT                      
         DROP  R2                                                               
*                                                                               
CHEKRSET L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         JZ    EXITOK                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  RF                                                               
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS OPERATOR INPUT IN CARD (LENGTH IN R3)            *         
***********************************************************************         
*                                                                               
PROCOPER NTR1                                                                   
*                                                                               
         OC    CARD,SPACES         MUST BE UPPER CASE                           
         STC   R3,CARDHDR+5        FAKE UP A FIELD HEADER                       
*                                                                               
         LARL  RF,SCNBLK                                                        
         GOTO1 =V(SCANNER),DMCB,('SCNRGHTQ',CARDHDR),(5,(RF)),0                 
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         JZ    POINVLIN            INVALID IF NO ENTRY                          
*                                                                               
         LARL  R2,SCNBLK           FIRST ENTRY IS COMMAND NAME                  
         USING SCANBLKD,R2                                                      
         CLI   SC2NDLEN,0                                                       
         JNE   POINVLIN            INVALID IF COMMAND IN A=B FORMAT             
         MVC   WORK(60),SC1STFLD                                                
*                                                                               
         LA    R4,COMMTAB          FIND COMMAND IN TABLE                        
         USING COMMTABD,R4                                                      
         SR    RF,RF                                                            
PROCO04  CLI   ONAME,COMMEOT       END OF TABLE                                 
         JE    POINVKEY            INVALID KEYWORD                              
         CLC   SC1STFLD,ONAME                                                   
         JE    *+12                                                             
         LA    R4,COMMTABL(,R4)                                                 
         J     PROCO04                                                          
         ICM   RF,15,ORTN                                                       
         BR    RF                                                               
         DROP  R4                                                               
*                                                                               
POSTART  MVI   OPERCOMM,C'S'                                                    
         CHI   R0,2                MUST BE TWO PARMS                            
         JNE   POINVLIN                                                         
         J     PROCO10                                                          
*                                                                               
POSTOP   MVI   OPERCOMM,C'P'                                                    
         CHI   R0,2                MUST BE TWO PARMS                            
         JNE   POINVLIN                                                         
         J     PROCO10                                                          
*                                                                               
POSTAT   MVI   OPERCOMM,C'U'                                                    
         CHI   R0,2                MUST BE TWO PARMS                            
         JNE   POINVLIN                                                         
         J     PROCO10                                                          
*                                                                               
POTRACE  MVI   OPERCOMM,C'T'                                                    
         MVI   OPEROPT,C'Y'        DEFAULT TRACE=Y                              
         CHI   R0,2                MUST BE AT LEAST TWO PARMS                   
         JL    POINVLIN                                                         
         CHI   R0,3                MAY BE 3 PARMS                               
         JH    POINVLIN                                                         
         JL    PROCO10                                                          
         LR    RF,R2               SAVE R2                                      
         LA    R2,2*(SCNENTLQ)(,R2) THIRD PARM                                  
         CLI   SC2NDLEN,0                                                       
         JNE   POINVLIN            INVALID IF A=B FORMAT                        
         CLI   SC1STLEN,0          LEN MUST NOT BE ZERO                         
         JE    POINVLIN                                                         
*                                                                               
         MVC   WORK(60),SC1STFLD                                                
         CLC   =C'ON ',SC1STFLD                                                 
         JE    POTRACE2                                                         
         CLC   =C'YES ',SC1STFLD                                                
         JE    POTRACE2                                                         
         CLC   =C'Y ',SC1STFLD                                                  
         JE    POTRACE2                                                         
         MVI   OPEROPT,C'N'        TRACE=N                                      
         CLC   =C'OFF ',SC1STFLD                                                
         JE    POTRACE2                                                         
         CLC   =C'NO ',SC1STFLD                                                 
         JE    POTRACE2                                                         
         CLC   =C'N ',SC1STFLD                                                  
         JE    POTRACE2                                                         
         MVI   OPEROPT,C'D'        TRACE=D                                      
         CLC   =C'DETAIL ',SC1STFLD                                             
         JE    POTRACE2                                                         
         CLC   =C'D ',SC1STFLD                                                  
         JNE   POINVTRC                                                         
*                                                                               
POTRACE2 LR    R2,RF               RESTORE R2                                   
PROCO10  LA    R2,SCNENTLQ(,R2)    SECOND PARM IS SE OR SYSTEM                  
         CLI   SC2NDLEN,0                                                       
         JNE   POINVLIN            INVALID IF A=B FORMAT                        
         CLI   SC1STLEN,0          LEN MUST NOT BE ZERO                         
         JE    POMISSE                                                          
         MVC   WORK(60),SC1STFLD                                                
*                                                                               
         CLI   SC1STLEN,3          POSIT 'ALL'                                  
         JNE   PROCO12                                                          
         CLC   =C'ALL',WORK                                                     
         JNE   PROCO12                                                          
         CLI   OPERCOMM,C'S'       'ALL' NOT ALLOWED ON START COMMAND           
         JE    POBADSE                                                          
         LARL  R3,ATTBLK                                                        
         USING ATTBLKD,R3                                                       
         MVI   OPERALL,C'Y'                                                     
         J     PROCO32                                                          
*                                                                               
PROCO12  MVI   OPERALL,C'N'        NOT 'ALL', MUST BE SE OR SYS                 
         CLI   SC1STLEN,2          LEN MUST BE 2 FOR SE                         
         JNE   PROCO14                                                          
         TM    SC1STVAL,SCHEXQ     AND FIELD MUST BE HEX                        
         JZ    PROCO14                                                          
         GOTO1 =V(HEXIN),DMCB,WORK,BYTE,2,0                                     
         OC    12(4,R1),12(R1)                                                  
         JZ    PROCO14             HEXIN NOT HAPPY!                             
         LLC   RF,BYTE                                                          
         LTR   RF,RF               RF IS SE NUMBER                              
         JZ    POBADSE             MUSTN'T BE ZERO                              
         BCTR  RF,0                                                             
         MHI   RF,ATTBLKLQ                                                      
         LARL  R3,ATTBLK                                                        
         AR    R3,RF               POINT TO A(ATT BLOCK) FOR THIS SE            
         OC    0(ATTBLKLQ,R3),0(R3)   CAN ONLY PROCESS KNOWN SE                 
         JNZ   PROCO20                                                          
         J     POWROSE                                                          
*                                                                               
PROCO14  CLI   SC1STLEN,7          MUST BE A SYSTEM NAME                        
         JH    POBADSE             TOO LONG                                     
         LARL  R3,ATTBLK                                                        
PROCO16  OC    0(ATTBLKLQ,R3),0(R3)   NOT A KNOWN SE                            
         JZ    PROCO18                                                          
         L     R4,ATTBPTR          GET ATT BLOCK FOR SE                         
         USING ATTD,R4                                                          
         CLC   ATTSYSN,WORK        IS THIS RIGHT SYSTEM?                        
         JE    PROCO22                                                          
         DROP  R4                                                               
*                                                                               
PROCO18  LA    R3,ATTBLKLQ(,R3)    ELSE TRY NEXT                                
         CLC   =X'FFFFFFFFFFFFFFFF',0(R3)                                       
         JNE   PROCO16                                                          
         J     POWROSE                                                          
*                                                                               
PROCO20  L     R4,ATTBPTR          GET ATT BLOCK FOR SE                         
         USING ATTD,R4                                                          
PROCO22  CLI   OPERCOMM,C'S'                                                    
         JE    POSTRT00                                                         
         CLI   OPERCOMM,C'P'                                                    
         JE    POSTOP00                                                         
         CLI   OPERCOMM,C'U'                                                    
         JE    POSTAT00                                                         
*                                                                               
         MVC   ATTTRACE,OPEROPT    SET TRACE FOR SE                             
         J     PROCO30                                                          
*                                                                               
POSTAT00 BRAS  RE,STATUS           DO STATUS FOR SE                             
         J     PROCO30                                                          
*                                                                               
POSTRT00 TM    ATTFLAGS,ATTFGO     TASK ALREADY ACTIVE?                         
         JO    POSEACT             YES, ERROR                                   
         MVI   ATTRSTRT,1          INDICATE RESTART BY COMMAND                  
         BRAS  RE,ATTACH           RE-ATTACH TASK                               
         J     PROCOX              CAN ONLY START ONE TASK                      
*                                                                               
POSTOP00 CLI   OPERALL,C'Y'        ALL SES?                                     
         JE    POSTOP02            YES, SKIP                                    
         TM    ATTFLAGS,ATTFGO     TASK ACTIVE?                                 
         JZ    POSENAC             NO, ERROR                                    
         TM    ATTFLAGS,ATTFCLSE   HAVE WE TOLD IT TO CLOSE YET?                
         JO    POSECIP             YES, ERROR                                   
         J     POSTOP04                                                         
*                                                                               
POSTOP02 TM    ATTFLAGS,ATTFGO     TASK ACTIVE?                                 
         JZ    PROCO30             NO, IGNORE                                   
         TM    ATTFLAGS,ATTFCLSE   HAVE WE TOLD IT TO CLOSE YET?                
         JO    PROCO30             YES, IGNORE                                  
*                                                                               
POSTOP04 OI    ATTFLAGS,ATTFCLSE   MAIN SHOULD DETACH IT                        
         POST  ATTECBO             SUBTASK WILL SHUT DOWN                       
*                                                                               
         DROP  R4                                                               
*                                                                               
PROCO30  CLI   OPERALL,C'Y'        ALL SES?                                     
         JNE   PROCOX              NO, ALL DONE                                 
         LA    R3,ATTBLKLQ(,R3)    ELSE DO NEXT                                 
         CLC   =X'FFFFFFFFFFFFFFFF',0(R3)                                       
         JE    PROCOX                                                           
*                                                                               
PROCO32  OC    0(ATTBLKLQ,R3),0(R3)   CAN ONLY PROCESS KNOWN SE'S               
         JZ    PROCO30                                                          
         J     PROCO20                                                          
         DROP  R3                                                               
*                                                                               
PROCOX   MVI   OPERCOMM,C' '                                                    
         J     EXITOK                                                           
*                                                                               
POINVLIN MVC   P+12(23),=C'Invalid command format.'                             
         J     POERR                                                            
POINVKEY MVC   P+12(16),=C'Invalid command:'                                    
         MVC   P+29(10),WORK                                                    
         J     POERR                                                            
POBADSE  MVC   P+12(15),=C'Invalid system:'                                     
         MVC   P+28(SCNRGHTQ),WORK                                              
         J     POERR                                                            
POMISSE  MVC   P+12(14),=C'Missing system'                                      
         J     POERR                                                            
POWROSE  MVC   P+12(20),=C'System not in parms.'                                
         J     POERR                                                            
POSEACT  MVC   P+12(22),=C'System already active.'                              
         J     POERR                                                            
POSENAC  MVC   P+12(23),=C'System already stopped.'                             
         J     POERR                                                            
POSECIP  MVC   P+12(23),=C'System already closing.'                             
         J     POERR                                                            
POINVTRC MVC   P+12(21),=C'TRACE option invalid:'                               
         MVC   P+34(SCNRGHTQ),WORK                                              
         J     POERR                                                            
*                                                                               
POERR    MVC   P(11),=CL11'***Error***'                                         
         MVC   WORK(80),P+12                                                    
         BRAS  RE,PRNT                                                          
*                                                                               
         LA    R1,WORK+79                                                       
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         MVC   1(2,R1),=C'//'                                                   
         GOTO1 =V(DDWTO),DMCB,WORK,0                                            
         J     EXITL                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* COMMANDS TABLE                                                      *         
***********************************************************************         
*                                                                               
* S procname (START the controller as a started task)                           
* P taskname (STOP the controller and all subtasks cleanly)                     
*                                                                               
* All F (Modify) commands are of the format                                     
*    F taskname,action,system(,optional parm)                                   
*      where "system" can be a hex SENUM or SENAME (some commands               
*       support system ALL)                                                     
*                                                                               
* F taskname,STOP,system (stop a single system, and detach its subtask)         
* F taskname,STOP,ALL                                                           
*                                                                               
* F taskname,START,system (start a single system, attach its subtask)           
*                                                                               
* F taskname,STATUS,system (print status info for a single subtask)             
* F taskname,STATUS,ALL                                                         
*                                                                               
* F taskname,TRACE,system,tracetype (change trace option for a system)          
* F taskname,TRACE,ALL,tracetype                                                
*  Tracetype can be:                                                            
*   ON, YES or Y to enable basic tracing.                                       
*   OFF, NO or N to disable tracing.                                            
*   DETAIL or D to enable more detailed tracing.                                
*                                                                               
COMMTAB  DS    0D                                                               
COMMSTAR DC    CL(L'SC1STFLD)'START     ',AL4(POSTART)                          
         DC    CL(L'SC1STFLD)'STOP      ',AL4(POSTOP)                           
         DC    CL(L'SC1STFLD)'STATUS    ',AL4(POSTAT)                           
         DC    CL(L'SC1STFLD)'TRACE     ',AL4(POTRACE)                          
COMMTABX DC    AL1(COMMEOT)                                                     
*                                                                               
***********************************************************************         
* COMMAND TABLE DSECT                                                 *         
***********************************************************************         
*                                                                               
COMMTABD DSECT                                                                  
ONAME    DS    CL(L'SC1STFLD)      INPUT COMMAND                                
ORTN     DS    AL4                 A(PROCESSOR)                                 
COMMTABL EQU   *-COMMTABD                                                       
*                                                                               
COMMEOT  EQU   X'FF'                                                            
*                                                                               
MITMCTL  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ IN THE INPUT CARDS AND VALIDATE THEM                *         
***********************************************************************         
*                                                                               
CARDREAD NTR1                                                                   
*                                                                               
         OPEN  (SYSIN)             (DDCARDS ISN'T 31-BIT COMPLIANT)             
*                                                                               
         LARL  R5,SCANNED_PARAMETERS                                            
         USING SCANPARMD,R5                                                     
         MVC   SCANPARMD_EYE,=CL16'**SCANNED PARMS*'                            
         XC    SCANPARMD_NUMENTS,SCANPARMD_NUMENTS                              
         MVI   SCANPARMD_LEFTLEN,L'SC1STFLD                                     
         MVI   SCANPARMD_RIGHTLEN,SCNRGHTQ                                      
*                                                                               
         LARL  R3,SCNCARDS         STORE SCANNED PARAMETER CARDS HERE           
*                                                                               
CRD02    DS    0H                                                               
         GET   SYSIN,CARD                                                       
*                                                                               
         MVC   P(11),=C'Validating:'                                            
         MVC   P+12(L'CARD),CARD                                                
         BRAS  RE,PRNT                                                          
*                                                                               
         CLI   CARD,C'*'           * IN COL 1 IS A COMMENT                      
         JE    CRD02                                                            
*                                                                               
         LA    R1,CARD+L'CARD-1    FIND DATA LENGTH                             
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         BRCT  R1,*-8                                                           
         LA    R0,CARD-1                                                        
         SR    R1,R0                                                            
         JNP   CEINVLIN            BLANK CARD                                   
         STC   R1,CARDHDR+5        FAKE UP A FIELD HEADER                       
*                                                                               
         LHI   R0,MAXSCAN                                                       
         SH    R0,SCANPARMD_NUMENTS   R0 = # OF REMAINING SCANNER SLOTS         
*                                                                               
         GOTO1 =V(SCANNER),DMCB,('SCNRGHTQ',CARDHDR),((R0),(R3))                
         SR    RF,RF                                                            
         ICM   RF,1,4(R1)                                                       
         JZ    CEINVLIN            INVALID LINE IF ZERO                         
*                                                                               
         LR    R2,RF                                                            
         BRAS  RE,CARDVAL          VALIDATE KEYWORD=VALUE                       
*                                                                               
         AH    RF,SCANPARMD_NUMENTS                                             
         STH   RF,SCANPARMD_NUMENTS  UPDATE NUMBER OF USED SLOTS                
         CH    RF,=AL2(MAXSCAN)                                                 
         JNL   CETOOMNY            TOO MANY PARAMETERS                          
         J     CRD02                                                            
*                                                                               
CRD03    DS    0H                                                               
         MVC   0(2,R3),=C'/*'      SET EOF IN SCANNER TABLE                     
*                                                                               
CRD06    DS    0H                                                               
         BRAS  RE,PRNT             PRINT A CLEAR LINE                           
         OC    ERRCNT,ERRCNT       ERRORS?                                      
         JZ    EXITOK                                                           
         MVC   P(26),=C'Errors found. Terminating.'                             
         BRAS  RE,PRNT                                                          
         J     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
*                                                                               
CARDVAL  NTR1                                                                   
*                                                                               
         USING SCANBLKD,R3                                                      
*                                                                               
CARDV02  DS    0H                                                               
         LA    R4,CARDTAB          NEXT PARAMETER                               
         USING CARDTABD,R4                                                      
         SR    RF,RF                                                            
*                                                                               
CARDV04  CLI   CNAME,CARDEOT       END OF TABLE                                 
         JE    CEINVKEY            INVALID KEYWORD                              
         CLC   CXLEN,SC1STLEN      RIGHT LENGTH?                                
         JNE   CARDV06                                                          
         ICM   RF,1,CXLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         JE    CARDV08                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV06  LA    R4,CARDTABL(,R4)                                                 
         J     CARDV04                                                          
*                                                                               
CARDV08  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         JNE   CARDV10             NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         JNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         JL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         JH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         J     CARDV22                                                          
*                                                                               
CARDV10  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         JNE   CARDV12             NO                                           
         SR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         JZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         JL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         JH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     CARDV22                                                          
         MVC   0(0,RE),SC2NDFLD                                                 
*                                                                               
CARDV12  CLI   CTYPE,CTRTN         ROUTINE                                      
         JNE   CARDV14             NO                                           
         ICM   RF,15,CRTN                                                       
         BASR  RE,RF                                                            
         J     CARDV22                                                          
*                                                                               
CARDV14  CLI   CTYPE,CTTBL         TABLE                                        
         JNE   CARDV20             NO                                           
         ICM   RE,15,CTBL          A(TABLE)                                     
         USING CTBLD,RE                                                         
         CLI   SC2NDLEN,L'CTBLIVAL KEYWORD MAX IS 8 CHARS                       
         JH    CETOOLNG                                                         
         SR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         JZ    CENOINP                                                          
         BCTR  RF,0                                                             
CARDV16  CLC   SC2NDLEN,CTBLMINL   INPUT TOO SHORT FOR THIS ENTRY               
         JL    CARDV17                                                          
         EX    RF,*+8                                                           
         JE    CARDV18                                                          
         CLC   SC2NDFLD(0),CTBLIVAL                                             
CARDV17  LA    RE,CTBLDLQ(,RE)                                                  
         CLI   0(RE),CARDEOT                                                    
         JNE   CARDV16                                                          
         J     CENOINP                                                          
CARDV18  ICM   RF,15,COUT          MOVE IN FIELD                                
         MVC   0(1,RF),CTBLOVAL                                                 
         J     CARDV22                                                          
         DROP  RE                                                               
*                                                                               
CARDV20  DS    0H                                                               
         CLI   CTYPE,CTSUBVAL      SUBROUTINE WILL VALIDATE?                    
         JE    CARDV22             YES                                          
*                                                                               
         DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CARDV22  DS    0H                                                               
         LA    R3,SCNENTLQ(,R3)    NEXT ENTRY                                   
         BRCT  R2,CARDV02                                                       
*                                                                               
         XIT1  REGS=(R3)           RETURN UPDATED R3 (NEXT TABLE SLOT)          
         DROP  R4                                                               
*                                                                               
CETOOMNY MVC   P+12(29),=C'Too many parameters. Max NNN.'                       
         LHI   R0,MAXSCAN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+37(3),DUB                                                      
         LA    R8,EXITL                                                         
         J     CERR1                                                            
*                                                                               
CEINVLIN MVC   P+12(19),=C'Invalid line format'                                 
         LA    R8,EXITL                                                         
         J     CERR1                                                            
*                                                                               
CEINVKEY MVC   P+12(16),=C'Invalid Keyword:'                                    
         MVC   P+29(10),SC1STFLD                                                
         LA    R8,CARDV22                                                       
         J     CERR1                                                            
*                                                                               
CENOTNUM MVC   P+23(19),=C'not a valid number:'                                 
         MVC   P+43(SCNRGHTQ),SC2NDFLD                                          
         J     CERR                                                             
*                                                                               
CEBADSYS MVC   P+12(28),=C'Invalid or duplicate system:'                        
         MVC   P+41(SCNRGHTQ),WORK                                              
         LA    R8,EXITL                                                         
         J     CERR1                                                            
*                                                                               
CENODSP  MVC   P+12(20),=C'DSPACE= must precede'                                
         MVC   P+33(8),SC1STFLD                                                 
         LA    R8,EXITL                                                         
         J     CERR1                                                            
*                                                                               
CETOOSHT MVC   P+23(10),=C'too short:'                                          
         MVC   P+34(SCNRGHTQ),SC2NDFLD                                          
         J     CERR                                                             
*                                                                               
CETOOLNG MVC   P+23(09),=C'too long:'                                           
         MVC   P+33(SCNRGHTQ),SC2NDFLD                                          
         J     CERR                                                             
*                                                                               
CETOOLOW MVC   P+23(10),=C'too small:'                                          
         MVC   P+34(SCNRGHTQ),SC2NDFLD                                          
         J     CERR                                                             
*                                                                               
CETOOBIG MVC   P+23(10),=C'too large:'                                          
         MVC   P+34(SCNRGHTQ),SC2NDFLD                                          
         J     CERR                                                             
*                                                                               
CENOINP  MVC   P+23(24),=C'invalid or missing value'                            
         J     CERR                                                             
*                                                                               
CEDSPNF  MVC   P+23(40),=C'Dataspace not found. May not be started.'            
         J     CERR                                                             
*                                                                               
CERR     MVC   P+12(10),SC1STFLD                                                
         LA    R8,CARDV22                                                       
CERR1    MVC   P(11),=CL11'***Error***'                                         
         BRAS  RE,PRNT                                                          
         LH    R0,ERRCNT                                                        
         AHI   R0,1                                                             
         STH   R0,ERRCNT                                                        
         BR    R8                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DATASPACE ID AND GET ALET                       *         
***********************************************************************         
*                                                                               
         USING SCANBLKD,R3                                                      
DSPVAL   CLI   SC2NDLEN,1          2ND LEN MUST BE 1                            
         JH    CETOOLNG                                                         
         JL    CENOINP                                                          
*                                                                               
         XC    WORK(28),WORK                                                    
         MVC   WORK+0(16),=C'GETADMG?DATAMGRX'                                  
         MVC   WORK+7(1),SC2NDFLD                                               
         CLI   SC2NDFLD,C'C'                                                    
         JE    DSPV02                                                           
         CLI   SC2NDFLD,C'T'                                                    
         JE    DSPV02                                                           
         CLI   SC2NDFLD,C'Q'                                                    
         JE    DSPV02                                                           
         CLI   SC2NDFLD,C'B'                                                    
         JE    DSPV02                                                           
         CLI   SC2NDFLD,C'R'                                                    
         JE    DSPV02                                                           
         CLI   SC2NDFLD,C'A'                                                    
         JE    DSPV02                                                           
         CLI   SC2NDFLD,C'P'                                                    
         JNE   CENOINP                                                          
         MVI   WORK+7,C'A'                                                      
*                                                                               
DSPV02   DS    0H                                                               
         LHI   R0,-21              GO GET DATASPACE                             
         LA    R1,WORK                                                          
         SVC   247                                                              
         LTR   RF,RF               ANY ERRORS?                                  
         JNZ   CEDSPNF                                                          
         OC    WORK+24(4),WORK+24                                               
         JZ    CEDSPNF                                                          
*                                                                               
         MVC   DSPACE,SC2NDFLD     SET DSPACE VALUES                            
         LARL  RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),DSPACE                                      
         MVC   DSPALET,WORK+24                                                  
*                                                                               
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DDSIO= CARD                                     *         
***********************************************************************         
*                                                                               
         USING SCANBLKD,R3                                                      
DDSIOVAL DS    0H                                                               
         CLI   SC2NDLEN,8          2ND LEN MUST BE BETWEEN 5 AND 8              
         JH    CETOOLNG                                                         
         CLI   SC2NDLEN,5                                                       
         JL    CETOOSHT                                                         
*                                                                               
         ICM   RF,15,=V(DDSIO)                                                  
         JZ    *+10                                                             
         MVC   0(8,RF),SC2NDFLD    DDSIO LOAD MODULE NAME                       
*                                                                               
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A LIST OF SE#S                                  *         
* CARD CONTAINS SE=NN,NN,NN,...                                       *         
* R2=N'ENTRIES LEFT, R3=A(SE= ENTRY)                                  *         
* RETURNS UPDATED R2,R3                                               *         
***********************************************************************         
*                                                                               
SEVAL    NTR1                                                                   
*                                                                               
         CLI   DSPACE,C' '         DO WE HAVE A DSPACE YET?                     
         JH    *+12                YES                                          
         LA    RF,CENODSP          A(THIS PARTICULAR ERROR ROUTINE)             
         J     SEV03NTR            PRINT ERROR                                  
*                                                                               
         USING SCANBLKD,R3                                                      
         MVC   WORK(SCNRGHTQ),SC2NDFLD                                          
         CLI   SC2NDLEN,2          2ND LEN MUST BE 2                            
         JNE   SEV03                                                            
         TM    SC2NDVAL,SCHEXQ     2ND FIELD MUST BE HEX                        
         JZ    SEV03                                                            
*                                                                               
SEV02    DS    0H                                                               
         GOTO1 =V(HEXIN),DMCB,WORK,BYTE,2,0                                     
         ICM   RF,15,12(R1)                                                     
         JZ    SEV03                                                            
         LLC   RF,BYTE                                                          
         LTR   RF,RF               RF IS SE NUMBER                              
         JZ    SEV03               MUSTN'T BE ZERO                              
*                                                                               
         XC    DUB(L'DSPNAME),DUB  GET SYSTEM NAME FROM DSPACE                  
         STAR  LABEL=Y,CLEAR=Y,ARS=ON                                           
         LAM   AR4,AR4,DSPALET                                                  
         SR    R4,R4                                                            
         L     R1,0(,R4)           A(ECB TABLE) = UPPER LIMIT                   
         LR    R4,RF                                                            
         MHI   R4,L'DSPHDR         SE# * 64 (L'RESOURCE HEADER)                 
         CR    R4,R1               ENSURE NOT PASSED UPPER LIMIT                
         JNL   *+10                                                             
         MVC   DUB(L'DSPNAME),DSPNAME-DMSPACED(R4)                              
         REAR  ARS=OFF                                                          
         CLI   DUB,C' '            DID WE FIND A SYSTEM NAME?                   
         JNH   SEV03               NO, ERROR                                    
*                                                                               
         BCTR  RF,0                                                             
         MHI   RF,ATTBLKLQ                                                      
         LARL  R1,ATTBLK                                                        
         USING ATTBLKD,R1                                                       
         AR    R1,RF               POINT TO A(ATT BLOCK) FOR THIS SE            
         OC    0(ATTBLKLQ,R1),0(R1)    SHOULD BE ZERO                           
         JNZ   SEV03               IF NOT IT'S A DUPLICATE                      
         MVC   ATTBSE#,BYTE        MARK ACTIVE BY SETTING SE IN TABLE           
         DROP  R1                                                               
*                                                                               
*                                  GET A(SYSFLES) (FORCE SYSTEM X'01')          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'SYSFLES',1,0,0,0              
         SR    RF,RF                                                            
         ICM   RF,7,13(R1)         RF = A(SYSFLES)                              
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   RF,12               BACK UP TO A(SYSFTAB)                        
         L     RF,0(RF)            RF=A(SYSFTAB)                                
         LLC   R0,BYTE             R0=SENUM                                     
         AR    RF,R0               INDEX INTO TABLE: RF = A(MAJOR SYS)          
         CLI   MAJORSYS,0          ANY SYSTEM FOUND YET?                        
         JNE   *+14                YES                                          
         MVC   MAJORSYS,0(RF)      NO: SAVE THE FIRST ONE                       
         J     *+14                                                             
         CLC   MAJORSYS,0(RF)      IS THIS SYSTEM THE SAME AS THE LAST?         
         JNE   SEV03               NO: CANNOT MIX MAJOR SYSTEMS                 
*                                                                               
         STM   R2,R3,DUB           SAVE R2,R3                                   
         LA    R3,SCNENTLQ(,R3)    NEXT ENTRY                                   
         BRCT  R2,*+8                                                           
         J     SEV04               NO MORE ENTRIES                              
*                                                                               
         CLI   SC2NDLEN,0          IF 2ND LEN ZERO, THIS IS ANOTHER SE          
         JNE   SEV04                                                            
         MVC   WORK(SCNRGHTQ),SC1STFLD                                          
         CLI   SC1STLEN,2          SO 1ST LEN MUST BE 2                         
         JNE   SEV03                                                            
         TM    SC1STVAL,SCHEXQ     1ST FIELD MUST BE HEX                        
         JO    SEV02               GO VALIDATE IT                               
         DROP  R3                                                               
*                                                                               
SEV03    LA    RE,SEVXIT1          RETURN ADDRESS FOR CERR                      
         LA    RF,CEBADSYS         A(THIS PARTICULAR ERROR ROUTINE)             
*                                  FALL INTO NTR1                               
*                                                                               
SEV03NTR NTR1  ,                   SET UP SO CERR COMES BACK                    
         BR    RF                  PRINT ERROR, RETURN TO SEVXIT1               
*                                                                               
SEV04    LM    R2,R3,DUB           RELOAD SAVED PTRS AS NTRY NOT USED           
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
SEVXIT1  DS    0H                                                               
         XIT1  REGS=(R2,R3)        RETURN UPDATED R2,R3                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A LIST OF SYSTEM NAMES                          *         
* CARD CONTAINS SYS=XXXXX,YYYYY,ZZZZ                                  *         
* R2=N'ENTRIES LEFT, R3=A(SE= ENTRY)                                  *         
* RETURNS UPDATED R2,R3                                               *         
***********************************************************************         
*                                                                               
SYSVAL   NTR1                                                                   
*                                                                               
         CLI   DSPACE,C' '         DO WE HAVE A DSPACE YET?                     
         JH    *+12                YES                                          
         LA    RF,CENODSP          A(THIS PARTICULAR ERROR ROUTINE)             
         J     SYSV03NTR           PRINT ERROR                                  
*                                                                               
         USING SCANBLKD,R3                                                      
         MVC   WORK(SCNRGHTQ),SC2NDFLD                                          
         CLI   SC2NDLEN,0          2ND VALUE MUST BE PRESENT                    
         JE    SYSV03                                                           
         CLI   SC2NDLEN,7          2ND LEN MUST BE 7 OR LESS                    
         JH    SYSV03                                                           
SYSV02   SR    RF,RF               SE NUMBER FROM DATASPACE                     
         STAR  LABEL=Y,CLEAR=Y,ARS=ON                                           
         LAM   AR4,AR4,DSPALET                                                  
         SR    R4,R4                                                            
         L     R1,0(,R4)           A(ECB TABLE) = UPPER LIMIT                   
         BASR  RE,0                START LOOP                                   
         USING DMSPACED,R4                                                      
         LA    RF,1(,RF)                                                        
         LA    R4,L'DSPHDR(,R4)                                                 
         CR    R4,R1               ENSURE NOT PASSED UPPER LIMIT                
         JL    SYSV02E             IT HASN'T                                    
         REAR  ARS=OFF                                                          
         J     SYSV03              SYSTEM DOES NOT EXIST                        
*                                                                               
SYSV02E  DS    0H                                                               
         CLC   WORK(L'DSPNAME),DSPNAME                                          
         BNER  RE                  LOOP TILL NAME FOUND                         
         DROP  R4                                                               
*                                                                               
         REAR  ARS=OFF                                                          
         STC   RF,BYTE             STORE SE NUMBER                              
         BCTR  RF,0                                                             
         MHI   RF,ATTBLKLQ                                                      
         LARL  R1,ATTBLK                                                        
         AR    R1,RF               POINT TO A(ATT BLOCK) FOR THIS SE            
         USING ATTBLKD,R1                                                       
         OC    0(ATTBLKLQ,R1),0(R1)    SHOULD BE ZERO                           
         JNZ   SYSV03              IF NOT IT'S A DUPLICATE                      
         MVC   ATTBSE#,BYTE        MARK ACTIVE BY SETTING SE IN TABLE           
         DROP  R1                                                               
*                                                                               
*                                  GET A(SYSFLES) (FORCE SYSTEM X'01')          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'SYSFLES',1,0,0,0              
         SR    RF,RF                                                            
         ICM   RF,7,13(R1)         RF = A(SYSFLES)                              
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   RF,12               BACK UP TO A(SYSFTAB)                        
         L     RF,0(RF)            RF=A(SYSFTAB)                                
         LLC   R0,BYTE             R0=SENUM                                     
         AR    RF,R0               INDEX INTO TABLE: RF = A(MAJOR SYS)          
         CLI   MAJORSYS,0          ANY SYSTEM FOUND YET?                        
         JNE   *+14                YES                                          
         MVC   MAJORSYS,0(RF)      NO: SAVE THE FIRST ONE                       
         J     *+14                                                             
         CLC   MAJORSYS,0(RF)      IS THIS SYSTEM THE SAME AS THE LAST?         
         JNE   SYSV03              NO: CANNOT MIX MAJOR SYSTEMS                 
*                                                                               
         STM   R2,R3,DUB           SAVE R2,R3                                   
         LA    R3,SCNENTLQ(,R3)    NEXT ENTRY                                   
         BRCT  R2,*+8                                                           
         J     SYSV04              NO MORE ENTRIES                              
*                                                                               
         CLI   SC2NDLEN,0          IF 2ND LEN ZERO, THIS IS ANOTHER SYS         
         JNE   SYSV04                                                           
         MVC   WORK(SCNRGHTQ),SC1STFLD                                          
         CLI   SC1STLEN,0          SO 1ST VALUE MUST BE PRESENT                 
         JE    SYSV03                                                           
         CLI   SC1STLEN,7          AND LEN MUST BE 7 OR LESS                    
         JNH   SYSV02              GO VALIDATE IT                               
         DROP  R3                                                               
*                                                                               
SYSV03   LA    RE,SYSXIT1          RETURN ADDRESS FOR CERR                      
         LA    RF,CEBADSYS         A(THIS PARTICULAR ERROR ROUTINE)             
*                                  FALL INTO NTR1                               
*                                                                               
SYSV03NTR NTR1 ,                   SET UP SO CERR COMES BACK                    
         BR    RF                  PRINT ERROR, RETURN TO SYSXIT1               
*                                                                               
SYSV04   LM    R2,R3,DUB           RELOAD SAVED PTRS AS NTRY NOT USED           
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
SYSXIT1  DS    0H                                                               
         XIT1  REGS=(R2,R3)        RETURN UPDATED R2,R3                         
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
*                                                                               
CARDTAB  DS    0D                                                               
         DC    CL8'DDSIO   ',AL4(0,0)                                           
         DC    AL1(5,CTRTN,0,0),AL4(DDSIOVAL)                                   
         DC    CL8'SERVER  ',AL4(4,L'MITMSRV)                                   
         DC    AL1(6,CTCHR,L'MITMSRV,0),AL4(MITMSRV)                            
         DC    CL8'INITMOD ',AL4(4,L'INITMOD)                                   
         DC    AL1(7,CTCHR,L'INITMOD,0),AL4(INITMOD)                            
         DC    CL8'DSPACE  ',AL4(0,0)                                           
         DC    AL1(6,CTRTN,0,0),AL4(DSPVAL)                                     
         DC    CL8'SE      ',AL4(0,0)                                           
         DC    AL1(2,CTRTN,0,0),AL4(SEVAL)                                      
         DC    CL8'SYS     ',AL4(0,0)                                           
         DC    AL1(3,CTRTN,0,0),AL4(SYSVAL)                                     
         DC    CL8'TRACE   ',AL4(CARDTRTB,0)                                    
         DC    AL1(5,CTTBL,0,0),AL4(TRACE)                                      
         DC    CL8'SUBABEND',AL4(CARDSATB,0)                                    
         DC    AL1(8,CTTBL,0,0),AL4(SUBABEND)                                   
         DC    CL8'MAXABEND',AL4(0,255)                                         
         DC    AL1(8,CTNUM,0,0),AL4(MAXABEND)                                   
         DC    CL8'QMGR    ',AL4(0,255)                                         
         DC    AL1(4,CTSUBVAL,0,0),AL4(0)                                       
         DC    CL8'QUEUE   ',AL4(0,255)                                         
         DC    AL1(5,CTSUBVAL,0,0),AL4(0)                                       
         DC    CL8'QLABEL  ',AL4(0,255)                                         
         DC    AL1(6,CTSUBVAL,0,0),AL4(0)                                       
CARDTABX DC    AL1(CARDEOT)                                                     
*                                                                               
CARDTRTB DC    AL1(1),C'YES     ',C'Y' TRACE=                                   
         DC    AL1(2),C'ON      ',C'Y'                                          
         DC    AL1(1),C'NO      ',C'N'                                          
         DC    AL1(3),C'OFF     ',C'N'                                          
         DC    AL1(1),C'DETAIL  ',C'D'                                          
         DC    AL1(CARDEOT)                                                     
*                                                                               
CARDSATB DC    AL1(1),C'CONTINUE',C'C' SUBABEND=                                
         DC    AL1(1),C'EOJ     ',C'E'                                          
         DC    AL1(CARDEOT)                                                     
*                                                                               
***********************************************************************         
* CARD TABLE DSECTS                                                   *         
***********************************************************************         
*                                                                               
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
         ORG   CMIN                                                             
CTBL     DS    A                   A(TABLE) OF VALUES                           
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LENGTH OF CNAME VALUE FOR COMPARE            
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CTRTN    EQU   3                   ROUTINE                                      
CTTBL    EQU   4                   TABLE                                        
CTSUBVAL EQU   5                   SUBTASK WILL VALIDATE                        
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
         ORG   COUT                                                             
CRTN     DS    AL4                                                              
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
*                                                                               
CTBLD    DSECT                                                                  
CTBLMINL DS    AL1                 MINIMUM VALUE LENGTH                         
CTBLIVAL DS    CL8                 INPUT VALUE                                  
CTBLOVAL DS    CL1                 OUTPUT VALUE                                 
CTBLDLQ  EQU   *-CTBLD                                                          
*                                                                               
MITMCTL  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PLINE TO A PRINT LINE                             *         
***********************************************************************         
*                                                                               
PRNT     NTR1                                                                   
         CLI   NOTIME,C'Y'                                                      
         JE    PRNT04                                                           
*                                                                               
         TIME  DEC,PRNTWORK,LINKAGE=SYSTEM,DATETYPE=YYYYMMDD                    
         LAM   ARE,AR1,=16F'0'     MACRO CAN CLOBBER ANY OF THESE               
*                                                                               
*        ABOVE MACRO RETURNS (BOTH AS PWOS):                                    
*        TIME IN PRNTWORK+0(8) AS HHMMSSMMMMMM0000 (MILLISECONDS),              
*                          E.G. X'1043561285570000'                             
*        DATE IN PRNTWORK+8(4) AS YYYYMMDD,                                     
*                          E.G. X'20100812'                                     
*                                                                               
         MVI   PRNTWORK+12,X'0F'         PRNTWORK+8=YYYYMMDD0F                  
         UNPK  PLINTIME(3),PRNTWORK+11(2) PLINTIME=C'DD0'                       
         MVC   PLINDAY,PLINTIME          PLINDAY=C'DD'                          
*                                                                               
         MVI   PRNTWORK+4,X'0F'          PRNTWORK=HHMMSSTT0F......              
         UNPK  PRNTWORK+6(9),PRNTWORK(5) PRNTWORK+6=C'HHMMSSTT0'                
         MVC   PLINTIME+00(2),PRNTWORK+6 PLINTIME=C'HH'                         
         MVI   PLINTIME+02,C':'                     HH:                         
         MVC   PLINTIME+03(2),PRNTWORK+8            HH:MM                       
         MVI   PLINTIME+05,C':'                     HH:MM:                      
         MVC   PLINTIME+06(2),PRNTWORK+10           HH:MM:SS                    
         MVI   PLINTIME+08,C'.'                     HH:MM:SS.                   
         MVC   PLINTIME+09(2),PRNTWORK+12           HH:MM:SS.TT                 
*                                                                               
PRNT04   MVC   PLINSYS,=C'Ctrl:'   INDICATES MAIN CONTROLLER                    
         LARL  R1,SYSPRINT                                                      
         PUT   (1),PCC                                                          
         MVI   PCC,C' '                                                         
         MVC   PLINE,SPACES                                                     
         MVI   NOTIME,C'N'                                                      
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT STATUS FROM TASK WITH ATTBLK AT R4                            *         
***********************************************************************         
*                                                                               
STATUS   NTR1                                                                   
*                                                                               
         USING ATTD,R4                                                          
         MVC   WORK,SPACES                                                      
         MVC   WORK(3),=C'SE:'                                                  
         MVC   WORK+3(2),ATTSEC                                                 
         MVC   WORK+6(7),ATTSYSN                                                
         MVC   WORK+14(8),=C'DSpace:x'                                          
         MVC   WORK+21(1),ATTDSPCE                                              
*                                                                               
         LA    R1,WORK+34+7                                                     
         MVC   WORK+34(7),=C'Closing'                                           
         TM    ATTFLAGS,ATTFCLSE                                                
         JO    STAT02                                                           
         MVC   WORK+34(7),=C'Started'                                           
         TM    ATTFLAGS,ATTFGO                                                  
         JO    STAT02                                                           
         MVC   WORK+34(7),=C'Abended'                                           
         TM    ATTFLAGS,ATTFABND                                                
         JO    STAT02                                                           
         MVC   WORK+34(7),=C'Stopped'                                           
*                                                                               
STAT02   DS    0H                                                               
         TM    ATTFLAGS,ATTFINI                                                 
         JZ    *+14                                                             
         MVC   0(14,R1),=C', Initialising'                                      
         LA    R1,14(,R1)                                                       
         TM    ATTFLAGS,ATTFHLO                                                 
         JZ    *+14                                                             
         MVC   0(11,R1),=C', Connected'                                         
         LA    R1,11(,R1)                                                       
         TM    ATTFLAGS,ATTFSEND                                                
         JZ    *+14                                                             
         MVC   0(14,R1),=C', SEND pending'                                      
         LA    R1,14(,R1)                                                       
         CLI   ATTTRACE,C'Y'                                                    
         JNE   *+14                                                             
         MVC   0(10,R1),=C', Trace On'                                          
         LA    R1,10(,R1)                                                       
*                                                                               
         MVI   0(R1),C'.'                                                       
         LA    R0,WORK                                                          
         SR    R1,R0                                                            
         EX    R1,*+8              (NO BCTR SO MOVES DOT)                       
         J     *+10                                                             
         MVC   P(0),WORK                                                        
         CLI   OPERCOMM,C' '                                                    
         JE    STAT04                                                           
         AR    R1,R0                                                            
         MVC   0(2,R1),=C'//'                                                   
*                                  WTO IF RESULT OF OPERATOR COMMAND            
         GOTO1 =V(DDWTO),DMCB,WORK,0                                            
*                                                                               
STAT04   DS    0H                                                               
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P+6(7),=C'Started'                                               
         GOTOR TIMEOUT,DMCB,P+14,ATTTLATT                                       
         LA    RF,P+28                                                          
         CLC   ATTTFATT,ATTTLATT                                                
         JE    STAT06                                                           
         MVC   P+29(13),=C'First started'                                       
         GOTOR TIMEOUT,DMCB,P+43,ATTTFATT                                       
         LA    RF,P+57                                                          
STAT06   OC    ATTABNCT,ATTABNCT                                                
         JZ    STAT08                                                           
         LH    R1,ATTABNCT                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,RF),DUB                                                      
         MVC   5(6,RF),=C'abends'                                               
         LA    RF,10(,RF)                                                       
STAT08   MVI   0(RF),C'.'                                                       
         MVI   NOTIME,C'Y'                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
*&&DO                                                                           
         MVC   P+6(7),=C'Connect'                                               
         MVC   P+14(5),=C'Never'                                                
         LA    RF,P+19                                                          
         OC    ATTTLC,ATTTLC                                                    
         JZ    STAT12                                                           
         MVC   P+14(5),SPACES                                                   
         GOTOR TIMEOUT,DMCB,P+14,ATTTLC                                         
         LA    RF,P+28                                                          
         CLC   ATTTFC,ATTTLC                                                    
         JE    STAT10                                                           
         MVC   P+29(13),=C'First connect'                                       
         GOTOR TIMEOUT,DMCB,P+43,ATTTFC                                         
         LA    RF,P+57                                                          
STAT10   ICM   R1,15,ATTNERRS                                                   
         JZ    STAT12                                                           
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(5,RF),DUB                                                      
         MVC   7(6,RF),=C'errors'                                               
         LA    RF,13(,RF)                                                       
STAT12   MVI   0(RF),C'.'                                                       
         MVI   NOTIME,C'Y'                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    ATTTLD,ATTTLD                                                    
         JZ    STAT14                                                           
         MVC   P+27(15),=C'Last disconnect'                                     
         GOTOR TIMEOUT,DMCB,P+43,ATTTLD                                         
         ICM   R1,15,ATTNCONS                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+58(3),DUB                                                      
         MVC   P+62(9),=C'connects.'                                            
         MVI   NOTIME,C'Y'                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
STAT14   DS    0H                                                               
         OC    ATTTFMI,ATTTFMI                                                  
         JZ    STAT16                                                           
         MVC   P+6(5),=C'First'                                                 
         MVC   P+12(2),=C'IP'                                                   
         MVC   P+15(5),=C'rcvd.'                                                
         GOTOR TIMEOUT,DMCB,P+21,ATTTFMI                                        
         MVC   P+36(6),=C'latest'                                               
         GOTOR TIMEOUT,DMCB,P+43,ATTTLMI                                        
         MVI   P+57,C'.'                                                        
         MVI   NOTIME,C'Y'                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
*&&                                                                             
STAT16   DS    0H                                                               
         OC    ATTTFRU,ATTTFRU                                                  
         JZ    STAT18                                                           
         MVC   P+6(5),=C'First'                                                 
         MVC   P+12(3),=C'USS'                                                  
         MVC   P+16(4),=C'read'                                                 
         GOTOR TIMEOUT,DMCB,P+21,ATTTFRU                                        
         MVC   P+36(6),=C'latest'                                               
         GOTOR TIMEOUT,DMCB,P+43,ATTTLRU                                        
         MVC   P+58(8),=C'#records'                                             
         ICM   R1,15,ATTNRECU                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+67(7),DUB                                                      
         MVC   P+75(6),=C'#bytes'                                               
         ICM   R1,15,ATTNBYTU                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+82(7),DUB                                                      
         MVC   P+89(2),=C'k.'                                                   
         MVI   NOTIME,C'Y'                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
STAT18   DS    0H                                                               
*&&DO                                                                           
         OC    ATTTFMO,ATTTFMO                                                  
         JZ    STAT20                                                           
         MVC   P+6(5),=C'First'                                                 
         MVC   P+12(2),=C'IP'                                                   
         MVC   P+15(4),=C'send'                                                 
         GOTOR TIMEOUT,DMCB,P+21,ATTTFMO                                        
         MVC   P+36(6),=C'latest'                                               
         GOTOR TIMEOUT,DMCB,P+43,ATTTLMO                                        
         MVC   P+58(8),=C'#records'                                             
         ICM   R1,15,ATTNRECO                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+67(7),DUB                                                      
         MVC   P+75(6),=C'#bytes'                                               
         ICM   R1,15,ATTNBYTO                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+82(7),DUB                                                      
         MVC   P+89(2),=C'k.'                                                   
         MVI   NOTIME,C'Y'                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P+58(8),=C'#commits'                                             
         ICM   R1,15,ATTNEOTO                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+67(5),DUB                                                      
         MVC   P+73(10),=C'#rollbacks'                                          
         ICM   R1,15,ATTNRBKO                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+84(5),DUB                                                      
         MVI   P+89,C'.'                                                        
         MVI   NOTIME,C'Y'                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
STAT20   DS    0H                                                               
*&&                                                                             
         J     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* EDIT A TIME TO DD HH:MM:SS.TT                                       *         
*        P1=A(OUTPUT), P2=A(PWOS(YYYYMMDDHHMMSSTT))                   *         
***********************************************************************         
*                                                                               
TIMEOUT  DS    0H                                                               
         LR    R0,RE                                                            
         LM    RE,RF,0(R1)                                                      
         MVC   DUB(1),3(RF)        DUB =X'DD..............'                     
         MVI   DUB+1,X'0F'                DD0F                                  
         UNPK  WORK(3),DUB(2)      WORK=C'DD0'                                  
         MVC   00(2,RE),WORK       DD                                           
         MVC   DUB(4),4(RF)        DUB =X'HHMMSSTT........'                     
         MVI   DUB+4,X'0F'                HHMMSSTT0F                            
         UNPK  WORK(9),DUB(5)      WORK=C'HHMMSSTT0'                            
         MVC   03(2,RE),WORK          HH                                        
         MVI   05(RE),C':'              :                                       
         MVC   06(2,RE),WORK+2           MM                                     
         MVI   08(RE),C':'                 :                                    
         MVC   09(2,RE),WORK+4              SS                                  
         MVI   11(RE),C'.'                    .                                 
         MVC   12(2,RE),WORK+6                 TT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET A TIMER FOR TIMERINT SECONDS                                    *         
***********************************************************************         
*                                                                               
SETWAIT  NTR1                                                                   
*                                                                               
         OC    STIMERID,STIMERID   STIMER SET UP?                               
         JZ    SWT02               NO                                           
*                                                                               
         STIMERM TEST,ID=STIMERID,TU=FULL                                       
         ICM   R0,15,FULL                                                       
         JZ    SWT02               NO CURRENT TIMER ACTIVE                      
*                                                                               
         STIMERM CANCEL,ID=STIMERID                                             
*                                                                               
SWT02    XC    TIMERECB,TIMERECB   CLEAR ECB                                    
         STIMERM SET,ID=STIMERID,BINTVL=TIMERINT,EXIT=TIMERXIT                  
         LTR   RF,RF                                                            
         JZ    EXITOK                                                           
         DC    H'0'                WHY CAN'T WE SET THE TIMER?                  
*                                                                               
***********************************************************************         
* THE TIMER EXIT ROUTINE.  IT POSTS THE TIMERECB.                     *         
***********************************************************************         
*                                                                               
TIMERXIT SAVE  (14,12),,*                                                       
*                                                                               
         PUSH  USING                                                            
         DROP  RB,RA,R9                                                         
         USING TIMERXIT,RF                                                      
         LM    R9,RB,MITMBASE                                                   
         POP   USING                                                            
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS AND USEFUL ROUTINES                                     *         
***********************************************************************         
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         J     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         J     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         J     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
*                                                                               
POSTED   EQU   X'40'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
SPACES   DC    166C' '                                                          
         EJECT                                                                  
***********************************************************************         
* VARIABLES                                                           *         
***********************************************************************         
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
BYTE     DS    X                                                                
*                                                                               
WORK     DS    XL132                                                            
*                                                                               
RETCODE  DC    F'0'                PROGRAM RETURN CODE                          
DMCB     DS    6A                                                               
*                                                                               
MITMBASE DS    3A                  BASE REGISTER SAVE                           
*                                                                               
TIMERECB DC    F'0'                TIMER ECB                                    
AOPERECB DC    F'0'                A(OPERATOR ECB)                              
*                                                                               
TIMERINT DC    A(0)                TIMER INTERVAL                               
STIMERID DC    XL4'00'                                                          
*                                                                               
ACOMM    DC    A(0)                                                             
*                                                                               
MAXABEND DC    F'0'                                                             
*                                                                               
DSPALET  DC    F'0'                                                             
*                                                                               
DSPACE   DC    C' '                DATASPACE ID                                 
TRACE    DC    C'N'                                                             
SUBABEND DC    C'C'                C=CONTINUE, E=EOJ                            
MAJORSYS DC    X'00'               MAJOR SYSTEM # (E.G., X'06' = ACC)           
*                                                                               
ERRCNT   DC    H'0'                                                             
*                                                                               
* !!! maybe this shouldn't default to anything.                                 
* !!! (will there be only one type of server per instance of the                
*      controller?) YES!                                                        
MITMSRV  DC    CL8'MITMSRV '                                                    
INITMOD  DS    CL8                                                              
AINITMOD DS    A                   A(LOADED INITIALIZATION MODULE)              
*                                                                               
PRNTWORK DS    CL16                FOR PRNT ROUTINE ONLY                        
NOTIME   DC    C'N'                                                             
OPERSTOP DC    C'N'                                                             
OPERCOMM DC    C' '                                                             
OPERALL  DC    C' '                                                             
OPEROPT  DC    C' '                                                             
*                                                                               
CARDHDR  DC    XL8'00'                                                          
CARD     DS    CL80                                                             
*                                                                               
PCC      DS    C                                                                
PLINE    DS    0CL166                                                           
PLINDAY  DS    CL2                 DD                                           
         DS    C                                                                
PLINTIME DS    CL11                HH:MM:SS.HH                                  
         DS    C                                                                
PLINSYS  DS    CL5                 SE:XX / SYSYY                                
         DS    C                                                                
P        DS    CL132                                                            
         DS    CL13                                                             
*                                                                               
SYSIN    DCB   DDNAME=SYSIN,MACRF=GM,DSORG=PS,RECFM=FB,LRECL=80,       +        
               EODAD=CRD03                                                      
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
         EJECT                                                                  
ACFLIST  DS    0D                  ** ACCPAK FILE LIST **                       
         DC    CL8'NACCDIR'                                                     
         DC    CL8'NACCMST'                                                     
         DC    CL8'NACCARC'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
*                                                                               
CTFLIST  DS    0D                  ** CONTROL FILE LIST **                      
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* NO BASE ADDRESSABILITY BEYOND HERE                                  *         
***********************************************************************         
         DROP  RB,RA,R9                                                         
         EJECT                                                                  
***********************************************************************         
* ATTACHED TASK ASYNCHRONOUS ESTAI EXIT ROUTINE                       *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
ATTESTAI STM   RE,RC,12(RD)        SAVE CALLING REGS                            
*                                                                               
         LR    RB,RF                                                            
         USING ATTESTAI,RB                                                      
*                                                                               
         ST    RD,ESTAIRD          SAVE SAVE AREA POINTER                       
         ST    R1,ESTAIR1          AND POINTER TO SDWA                          
*                                                                               
         L     R3,SDWAPARM-SDWA(,R1)       GET ATTACH PARMS ADDRESS             
         L     R4,ATTBPTR-ATTBLKD(,R3)     ONLY PARM IS A(ATT BLOCK)            
         OI    ATTFLAGS-ATTD(R4),ATTFABND  FLAG TASK ABENDED                    
*                                                                               
         L     RD,ESTAIRD                                                       
         CHI   R0,12               SDWA ACQUIRED?                               
         LM    RE,RC,12(RD)                                                     
         BNER  RE                                                               
         SR    RF,RF               NO, LEAVE TO MVS                             
         BR    RE                                                               
*                                                                               
ESTAIRD  DC    F'0'                                                             
ESTAIR1  DC    F'0'                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
SCNBLK   DS    (5+1)CL(SCNENTLQ)   LEAVE A LITTLE EXTRA ROOM                    
*                                                                               
*                                                                               
***********************************************************************         
* SCANNED PARAMETER BLOCK                                             *         
***********************************************************************         
*                                                                               
SCANNED_PARAMETERS DS 0D                                                        
         DS    CL(SCANPARMD_BLOCK-SCANPARMD)                                    
SCNCARDS DC    (MAXSCAN)CL(SCNENTLQ)' '                                         
SCNRGHTQ EQU   50                  L'RIGHT SIDE OF SCANNER ENTRY                
SCNENTLQ EQU   SCBLKLQ-L'SC2NDFLD+SCNRGHTQ   L'SCANNER TABLE ENTRY              
MAXSCAN  EQU   100                                                              
*                                                                               
*                                                                               
***********************************************************************         
* ECB LIST                                                            *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
         DC    C'*ECBLST**ECBLST*'                                              
ECBLST   DC    256A(0)             ONE PER SE                                   
         DC    C'*ECBLSTX*ECBLSTX'                                              
*                                                                               
***********************************************************************         
* ATTACHED TASK BLOCK POINTERS                                        *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
         DC    C'*ATTBLK**ATTBLK*'                                              
ATTBLK   DC    256XL(ATTBLKLQ)'00' ONE PER SE                                   
         DC    X'FFFFFFFFFFFFFFFF'                                              
         DC    C'ATTBLKX*ATTBLKX*'                                              
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    CL16'**SSB*SSB*SSB***'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         DC    X'10'               SSOSGALO                                     
         ORG                                                                    
         SPACE 3                                                                
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    256X'00'                                                         
         EJECT                                                                  
***********************************************************************         
* WORK AREA                                                           *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
         DC    C'WORKAREA'                                                      
WORKAREA DS    (16*1024)X                                                       
         DC    C'WORKAREX'                                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
* USEFUL DSECT BLOCKS                                                 *         
***********************************************************************         
*                                                                               
       ++INCLUDE DDMITMATTD                                                     
*                                                                               
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*                                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
*                                                                               
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
*                                                                               
* IHASDWA                                                                       
         PRINT OFF                                                              
         IHASDWA                                                                
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043DDMITMCTL 07/31/13'                                      
         END                                                                    
