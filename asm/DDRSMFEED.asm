*          DATA SET DDRSMFEED  AT LEVEL 014 AS OF 09/13/19                      
*PHASE RSMFEEDB                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE DDWTO                                                                  
*INCLUDE USSQINQ                                                                
*INCLUDE MDUMPER                                                                
         TITLE 'DDRSMFEED - Read stats and push to RSM'                         
         PRINT GEN                                                              
         IEABRCX DEFINE                                                         
         IEABRCX DISABLE                                                        
*************************************************************                   
* LINK USING DSN=SYS2.RSM.MACLIB,DISP=SHR                   *                   
*************************************************************                   
*&&NOP   SET   N                                                                
                                                                                
RSMXFEED CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         NBASE WORKX-WORKD,**RSMF**,=A(WORKAREA),RA,CLEAR=YES                   
         USING WORKD,RC                                                         
         J     START                                                            
*                                                                               
$$DATA   LOCTR ,                   8k DATA BLOCK                                
DATABLK  DC    (8192-(DATABLK-RSMXFEED))X'00'                                   
         ORG   DATABLK                                                          
$$CODE   LOCTR ,                   CODE at +8k AFTER DATA                       
                                                                                
START    ST    RD,SAVERD                                                        
*                                                                               
         BRAS  RE,PRINTI           INIT PRINTING                                
*                                                                               
         BRAS  RE,INIT             READ CARDS ECT                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LARL  RF,ERREXT           EXIT ROUTINE                                 
         ST    RF,DMCB+4                                                        
         MVI   DMCB+4,15           MAX 15 DUMPS                                 
         CLI   ESTAESW,C'Y'                                                     
         JNE   RESTART                                                          
*                                                                               
         GOTO1 =V(MDUMPER),DMCB,=C'INIT'                                        
*                                                                               
RESTART  SAM24                     MAKE SURE WE RESTART IN 24 BIT               
         SAC   0                                                                
         CLI   ENDTASK,C'Y'        DO WE WANT TO CANCEL                         
         JE    ENDOFJOB                                                         
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,SENDLIM          SENT LIMITS FILE                             
RSM010   BRAS  RE,ARSOFF                                                        
         BRAS  RE,SETWAIT          MAIN LOOP                                    
         BRAS  RE,TIMECHK          CHECK TIMES                                  
         BRAS  RE,MAIN                                                          
         BRAS  RE,ARSOFF                                                        
         J     RSM010                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK TIMERS - SEE IF ANYTHING TO DO                                *         
***********************************************************************         
TIMECHK  NTR1  ,                                                                
         LARL  RF,TIMETAB                                                       
*                                                                               
TIMECHK0 CLI   0(RF),C'*'          HAS OPER SET THIS ON                         
         JE    TIMECHK1                                                         
         MVI   0(RF),C'N'          LETS ASSUME NOT                              
         L     R1,TIMENOW          R1 = TIME NOW                                
         SR    R0,R0                                                            
         S     R1,4(RF)            SUBTRACT LAST TIME                           
*                                                                               
         JM    TIMECHK1            -VE MUST BE MIDNIGHT                         
*                                                                               
         CLM   R1,7,1(RF)          IS IT > TIME FREQ                            
         JL    TIMECHK2                                                         
*                                                                               
TIMECHK1 MVI   0(RF),C'Y'          OK DO IT                                     
         MVC   4(4,RF),TIMENOW     SET TIME WE DID IT                           
*                                                                               
TIMECHK2 LA    RF,8(RF)            NEXT                                         
         CLI   0(RF),X'FF'                                                      
         JNE   TIMECHK0                                                         
*                                                                               
TIMECHKX J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SEND LIMIT FILE                                                     *         
***********************************************************************         
SENDLIM  NTR1                                                                   
         LARL  R7,SENDLIM                                                       
         USING SENDLIM,R7                                                       
*                                                                               
         OPEN  (LIMITS,INPUT)                                                   
         OI    FLAG1,F1ENABLE      ENABLE OUTPUT                                
*                                                                               
LIMLOOP  GET   LIMITS,GRLEN                                                     
*                                                                               
         LA    R5,GRLINE                                                        
         XR    R6,R6                                                            
         ICM   R6,3,GRLEN                                                       
*                                                                               
         CLI   0(R5),C'*'          COMMENT                                      
         JE    LIMLOOP                                                          
*                                                                               
LIMLOOP1 CLI   3(R5),C'='          VAL=000000                                   
         JNE   LIMLOOP4            NO THEN JUST PASS IT ON                      
*                                                                               
         MVC   WORK1(10),=C'0000000000' DEFAULT FOR ERORRS                      
*                                                                               
         LA    R3,6                6 NUMERIC                                    
         LA    R4,4(R5)            AT 4(R1)                                     
         BRAS  RE,VALNUM                                                        
         CLI   DUB,X'FF'           BAD VALUE                                    
         JE    LIMLOOP3                                                         
*                                                                               
         XR    RE,RE                                                            
         CVB   RF,DUB                                                           
         M     RE,=F'38400'        SECONDS TO TUS                               
         CLC   0(4,R5),=C'SEC='                                                 
         JE    LIMLOOP2            SEC= THEN DONE                               
         CLC   0(4,R5),=C'MIN='                                                 
         JNE   LIMLOOP3            NOT MIN= EITHEER THEN ERR                    
         MHI   RF,60               ANOTHER 60 FOR MINS                          
*                                                                               
LIMLOOP2 EDIT  (RF),(10,WORK1),FILL=0                                           
*                                                                               
LIMLOOP3 MVC   0(10,R5),WORK1                                                   
*                                                                               
LIMLOOP4 LA    R5,11(R5)           NEXT                                         
         SHI   R6,11                                                            
         JP    LIMLOOP1                                                         
*                                                                               
LIMLOOP5 BRAS  RE,PUTLOG                                                        
         J     LIMLOOP                                                          
*                                                                               
LIMITX   NI    FLAG1,255-F1ENABLE  DISABLE FOR 1ST PASS                         
         CLOSE LIMITS              THIS IS TO AVOID PASSING INTIAL              
         J     EXITOK              VALUES WHICH ARE SKEWED BY DOWNTIME          
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM LOOP                                                   *         
***********************************************************************         
MAIN     NTR1  ,                                                                
         LARL  R9,MAIN                                                          
         USING MAIN,R9                                                          
*                                                                               
         CLI   TIMEGRAP,YES        TIME TO OUTPUT FOR GRAPHS                    
         JNE   MAIN010                                                          
         BRAS  RE,GRSTATS          OUTPUT GR STATS                              
         MVI   TIMEGRAP,NO                                                      
         OI    FLAG1,F1ENABLE      ENABLE OUTPUT AFTER FIRST PASS               
*                                                                               
MAIN010  CLI   TIMELOG,YES         TIME TO START NEW TAPE                       
         JNE   MAINX                                                            
         MVI   TIMELOG,NO                                                       
         CLI   TAPE,C'Y'                                                        
         JNE   MAINX                                                            
*                                                                               
         CLOSE GRAPHLOG            REFRESH TAPE                                 
         BRAS  RE,DYNIT                                                         
         OPEN  (GRAPHLOG,OUTPUT)   OPEN GRAPH LOG                               
*                                                                               
MAINX    J     EXITOK                                                           
         DROP  R9                                                               
         EJECT                                                                  
*************************************************************                   
* PUT STATS TO FILE FOR GRAPHIC DISPLAY                                         
*************************************************************                   
         USING GRSTATS,R7                                                       
GRSTATS  LARL  R7,GRSTATS                                                       
GRSTATS0 NTR1                                                                   
         BRAS  RE,ARSOFF                                                        
*                                                                               
*&&UK*&& MVC   HEADER0,=C'UK        ,'                                          
*&&US*&& MVC   HEADER0,=C'US        ,'                                          
*                                                                               
         MVI   OPENFLG,C'Y'                                                     
         L     RF,TIMENOW                                                       
         MHI   RF,384              TIME NOW IN TU                               
         ST    RF,FULL                                                          
         BRAS  RE,TIMEOUT                                                       
         L     RF,FULL                                                          
         MVC   GRLEN,=X'00300000'                                               
         MVC   HEADER1,=C'TIME      ,'                                          
         MVC   HEADER2,SPACES                                                   
         MVC   HEADER2(8),WORK1                                                 
         MVI   HEADER2+10,C','                                                  
         MVC   GRLINE(33),HEADER                                                
         CVDG  GRF,QUAD                                                         
         OI    QUAD+15,X'0F'                                                    
         UNPK  GRLINE+33(10),QUAD(16)                                           
         MVC   GRLINE+44(256-44),SPACES                                         
         BRAS  RE,PUTLOG                                                        
         MVC   GRLINE,SPACES                                                    
*                                                                               
         MVC   GRLEN,=X'00140000'                                               
         MVC   HEADER1,=C'FAC/FAC   ,'                                          
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         XR    R2,R2                                                            
         SAC   512                                                              
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSSTAT      FIND STATS AREA BY ADV                       
         JZ    GREXIT                                                           
         XR    R6,R6                                                            
GRSTA010 SAM31                                                                  
         OC    0(64,R2),0(R2)      ALL ZERO FORGET IT                           
         SAM24                                                                  
         JZ    GRSTA020                                                         
*                                                                               
         LR    RE,R6               WORK OUT ADV NAME AND AOR NUM                
         N     RE,=X'0000000F'                                                  
         SLL   RE,3                INDEX TO NAME IN FACIDTAB                    
         A     RE,AFACITAB                                                      
         MVC   GRLINE+33(4),0(RE)  MOVE TO GRLINE                               
         LR    RE,R6                                                            
         SRL   RE,4                                                             
         LTR   RE,RE                                                            
         JZ    GRSTA011            MUST BE TOR                                  
         LA    RE,X'C0'(RE)                                                     
         CLI   GRLINE+36,C' '                                                   
         JNE   *+12                                                             
         STC   RE,GRLINE+36        MOVE IN AOR LETTER                           
         J     GRSTA011                                                         
         STC   RE,GRLINE+37                                                     
GRSTA011 MVI   GRLINE+43,C','                                                   
*                                                                               
         BRAS  RE,GROUT                                                         
*                                                                               
         SAC   0                                                                
         BRAS  RE,PUTLOG                                                        
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
         SAC   512                                                              
         J     GRSTA021                                                         
*                                                                               
GRSTA020 LA    R2,64(R2)                                                        
GRSTA021 LA    R6,1(R6)                                                         
         CHI   R6,256                                                           
         JL    GRSTA010                                                         
*                                                                               
         MVI   BYTE,0                                                           
GRSTA100 XR    R2,R2                                                            
         ICM   R2,15,TABSSTAT      FIND STATS AREA BY ADV                       
         JZ    GREXIT                                                           
*                                                                               
         MVC   GRLEN,=X'00140000'                                               
GRSTA101 CLI   BYTE,0              SELECT WHICH TABLE                           
         JNE   GRSTA102                                                         
         MVC   HEADER1,=C'FAC/SE    ,'                                          
         A     R2,=A(DSSLOGSE-DSSLOGD)  FAC/SE                                  
         J     GRSTA104                                                         
GRSTA102 CLI   BYTE,1                                                           
         JNE   GRSTA103                                                         
         MVC   HEADER1,=C'SOON/SE   ,'                                          
         A     R2,=A(DSSLOGSO-DSSLOGD)  SOON/SE                                 
         J     GRSTA104                                                         
GRSTA103 CLI   BYTE,2                                                           
         JNE   GRSTA104                                                         
         MVC   HEADER1,=C'RUNNER/SE ,'                                          
         A     R2,=A(DSSLOGRN-DSSLOGD)  RUNNER/SE                               
         J     GRSTA104                                                         
*                                                                               
GRSTA104 SAC   0                                                                
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
         SAC   512                                                              
*                                                                               
         XR    R6,R6                                                            
GRSTA110 SAM31                                                                  
         OC    0(64,R2),0(R2)      IGNORE IF ALL ZERO                           
         SAM24                                                                  
         JZ    GRSTA120                                                         
*                                                                               
         SLL   R6,6                SET SE NAME IN GRLINE                        
         ST    R2,SAVER2                                                        
         LR    R2,R6                                                            
         LAM   AR2,AR2,ALET                                                     
         MVC   GRLINE+33(10),SPACES                                             
         MVC   GRLINE+33(7),DSPNAME-DSPHDR(R2)                                  
         LAM   AR2,AR2,TBLET                                                    
         SRL   R6,6                                                             
         L     R2,SAVER2                                                        
         MVI   GRLINE+43,C','                                                   
*                                                                               
         BRAS  RE,GROUT            PUT OUT STATS NUMBERS                        
*                                                                               
         SAC   0                                                                
         BRAS  RE,PUTLOG           PUT TO FILE                                  
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
         SAC   512                                                              
         J     GRSTA121            NEXT                                         
*                                                                               
GRSTA120 LA    R2,64(R2)                                                        
GRSTA121 LA    R6,1(R6)                                                         
         CHI   R6,256              DO UNTIL SE 256                              
         JL    GRSTA110                                                         
*                                                                               
         LLC   R1,BYTE             NEXT TABLE                                   
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         CLI   BYTE,3                                                           
         JL    GRSTA100                                                         
*                                                                               
         BRAS  RE,ARSOFF                                                        
                                                                                
*************************************************************                   
* DO =RUN STATS FOR GRAPHIC DISPLAY                                             
*************************************************************                   
                                                                                
GRSTA200 MVC   GRLEN,=X'00140000'                                               
         MVC   HEADER1,=C'RUN QUEUE ,'                                          
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
*                                                                               
         BRAS  RE,LOCK             LOCK JOB AND CLASS TABLES                    
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         LAM   AR3,AR3,TBLET                                                    
         SAC   512                                                              
*                                                                               
         LA    R2,DTJOB-X'8000'    JOB TABLE R2                                 
         SLL   R2,6                                                             
*                                                                               
         LA    R3,DTDCLASS-X'8000' CLASS TABLE R3                               
         SLL   R3,6                                                             
         ICM   R3,7,61(R3)                                                      
         JZ    GRSTA299                                                         
*                                                                               
         USING JCLASSD,R3                                                       
GRSTA201 OC    JCCLASS,JCCLASS                                                  
         JZ    GRSTA299                                                         
         MVC   GRLINE+33(5),JCCLASS                                             
         OC    GRLINE+33(5),SPACES                                              
         MVI   GRLINE+43,C','                                                   
         LA    R4,GRLINE+44                                                     
         XR    R1,R1                                                            
         ICM   R1,3,JCNTSUB                                                     
         BRAS  RE,GREDIT                                                        
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,JCNSUB                                                      
         BRAS  RE,GREDIT                                                        
*                                                                               
         XC    CLASSWAI,CLASSWAI                                                
         XC    CLASSCNT,CLASSCNT                                                
*                                                                               
         XR    R1,R1                                                            
         ICM   R2,15,JCFSUB        FIRST SUBMITTED                              
         JZ    GRSTA220                                                         
         MVC   FULL,JCLSUB         A(LAST)                                      
         USING TBJNTRY,R2                                                       
GRSTA210 XR    RF,RF                                                            
         ICM   RF,7,TBJSTIME       SET RF TO STIME                              
         L     RE,TIMENOW          SET RE TO NOW                                
         SR    RE,RF               RE=WAIT TIME IN BIN                          
         A     RE,CLASSWAI                                                      
         ST    RE,CLASSWAI                                                      
         LA    RE,1                BUMP NUMBER FOUND                            
         A     RE,CLASSCNT                                                      
         ST    RE,CLASSCNT                                                      
         ICM   R2,15,TBJNXT        NEXT                                         
         JNZ   GRSTA210                                                         
*                                                                               
         OC    CLASSCNT,CLASSCNT                                                
         JZ    GRSTA219                                                         
         XR    R0,R0               AVG RUN TIME                                 
         LT    R1,CLASSWAI                                                      
         JM    GRSTA219                                                         
         D     R0,CLASSCNT                                                      
         J     GRSTA220                                                         
*                                                                               
GRSTA219 XC    CLASSWAI,CLASSWAI                                                
         XC    CLASSCNT,CLASSCNT                                                
         XR    R1,R1                                                            
*                                                                               
GRSTA220 MHI   R1,384                                                           
         BRAS  RE,GREDIT                                                        
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,JCNRUN                                                      
         BRAS  RE,GREDIT                                                        
*                                                                               
         XC    CLASSRUN,CLASSRUN                                                
         XC    CLASSCNT,CLASSCNT                                                
*                                                                               
         XR    R1,R1                                                            
         ICM   R2,15,JCFRUN        FIRST SUBMITTED                              
         JZ    GRSTA240                                                         
         MVC   FULL,JCLRUN         A(LAST)                                      
         USING TBJNTRY,R2                                                       
GRSTA230 XR    RF,RF                                                            
         ICM   RF,7,TBJRTIME       SET RF TO STIME                              
         L     RE,TIMENOW          SET RE TO NOW                                
         SR    RE,RF               RE=RUN TIME IN BIN                           
         A     RE,CLASSRUN                                                      
         ST    RE,CLASSRUN                                                      
         LA    RE,1                BUMP NUMBER FOUND                            
         A     RE,CLASSCNT                                                      
         ST    RE,CLASSCNT                                                      
         ICM   R2,15,TBJNXT        NEXT                                         
         JNZ   GRSTA230                                                         
*                                                                               
         OC    CLASSCNT,CLASSCNT                                                
         JZ    GRSTA239                                                         
         XR    R0,R0               AVG RUN TIME                                 
         LT    R1,CLASSRUN                                                      
         JM    GRSTA239                                                         
         D     R0,CLASSCNT                                                      
         J     GRSTA240                                                         
*                                                                               
GRSTA239 XC    CLASSRUN,CLASSRUN                                                
         XC    CLASSCNT,CLASSCNT                                                
         XR    R1,R1                                                            
*                                                                               
GRSTA240 MHI   R1,384                                                           
         BRAS  RE,GREDIT                                                        
*                                                                               
         LA    RE,GRLEN                                                         
         SR    R4,RE                                                            
         STCM  R4,3,GRLEN                                                       
         SAC   0                                                                
         BRAS  RE,PUTLOG           PUT TO FILE                                  
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
         SAC   512                                                              
*                                                                               
         LA    R3,JCLASSL(R3)                                                   
         J     GRSTA201                                                         
*                                                                               
GRSTA299 BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLK                                                          
         DROP  R2,R3                                                            
*************************************************************                   
* DO RUNNER QUEUE STATS                                                         
*************************************************************                   
                                                                                
GRSTA300 MVC   GRLEN,=X'00130000'                                               
         MVC   HEADER1,=C'RUNNER QUE,'                                          
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         LAM   AR3,AR3,TBLET                                                    
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         SAC   512                                                              
         XC    RUNSTATS(RUNSTLNQ),RUNSTATS                                      
*                                                                               
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSRUN                                                    
         JZ    *+2                                                              
         USING TABSRUND,R2                                                      
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,TABSNSVR       Number of servers                            
         ICM   R3,15,TABSASVR      A(Server table)                              
         JZ    *+2                                                              
         USING TABSSVRD,R3         BUILD DYNAMIC TYPE TABLE                     
         XC    TYPETAB(TYPETABL),TYPETAB                                        
*                                                                               
GRSTA301 LA    R1,TYPETAB          Type table                                   
         USING RTYPED,R1                                                        
*                                                                               
GRSTA302 CLI   TSTYPE,0            EMPTY ENTRY in data space                    
         JE    GRSTA309            Yes                                          
         CLC   TSTYPE,RTYPE        Already in internal table?                   
         JE    GRSTA308            Yes                                          
         CLI   RTYPE,0             EOT                                          
         JE    GRSTA303            Yes, add entry                               
         LA    R1,L'TYPETAB(R1)    NEXT                                         
         LA    RE,TYPETABX                                                      
         CR    R1,RE                                                            
         JL    GRSTA302                                                         
         DC    H'0'                                                             
*                                                                               
GRSTA303 MVC   RTYPE,TSTYPE        Save into local table type                   
         MVC   RTYPJOB,TSJOB       Save job name into local table               
         J     GRSTA309                                                         
*                                  Multiple Runners for type                    
GRSTA308 LA    RE,L'RTYPJOB        C JOBNAME1                                   
         LA    RF,RTYPJOB+7                                                     
         CLI   0(RF),C' '          REMOVE LAST CHR FROM JOBNAME IF DUPS         
         JNE   *+12                                                             
         AHI   RF,-1                                                            
         JCT   RE,*-12                                                          
         MVI   0(RF),C'+'          REPLACE WITH + TO SHOW MORE RUNNERS          
*                                                                               
GRSTA309 AHI   R3,TABSSVRL         Next server in data space                    
         JCT   R0,GRSTA301                                                      
*                                                                               
         ICM   R3,15,TABSAQUE      A(Runner request queue)                      
         JZ    *+2                                                              
         XR    R4,R4                                                            
         ICM   R4,3,TABSNQUE       Max queues entries                           
*                                                                               
         USING TABSQUED,R3                                                      
         CLI   TQSTATUS,TQSWAIT                                                 
         JNE   GRSTA350            FIND IN RUNSTATS                             
GRSTA310 LA    R1,RNSMAXQ          Max type entries                             
         LA    RE,RUNSTATS                                                      
         USING RUNSTATD,RE                                                      
*                                                                               
GRSTA320 CLC   TQTYPE,RNSTYPE      Found active entry in queue                  
         JE    GRSTA330                                                         
         CLI   RNSTYPE,0           OR NEW                                       
         JE    GRSTA330                                                         
         AHI   RE,RNSLNQ           Next entry in local table                    
         JCT   R1,GRSTA320                                                      
         DC    H'0'                OVERFLOW RUNSTATS                            
*                                                                               
GRSTA330 MVC   RNSTYPE,TQTYPE      Set type                                     
         OC    TQATIME,TQATIME     Do we have an arrival time                   
         JZ    GRSTA331                                                         
         L     RF,TIMENOW                                                       
         MHI   RF,384              TIME NOW IN TU                               
         S     RF,TQATIME          Arrivial time                                
         JP    *+6                                                              
GRSTA331 XR    RF,RF               If negative or n/a assume zero               
         A     RF,RNSWAIT          Add total wait time                          
         ST    RF,RNSWAIT          Store new total                              
         ICM   RF,3,RNSCOUNT                                                    
         LA    RF,1(RF)            BUMP NUMBER                                  
         STCM  RF,3,RNSCOUNT                                                    
*                                                                               
GRSTA350 LA    R3,TABSQUEL(R3)                                                  
         JCT   R4,GRSTA310         NEXT QUEUE ENTRY                             
         DROP  RE                                                               
                                                                                
         BRAS  RE,ARSOFF                                                        
*                                                                               
         LA    R3,RUNSTATS                                                      
         USING RUNSTATD,R3                                                      
GRSTA360 CLI   RNSTYPE,0                                                        
         JE    GRSTA390                                                         
         MVC   GRLINE+33(1),RNSTYPE  QUEUE TYPE                                 
         LA    R1,TYPETAB                                                       
GRSTA362 CLC   RTYPE,RNSTYPE                                                    
         JE    GRSTA365                                                         
         LA    R1,L'TYPETAB(R1)                                                 
         CLI   0(R1),C' '                                                       
         JNE   GRSTA362                                                         
         J     *+10                                                             
GRSTA365 MVC   GRLINE+33(10),SPACES                                             
         MVC   GRLINE+33(8),RTYPJOB                                             
         MVI   GRLINE+43,C','                                                   
         LA    R4,GRLINE+44                                                     
         XR    R1,R1                                                            
         ICM   R1,3,RNSCOUNT       NUMBER IN QUEUE                              
         BRAS  RE,GREDIT                                                        
*                                                                               
         XR    R0,R0                                                            
         ICM   R1,15,RNSWAIT       TOTAL WAIT TIME                              
         XR    RE,RE                                                            
         ICM   RE,3,RNSCOUNT       NUMBER IN QUEUE                              
         DR    R0,RE               AVG WAIT TIME                                
         BRAS  RE,GREDIT                                                        
*                                                                               
         LA    RE,GRLEN                                                         
         SR    R4,RE                                                            
         STCM  R4,3,GRLEN                                                       
         SAC   0                                                                
         BRAS  RE,PUTLOG           PUT TO FILE                                  
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
         SAC   512                                                              
*                                                                               
         LA    R3,RNSLNQ(R3)                                                    
         J     GRSTA360                                                         
*                                                                               
GRSTA390 BRAS  RE,ARSOFF                                                        
         DROP  R3                                                               
*                                                                               
*************************************************************                   
* DO USS QUEUE STATS                                                            
*************************************************************                   
                                                                                
GRSTA400 MVC   GRLEN,=X'00140000'                                               
         MVC   HEADER1,=C'USS QUEUE ,'                                          
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         LA    R3,1                                                             
GRSTA410 LR    R2,R3                                                            
         SLL   R2,6                                                             
*                                                                               
         LA    R5,USSQEXCL         HAS IT BEEN EXCLUDED                         
         AR    R5,R3                                                            
         CLI   0(R5),C'X'                                                       
         JE    GRSTA450                                                         
*                                                                               
         SAC   512                                                              
         MVC   GRLINE+33(8),16(R2)                                              
         MVI   GRLINE+43,C','                                                   
*                                                                               
         XC    UNXQKEY,UNXQKEY                                                  
         MVI   UNXQTYPE,UNXQTY00                                                
         L     R1,=A(SSB)                                                       
         MVC   UNXQDSPC,SSODSPAC-SSOOFF(R1)                                     
         STCM  R3,3,UNXQSE#                                                     
         SAC   0                                                                
         GOTO1 =V(USSQINQ),DMCB,UNXQKEY                                         
         JNE   GRSTA440            QUEUE NOT FOUND                              
         CLI   DMCB+12,3                                                        
         JE    GRSTA440                                                         
*                                                                               
         L     R1,DMCB+4                                                        
         LA    R4,GRLINE+44                                                     
         BRAS  RE,GREDIT                                                        
*                                                                               
         LA    RE,GRLEN                                                         
         SR    R4,RE                                                            
         STCM  R4,3,GRLEN                                                       
         SAC   0                                                                
         BRAS  RE,PUTLOG           PUT TO FILE                                  
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
         J     GRSTA450                                                         
*                                                                               
GRSTA440 MVI   0(R5),C'X'          EXCLUDE ERRORS                               
*                                                                               
GRSTA450 LA    R3,1(R3)                                                         
         CHI   R3,256                                                           
         JL    GRSTA410                                                         
                                                                                
*************************************************************                   
* DO SE QUEUE STATS                                                             
*************************************************************                   
                                                                                
GRSTA500 MVC   GRLEN,=X'00140000'                                               
         MVC   HEADER1,=C'SE QUEUES ,'                                          
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         LAM   AR3,AR3,ALET                                                     
         SAC   512                                                              
         XR    R2,R2               FIND STATS AREA BY ADV                       
         ICM   R2,15,TABSSTAT-TABSADDR(R2)                                      
         JZ    GREXIT                                                           
         A     R2,=A(DSSLOGQL-DSSLOGD)  SE QUEUE                                
*                                                                               
         XR    R6,R6                                                            
GRSTA510 SAM31                                                                  
         OC    0(4,R2),0(R2)       IGNORE IF ZERO                               
         JZ    GRSTA520                                                         
*                                                                               
         SLL   R6,6                SET SE NAME IN GRLINE                        
         LR    R3,R6                                                            
         MVC   GRLINE+33(10),SPACES                                             
         MVC   GRLINE+33(7),DSPNAME-DSPHDR(R3)                                  
         SRL   R6,6                                                             
*                                                                               
         MVI   GRLINE+43,C','                                                   
         LA    R4,GRLINE+44                                                     
         XR    R1,R1                                                            
         ICM   R1,3,0(R2)                                                       
         BRAS  RE,GREDIT           PUT OUT QUEUE                                
         ICM   R1,3,2(R2)                                                       
         BRAS  RE,GREDIT           PUT OUT LONGEST QUEUE                        
         MVC   0(4,R2),=F'0'       RESET QUEUE COUNT                            
*                                                                               
         LA    RE,GRLEN                                                         
         SR    R4,RE                                                            
         STCM  R4,3,GRLEN                                                       
         SAC   0                                                                
         BRAS  RE,PUTLOG           PUT TO FILE                                  
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
         SAC   512                                                              
*                                                                               
GRSTA520 LA    R2,4(R2)                                                         
         LA    R6,1(R6)                                                         
         CHI   R6,256              DO UNTIL SE 256                              
         JL    GRSTA510                                                         
         BRAS  RE,ARSOFF                                                        
         SAM24                                                                  
                                                                                
*************************************************************                   
* DO CONTENTION                                                                 
*************************************************************                   
                                                                                
GRSTA600 MVC   GRLEN,=X'00140000'                                               
         MVC   HEADER1,=C'CONTENTION,'                                          
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
*                                                                               
         XR    R2,R2                                                            
         LAM   AR2,AR2,ALET                                                     
         LAM   AR3,AR3,ALET                                                     
*                                                                               
         SAC   512                                                              
*                                                                               
         LA    R6,1                START AT SE 1                                
GRSTA610 LR    R2,R6                                                            
         SLL   R2,6                INDEX SE*64                                  
         USING DMSPACED,R2                                                      
         XR    R3,R3                                                            
         ICM   R3,7,DSPECB+1       ANY SYSTEM BLOCK                             
         JZ    GRSTA650                                                         
*                                                                               
         USING DMSYSHDR,R3                                                      
         ICM   R3,15,DSYALOCK      POINT R3 TO LOCKTAB                          
*                                                                               
         LR    RF,R6               SET RF TO INDEX INTO CONTENT                 
         SLL   RF,5                                                             
         A     RF,=A(CONTENT-WORKD)                                             
         AR    RF,RC                                                            
*                                                                               
         MVC   GRLINE+33(10),SPACES   SET SE NAME                               
         MVC   GRLINE+33(7),DSPNAME                                             
         MVI   GRLINE+43,C','                                                   
         LA    R4,GRLINE+44                                                     
*                                                                               
         XR    R5,R5                                                            
         L     R1,DSPLCNT          ONLINE LOCK COUNT                            
         S     R1,0(RF)                                                         
         AR    R5,R1                                                            
         BRAS  RE,GREDIT                                                        
         MVC   0(4,RF),DSPLCNT                                                  
*                                                                               
         L     R1,DSPWCNT          ONLINE WAIT COUNT                            
         S     R1,4(RF)                                                         
         AR    R5,R1                                                            
         BRAS  RE,GREDIT                                                        
         MVC   4(4,RF),DSPWCNT                                                  
*                                                                               
         L     R1,DSPLCNTO         OFFLINE LOCK COUNT                           
         S     R1,8(RF)                                                         
         AR    R5,R1                                                            
         BRAS  RE,GREDIT                                                        
         MVC   8(4,RF),DSPLCNTO                                                 
*                                                                               
         L     R1,DSPWCNTO         OFFLINE WAIT COUNT                           
         S     R1,12(RF)                                                        
         AR    R5,R1                                                            
         BRAS  RE,GREDIT                                                        
         MVC   12(4,RF),DSPWCNTO                                                
*                                                                               
         L     R1,12(,R3)          DMGR LOCKS                                   
         S     R1,16(RF)                                                        
         AR    R5,R1                                                            
         BRAS  RE,GREDIT                                                        
         MVC   16(4,RF),12(R3)                                                  
*                                                                               
         L     R1,8(,R3)           DMGR WAITS                                   
         S     R1,20(RF)                                                        
         AR    R5,R1                                                            
         BRAS  RE,GREDIT                                                        
         MVC   20(4,RF),8(R3)                                                   
*                                                                               
         LTR   R5,R5               IF ALL WERE ZERO                             
         JZ    GRSTA650            DON'T PUT                                    
*                                                                               
         LA    RE,GRLEN                                                         
         SR    R4,RE                                                            
         STCM  R4,3,GRLEN                                                       
         SAC   0                                                                
         BRAS  RE,PUTLOG           PUT TO FILE                                  
         MVC   GRLINE,SPACES                                                    
         MVC   GRLINE(33),HEADER                                                
         SAC   512                                                              
*                                                                               
GRSTA650 LA    R6,1(R6)                                                         
         CHI   R6,256              DO UNTIL SE 256                              
         JL    GRSTA610                                                         
*                                                                               
         BRAS  RE,ARSOFF                                                        
                                                                                
         MVC   GRLEN,=X'00300000'                                               
         MVC   HEADER1,=C'END       ,'                                          
         MVC   GRLINE(33),HEADER                                                
         L     RF,TIMENOW                                                       
         MHI   RF,384              TIME NOW IN TU                               
         CVDG  GRF,QUAD                                                         
         OI    QUAD+15,X'0F'                                                    
         UNPK  GRLINE+33(10),QUAD(16)                                           
         MVC   GRLINE+43(256-43),SPACES                                         
         BRAS  RE,PUTLOG                                                        
         MVC   GRLINE,SPACES                                                    
                                                                                
         J     GREXIT                                                           
*************************************************************                   
* GENERAL ROUTINES FOR GRSTATS                                                  
*************************************************************                   
*                                                                               
GREXIT   BRAS  RE,ARSOFF                                                        
         XIT1                                                                   
*                                                                               
GRAFULL  MVI   GRAPHFUL,C'Y'       SET FILE TO FULL AND TURN OFF TAPE           
         MVC   TAPE,=C'NO '                                                     
         J     GREXIT                                                           
*                                                                               
GREDIT   NG    GR1,=X'00000000FFFFFFFF'  REMOVE ANY HIGH BITS                   
         CVDG  GR1,QUAD                                                         
         OI    QUAD+15,X'0F'                                                    
         UNPK  0(10,R4),QUAD(16)                                                
*OLD                               EDIT  (R1),(10,0(R4)),ZERO=NOBLANK           
         LA    R0,10               FORCE OR USE ALIGN=LEFT                      
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         BR    RE                                                               
*                                                                               
GROUT    ST    RE,SAVERE           WRITE OUT 16 COUNTERS TO GRLINE              
         SAM31                                                                  
         LA    RE,16                                                            
         LA    RF,GRLINE+44                                                     
GROUT015 LAM   ARE,AR1,ARZERO                                                   
*        EDIT  (4,0(R2)),(10,0(RF)),ZERO=NOBLANK                                
         L     R1,0(,R2)                                                        
         CVDG  GR1,QUAD                                                         
         OI    QUAD+15,X'0F'                                                    
         UNPK  0(10,RF),QUAD(16)                                                
         MVC   0(4,R2),=F'0'       RESET COUNT                                  
         LA    R2,4(R2)                                                         
         LA    R0,10               FORCE OR USE ALIGN=LEFT                      
         AR    RF,R0                                                            
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         JCT   RE,GROUT015         NEXT                                         
         LA    RE,GRLEN                                                         
         SR    RF,RE                                                            
         STCM  RF,3,GRLEN                                                       
         L     RE,SAVERE                                                        
         SAM24                                                                  
         BR    RE                                                               
*                                                                               
GRAX     DC    H'0'                                                             
GRAPHFUL DC    C'N'                                                             
*                                                                               
PUTLOG   NTR1                                                                   
         LARL  R7,PUTLOG                                                        
         USING PUTLOG,R7                                                        
         TM    FLAG1,F1ENABLE                                                   
         JZ    PUTLOGX                                                          
         CLI   TAPE,C'Y'                                                        
         JNE   PUTLOG01                                                         
         PUT   GRAPHLOG,GRAREA                                                  
PUTLOG01 LA    R8,GRLINE                * DATA TO SEND                          
         ICM   R9,15,GRLEN              * LENGTH TO SEND                        
         SRL   R9,16                                                            
         SHI   R9,4                                                             
*                                                                               
         CLI   ECMDO,C'Y'                                                       
         JNE   PUTLOGX                                                          
*                                                                               
         SAM31                                                                  
         ECIXAPI SEND,DATA=(R8),LEN=(R9),APPNAME=APPNAME,              X        
               HANDLE=HANDLE,MF=(E,PLIST)                                       
         SAM24                                                                  
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
PUTLOGX  LAM   ARE,AR1,ARZERO                                                   
         XIT1                                                                   
APPNAME  DC    CL08'MOCEAN  '                                                   
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* DYNAMICALLY ALLOCATE GRAPHLOG TAPE                                            
***********************************************************************         
DYNIT    NTR1  ,                                                                
         LARL  R7,DYNIT                                                         
         USING DYNIT,R7                                                         
*                                                                               
         MVC   DSNDSPC,SSB+SSODSPAC-SSOOFF             DATA SPACE               
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(X'20',DSNDATE)   DATE                     
         EDIT  (TIME,NOW),DUB1                         TIME                     
         MVC   DSNHOUR,DUB1                                                     
         MVC   DSNMINS,DUB1+3                                                   
         MVC   DSNSECS,DUB1+6                                                   
*                                                                               
         MVI   DYNTAPE,C'Y'        POSIT TAPEOUT DYNAMICALLY ALLOCATED          
         LA    R1,ARBLK            DYNAMIC ALLOCATION PARAMETER BLOCK           
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         JZ    DIT002                                                           
         CLC   RBLK+4(2),=X'0410'  TEST IF DD CARD EXISTS                       
         JNE   DIT010              NO, ERROR CREATING DATA SET                  
         MVI   DYNTAPE,C'N'        TAPEOUT NOT DYNAMICALLY ALLOCATED            
*                                                                               
DIT002   RDJFCB GRAPHLOG                                                        
         LTR   RF,RF                                                            
         JNZ   DIT010                                                           
*                                                                               
         LA    R3,JFCB                                                          
         USING INFMJFCB,R3                                                      
         CLI   DYNTAPE,C'N'        TAPEOUT DYNAMICALLY ALLOCATED                
         JNE   DIT003                                                           
         MVC   DSNAME,JFCBDSNM     NO, COPY OVERRIDE DSN                        
         J     EXIT                                                             
*                                                                               
DIT003   MVC   JFCBXPDT,JFCBCRDT   COPY CREATION DATE TO EXPIRATION             
*                                                                               
DIT005   XR    R1,R1               5 DAYS TO EXPIRY                             
         ICM   R1,B'0011',JFCBXPDT+1                                            
         AHI   R1,5                                                             
         CHI   R1,365              IF > 365 REMOVE 365 DAYS                     
         JNH   DIT006                                                           
         AHI   R1,-365                                                          
         LLC   RF,JFCBXPDT         AND ADD 1 YEAR                               
         AHI   RF,1                                                             
         STC   RF,JFCBXPDT                                                      
*                                                                               
DIT006   STCM  R1,B'0011',JFCBXPDT+1                                            
         J     EXIT                                                             
         DROP  R3                                                               
*                                                                               
DIT010   MVC   PLINE,SPACES                                                     
         MVC   PLINE(2),=AL2(78)                                                
         MVC   PLINE+2(13),=C'<<< ERROR >>>'                                    
         MVC   PLINE+20(22),=C'DYNALLOC ERROR CODE = '                          
         GOTO1 =V(HEXOUT),DMCB,RBLK+4,PLINE+42,2,=C'TOG'                        
         MVC   PLINE+46(15),=C',  INFO CODE = '                                 
         GOTO1 =V(HEXOUT),DMCB,RBLK+6,PLINE+61,2,=C'TOG'                        
*                                                                               
         LA    R3,PLINE                                                         
         WTO   TEXT=(R3)                                                        
         DC    H'0'                COULD NOT ALLOCATE TAPE                      
                                                                                
*----------------------------------------------------------------------         
* DYNALLOC REQUEST BLOCK                                                        
*----------------------------------------------------------------------         
         DS    0F                                                               
ARBLK    DC    X'80',AL3(RBLK)     R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLK     DC    X'1401000000000000',A(ATXT),X'0000000000000000'                  
*                                                                               
ATXT     DC    X'00',AL3(TXTDD)                                                 
         DC    X'00',AL3(TXTDSN)                                                
         DC    X'00',AL3(TXTUNIT)                                               
         DC    X'00',AL3(TXTSDISP)                                              
         DC    X'00',AL3(TXTNDISP)                                              
         DC    X'00',AL3(TXTCDISP)                                              
         DC    X'80',AL3(TXTCLOSE)                                              
*                                                                               
TXTDD    DC    AL2(DALDDNAM),X'00010008',CL8'GRAPHLOG' DDNAME=TAPEOUT           
TXTDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
*&&UK                                                                           
TXTUNIT  DC    AL2(DALUNIT),X'00010004',CL4'CART'      UNIT=CART  UK            
*&&                                                                             
*&&US                                                                           
TXTUNIT  DC    AL2(DALUNIT),X'00010003',CL4'VTS'       UNIT=VTS   US            
*&&                                                                             
TXTSDISP DC    AL2(DALSTATS),X'00010001',X'04'         DISP=(NEW,     )         
TXTNDISP DC    AL2(DALNDISP),X'00010001',X'02'              ( ,CATLG, )         
TXTCDISP DC    AL2(DALCDISP),X'00010001',X'02'              (   ,CATLG)         
TXTCLOSE DC    AL2(DALCLOSE),X'0000'                   FREE=CLOSE               
*                                                                               
         ORG   TXTDSN+6            BACK UP AND DEFINE DATA SET NAME             
DSNAME   DS    0CL44                                                            
         DC    C'FACTAPE.'         FACTAPE.                                     
         DC    C'RSMFEED.'         RSMFEED                                      
DSNDSPC  DC    C'X'                DATA SPACE                                   
         DC    C'.D'               .D = DATE                                    
DSNDATE  DC    C'YYMMDD'           YEAR/MONTH/DAY                               
         DC    C'.T'               .T = TIME                                    
DSNHOUR  DC    C'HH'               HOURS                                        
DSNMINS  DC    C'MM'               MINUTES                                      
DSNSECS  DC    C'SS'               SECONDS                                      
         ORG                                                                    
         DROP  R7                                                               
         EJECT                                                                  
***************************************************                             
*        EDIT TUS IN FULL TO WORK HH:MM:SS.SS     *                             
***************************************************                             
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(13),=C'00:00:00.0000'                                      
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'138240000'        60*60*38400                              
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'2304000'          60*38400                                 
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'38400'            38400                                    
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'384'              384                                      
         EDIT  (RF),(2,WORK1+9),FILL=0    100/SEC                               
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'3'                3                                        
         EDIT  (RF),(2,WORK1+11),FILL=0    100/SEC                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        SET UP OPERATOR COMMS                              *                   
*************************************************************                   
SETOPS   LARL  R7,SETOPS                                                        
         USING SETOPS,R7                                                        
         ST    RE,SAVERE                                                        
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         JNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* SETWAIT - SET TIMER AND WAIT 2 MINUTES                              *         
* RETURN WHEN AN INTERUPT IS DETECTED                                 *         
***********************************************************************         
SETWAIT  NTR1  ,                                                                
         LARL  R9,SETWAIT                                                       
         USING SETWAIT,R9                                                       
*                                                                               
         SR    R0,R0                                                            
         LHI   R1,10               1/10 SECOND ONLY                             
         ST    R1,TIME                                                          
         XC    TIMRECB2,TIMRECB2                                                
         STIMERM SET,ID=STIMERID,BINTVL=TIME,EXIT=TIMRXIT2                      
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         JZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         WAIT  1,ECBLIST=ECBLST2   WAIT FOR TIMER POP OR OPERATOR               
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         JO    WB10                                                             
         TM    DSPACECB,X'40'      OR DATASPACE COMMAND                         
         JNO   WB15                NO                                           
*                                                                               
WB10     STIMERM CANCEL,ID=STIMERID  YES, SO CANCEL THE TIMER                   
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL TIMER CANCEL                    
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         JZ    WB15                                                             
         WTO   'OPER INTERRUPT',MCSFLAG=HRDCPY                                  
*                                                                               
         BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
*                                                                               
WB14X    J     SETWAITX                                                         
*                                                                               
WB15     TM    TIMRECB2,X'40'      DID THE TIMER POP?                           
         JO    SETWAITX            YES                                          
*                                                                               
SETWAITX MVC   TIMEOLD,TIMENOW     SET THE PREVIOUS TIME                        
         MVC   DATEOLD,DATENOW                                                  
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R1,DATENOW                                                       
         ST    R0,TIMENOW                                                       
*                                                                               
         CLC   DATEOLD,DATENOW     HAS DATE CHANGED                             
         JE    EXITOK                                                           
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(0,DUB)                                    
         GOTO1 =V(GETDAY),DMCB,(0,DUB),FULL                                     
         MVC   DAYNOW,0(R1)                                                     
         GOTO1 =V(DATCON),DMCB,(5,0),(1,DUB)                                    
*NOP     MVC   DATNOW(1),DUB+2                                                  
*NOP     MVC   MONNOW(1),DUB+1                                                  
         J     EXITOK                                                           
         DROP  R9                                                               
         EJECT                                                                  
*************************************************************                   
*        CHECK OPER INTERRUPT                               *                   
*************************************************************                   
CHKOPER  NTR1                                                                   
*                                                                               
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         JNE   CHEK010                                                          
         J     ENDOFJOB                                                         
*                                                                               
CHEK010  CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         JE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
*                                                                               
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   CARD(0),CIBDATA                                                  
*                                                                               
         LA    R1,CARD                                                          
         BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
         JE    CHEK020             NEQ SO TRY GROUP COMMAND                     
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         MVC   PLINE+16(13),CARD                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         J     CHEKRSET                                                         
*                                                                               
CHEK020  MVC   PLINE(32),=C'             //'                                    
         MVC   PLINE(13),CARD                                                   
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
*                                                                               
         LH    RF,POPTIME          Poptime in seconds - reload                  
         M     RE,=AL4(100)                                                     
         STCM  RF,7,TIMEGRAP+1                                                  
         LH    RF,LOGTIME          Logtime in hours - reload                    
         M     RE,=AL4(60*60*100)                                               
         STCM  RF,7,TIMELOG+1                                                   
*                                                                               
CHEKRSET L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         JZ    EXITOK                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  RF                                                               
         J     EXITOK                                                           
         EJECT                                                                  
*************************************************************                   
*        END OF JOB                                         *                   
*************************************************************                   
ENDOFJOB LARL  R9,ENDOFJOB                                                      
         USING ENDOFJOB,R9                                                      
         BRAS  RE,PRINTX                                                        
         WTO   'END OF JOB ',MCSFLAG=HRDCPY                                     
*                                                                               
         CLI   OPENFLG,C'Y'                                                     
         JNE   ENDOF001                                                         
         CLI   TAPE,C'Y'                                                        
         JNE   ENDOF000                                                         
         CLOSE GRAPHLOG            CLOSE IF OPEN                                
*                                                                               
ENDOF000 CLI   ECMDO,C'Y'                                                       
         JNE   ENDOF001                                                         
         SAM31                                                                  
         ECIXAPI TERM,HANDLE=HANDLE,MF=(E,PLIST)                                
         SAM24                                                                  
*                                                                               
ENDOF001 CLI   LOCKFLG,C'Y'                                                     
         JNE   ENDOF002                                                         
         BRAS  RE,UNLK             UNLOCK IF LOCKED                             
*                                                                               
ENDOF002 EQU   *                                                                
*                                                                               
ENDOFXXX J     XBASE                                                            
         DROP  R9                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         IEABRCX ENABLE                                                         
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         JL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,T1         PRINT TITLE                                  
         PUT   SYSPRINT,T1                                                      
         PUT   SYSPRINT,T1                                                      
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         IEABRCX DISABLE                                                        
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*        X'0080'                   NO DELIMETER KEYWORD ONLY                    
*                                                                               
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,09),X'0000',AL3(DDSIO)                          
         DC    C'TAPE   ',AL1(3,09),X'0000',AL3(TAPE)                           
         DC    C'ECMDO  ',AL1(4,09),X'0000',AL3(ECMDO)                          
         DC    C'DSPACE ',AL1(5,00),X'0000',AL3(SSB+SSODSPAC-SSOOFF)            
         DC    C'ESTAE  ',AL1(4,09),X'0000',AL3(ESTAESW)                        
         DC    C'LOGTIME',AL1(6,02),X'0800',AL3(LOGTIME)                        
         DC    C'POPTIME',AL1(6,02),X'0800',AL3(POPTIME)                        
         DC    C'PUTLOG ',AL1(5,01),X'8080',AL3(POPNOW)                         
         DC    X'0000'                                                          
         BR    RE                                                               
***************************************************                             
*        CARD OUTPUT AREAS SET WITH DEFAULTS      *                             
***************************************************                             
$$DATA   LOCTR ,                                                                
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
DDSIO    DC    CL10'DDSIO'                                                      
TAPE     DC    CL10'NO  '          DEFAULT TO TAPE=NO                           
ECMDO    DC    CL10'YES '          DEFAULT TO ECMDO=YES                         
ESTAESW  DC    CL10'YES '                                                       
LOGTIME  DC    H'8'                DEFAULT 8 HOURS                              
POPTIME  DC    H'60'               DEFAULT 60 SECONDS                           
$$CODE   LOCTR ,                                                                
*                                                                               
         EJECT                                                                  
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         JE    EXITOK                                                           
*                                                                               
VALC001  LARL  R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EXRL  R1,*+10             EXECUTE KEYWORD TEST                         
         J     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         JE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         JNE   VALC010                                                          
         J     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         TM    10(R4),X'80'        IF NO DELIMENTER JUST DO IT                  
         JO    VALC025                                                          
*                                                                               
         LARL  RF,VALCDELS         DELIMITER TABLE                              
         J     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         JE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         JE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         JZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         JNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         J     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         JE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         JE    VALC030                                                          
         CLI   0(R1),0                                                          
         JE    VALC030                                                          
         LA    R1,1(R1)                                                         
         J     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         JZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         J     VALC500                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         JNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         JE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         JE    VALC050                                                          
         CLC   0(2,RF),=C'  '      EMPTY ENTRY                                  
         JE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         JZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         J     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         JZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         JNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         JZ    CERRHEX                                                          
         J     VALC500                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         JZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BRAS  RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         JE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         J     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         JZ    VALC080                                                          
         BRAS  RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         J     VALC500                                                          
*                                                                               
VALC080  TM    9(R4),X'01'         DATE INPUT                                   
         JZ    VALC400                                                          
         LA    R0,1(R1)            SET R0 INPUT LEN                             
         ST    RF,FULL                                                          
         GOTO1 =V(PERVAL),DMCB,((R0),(R2)),(X'60',WORK)                         
         L     RF,FULL                                                          
         CLI   4(R1),X'04'                                                      
         JNE   CERRDAT                                                          
         MVC   0(2,RF),WORK+PVALCSTA-PERVALD                                    
         J     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         JE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         JNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            PAD OUT TO SPACES                            
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
VALC410  EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         JE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         JL    VALC500                                                          
         J     EXITOK                                                           
         EJECT                                                                  
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         J     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         J     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         J     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         J     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         J     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         J     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         J     CERRX                                                            
CERRDAT  LA    R1,=C'INVALID DATE    '                                          
         J     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SYSTEM  '                                          
         J     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         JE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         JE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         J     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BRAS  RE,PRINTL                                                        
         J     EXITL                                                            
         EJECT                                                                  
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
         SPACE 1                                                                
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         JNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         J     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BRAS  RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BRAS  RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BRAS  RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         JNE   EXITOK                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         JNE   EXITOK                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BRAS  RE,VALTADD                                                       
         J     EXITOK                                                           
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         JE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
*                                                                               
VALNUM   NTR1                                                                   
         LR    R5,R3              SAVE LENGTH OF FIELD                          
         LR    R6,R4              SAVE ADDRESS OF FIELD                         
VALN2    TM    0(R4),X'F0'        NUMERIC ?                                     
         JO    VALN4              VALID                                         
         CLI   0(R4),X'40'        SPACE ?                                       
         JNE   VALN6              INVALID ZONE                                  
         OI    0(R4),X'F0'        SET TO VALID ZONE                             
VALN4    LA    R4,1(R4)           BUMP TO NEXT BYTE                             
         JCT   R3,VALN2           LOOP TO LENGTH                                
         BCTR  R5,R0              LENGTH-1 FOR EXECUTE                          
         EXRL  R5,*+10            CONVERT TO PACKED (LENGTH MODIFIED)           
         J     VALXIT             VALID EXIT                                    
         PACK  DUB,0(0,R6)        ZONED TO PACKED                               
VALN6    MVI   DUB,X'FF'          SET ON ERROR CONDITION                        
VALXIT   J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FORCE WRITE OF LOG TAPE                                             *         
***********************************************************************         
POPNOW   NTR1                                                                   
         MVI   TIMELOG,C'*'                                                     
         J     EXITOK                                                           
***********************************************************************         
* LOCK TABLES                                                         *         
***********************************************************************         
LOCK     NTR1  ,                                                                
         BRAS  RE,LOCKCLS          LOCK CLASS TABLE                             
         BRAS  RE,LOCKJOB          LOCK JOBTAB                                  
         BRAS  RE,ARSOFF                                                        
         MVI   LOCKFLG,C'Y'                                                     
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE TABLES                                                         *         
***********************************************************************         
UNLK     NTR1  ,                                                                
         BRAS  RE,FREEJOB          UNLK JOBTAB                                  
         BRAS  RE,FREECLS          UNLK CLASS TABLE                             
         BRAS  RE,ARSOFF                                                        
         MVI   LOCKFLG,C'N'                                                     
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ENQUIRE ON JOB TABLE TO ALLOCATE TABS                               *         
***********************************************************************         
ENQJOB   NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'20'           SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)                                                      
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ENQUIRE ON ZERO TO ALLOCATE DMGR                                    *         
***********************************************************************         
ENQDMGR  NTR1  ,                                                                
         XC    DUB,DUB                                                          
         MVI   DUB,X'20'           SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)                                                      
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOCK JOB TABLE                                                      *         
***********************************************************************         
LOCKJOB  NTR1  ,                   GET JOB TABLE HEADER INTO DSHDR              
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'80'           SET LOCK                                     
         GOTO1 ALOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)                                                      
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOCK CLASS TABLE                                                    *         
***********************************************************************         
LOCKCLS  NTR1  ,                   LOCK CLASS TABLE                             
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         MVI   DUB,X'80'           SET LOCK                                     
         GOTO1 ALOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)                                                      
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE JOB TABLE                                                      *         
***********************************************************************         
FREEJOB  NTR1  ,                   FREE JOB TABLE                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTJOB)                                               
         MVI   DUB,X'10'           SET UNLOCK                                   
         GOTO1 ALOCKSPC,DUB                                                     
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE CLASS TABLE                                                    *         
***********************************************************************         
FREECLS  NTR1  ,                   FREE CLASS TABLE                             
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTDCLASS)                                            
         MVI   DUB,X'10'           SET UNLOCK                                   
         GOTO1 ALOCKSPC,DUB                                                     
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE TO CONSOLE                                         *         
* NTRY   R0 = A(MESSAGE HEADER TO WRITE)                              *         
***********************************************************************         
WTO      NTR1  ,                                                                
         WTO   TEXT=(R0),MCSFLAG=HRDCPY                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE                                                          *         
***********************************************************************         
         IEABRCX ENABLE                                                         
INIT     NTR1  ,                                                                
         LARL  R9,INIT                                                          
         USING INIT,R9                                                          
         WTO   'INIT ',MCSFLAG=HRDCPY                                           
*                                                                               
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         MVC   MYASID,FULL+2                                                    
*                                                                               
         LA    R3,CARD                                                          
INIT02   GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         JE    INIT04                                                           
*                                                                               
         L     RF,=V(DDSIO)        SET UP DDSIO FIRST                           
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
         MVC   PLINE+1(80),0(R3)                                                
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
INIT03   LR    R1,R3               PASS TO VALCARD                              
         BRAS  RE,VALCARD          READ KEYWORD=VALUES                          
         JE    INIT02                                                           
         J     XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
INIT04   BRAS  RE,ENQJOB                                                        
         BRAS  RE,ENQDMGR                                                       
         L     RF,=A(SSB)                                                       
         MVC   ALET,SSOALET-SSOOFF(RF)                                          
         MVC   TBLET,SSOTBLET-SSOOFF(RF)                                        
         MVC   DSTYP,SSODSPAC-SSOOFF(RF)                                        
         OC    ALET,ALET                                                        
         JZ    *+2                                                              
         OC    TBLET,TBLET                                                      
         JZ    *+2                                                              
*                                                                               
         LH    RF,POPTIME          Poptime in seconds                           
         M     RE,=AL4(100)                                                     
         STCM  RF,7,TIMEGRAP+1                                                  
         LH    RF,LOGTIME          Logtime in hours                             
         M     RE,=AL4(60*60*100)                                               
         STCM  RF,7,TIMELOG+1                                                   
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R0,TIMELOG+4        SET AS LOG TIME                              
*                                                                               
         USING FACIDD,RF                                                        
         LARL  RF,FACIDTAB                                                      
INIT05   CLI   0(RF),X'FF'         End of table?                                
         JE    *+2                 Invalid DSTYP DIE                            
         CLC   FACIDSPC,DSTYP      Match on dataspace                           
         JE    INIT06                                                           
         AHI   RF,FACIDLNQ                                                      
         J     INIT05                                                           
*                                                                               
INIT06   MVC   AFACITAB,FACAID     A(FACIDxxx) table                            
*                                                                               
         XC    DMCB,DMCB           DO SYSFLES call to get table                 
         MVI   DMCB+11,1           Get first list                               
         GOTO1 ADATAMGR,DMCB,DMREAD,=C'SYSFLES'                                 
         ICM   R1,15,12(R1)                                                     
         JZ    *+2                 Bad call or something                        
         ST    R1,ASYSFLES                                                      
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DMCB+16                                                    
         JZ    INIT10                                                           
*                                                                               
         SAC   512                                                              
         USING DSJOBD,R2                                                        
         LA    R1,DSPACECB                                                      
         ST    R1,DSJDSECB                                                      
         BRAS  RE,ARSOFF                                                        
         DROP  R2                                                               
*                                                                               
INIT10   BRAS  RE,SETOPS           SET UP OPER COMMS                            
*                                                                               
         CLI   TAPE,C'Y'                                                        
         JNE   INIT015                                                          
*                                                                               
         BRAS  RE,DYNIT                                                         
         OPEN  (GRAPHLOG,OUTPUT)   OPEN GRAPH LOG                               
         MVI   OPENFLG,C'Y'                                                     
*                                                                               
INIT015  CLI   ECMDO,C'Y'                                                       
         JNE   INIT20                                                           
*                                                                               
         SAM31                                                                  
         ECIXAPI INIT,RSSID=RSSID,HANDLE=HANDLE,MF=(E,PLIST)                    
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         SAM24                                                                  
*                                                                               
INIT20   LHI   R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
         J     EXITOK                                                           
*                                                                               
RSSID    DC    CL16'ECMDO           '                                           
*                                                                               
         IEABRCX DISABLE                                                        
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
EXITL    LTR   RB,RB                                                            
         J     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
$$DATA   LOCTR ,                                                                
         EJECT                                                                  
*                                                                               
JFCB     DS    44F                                                              
JFCBPTR  DC    X'87'                                                            
         DC    AL3(JFCB)                                                        
*                                                                               
GRAPHLOG DCB   DDNAME=GRAPHLOG,DSORG=PS,MACRF=(PM),RECFM=VB,           *        
               BLKSIZE=8200,LRECL=4096,BUFNO=2,EODAD=GRAFULL,          *        
               SYNAD=GRAX,EXLST=JFCBPTR                                         
*                                                                               
LIMITS   DCB   DDNAME=LIMITS,DSORG=PS,MACRF=(GM),RECFM=VB,             *        
               BLKSIZE=8200,LRECL=4096,EODAD=LIMITX                             
*                                                                               
***********************************************************************         
*        TIMER TABLES  CL1'Y/N',AL3(FREQ),AL4(LAST TIME)              *         
***********************************************************************         
TIMETAB  DS    0F                                                               
TIMEGRAP DC    CL1'N',AL3(60*100),AL4(0)  GRAPH STATS 60 SECONDS                
TIMELOG  DC    CL1'N',AL3(8*60*60*100),AL4(0)  LOG REFRESH 8 HOURS              
TIMEEOT  DC    X'FFFFFFFF'                                                      
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS & LTORG                                            *         
***********************************************************************         
ARZERO   DC    16F'0'                                                           
READ     DC    CL8'READ   '                                                     
BUFFER   DC    CL8'BUFFER '                                                     
DMREAD   DC    CL8'DMREAD '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
OPEN     DC    CL8'OPEN'                                                        
CTFILE   DC    CL8'CTFILE'                                                      
LOCKSPC  DC    CL8'LOCKSPC'                                                     
SPACES   DC    CL256' '                                                         
STARS    DC    16C'*'                                                           
EFFS     DC    16X'FF'                                                          
MAXLINE  DC    PL3'60'                                                          
LWAIT    DC    AL4(30*100)                                                      
TIMRECB2 DC    F'0'                ECB OF ATTACHED TASK TIMER                   
ENDTASK  DS    X                   FLAG Y IF WE MUST EOJ                        
                                                                                
T1       DC    166C' '                                                          
                                                                                
*&&NOP                                                                          
TYPETAB  DS    0CL13                                                            
         DC    C'A ACRNPLA    '    ACCPAK ACCENT                                
         DC    C'B ACRNMPA    '    ACCPAK SCRIBE                                
         DC    C'C Control    '    CONTROL SYSTEM                               
         DC    C'D Demos      '    US DEMOGRAPHICS (COMPARAGRAPH)               
         DC    C'E Na Demos   '    NA DEMOGRAPHICS (DESKTOP)                    
         DC    C'H RMDOHA     '    UK MEDIA HIGH SPEED DOWNLOAD                 
         DC    C'M RMBUYA     '    EU MEDIA EXPLORER                            
         DC    C'N Me  STATS  '    US NETPAK MATCHMAKER                         
         DC    C'O ACRNBTA    '    BRAND OCEAN TIME UPLOAD                      
*&&US*&& DC    C'P Printpak   '    NA PRINTPAK ADBUYER                          
*&&UK*&& DC    C'P RMPAYA     '    EU MEDIA OFFLINE PAY UPLOAD                  
         DC    C'R (Gupta)    '    US DESKTOPS UPLOAD FOR GUPTA                 
*&&US*&& DC    C'S Steward    '    US NETPAK STEWARD                            
*&&UK*&& DC    C'S RMSTAA     '    UK MEDIA STATS                               
         DC    C'T Na Desktops'    NA DESKTOPS                                  
         DC    C'U RMUPDA     '    EU MEDIA FFLINE EST REFRESH UPLOAD           
         DC    C'W ACRNBDA    '    BRAND OCEAN DOWNLOADS                        
         DC    C'X Aud Estimat'    US NETPAK AUDIENCE ESTIMATOR                 
         DC    C'Y RMPUBA     '    EU MEDIA MQ SPOTS PROCESSOR                  
         DC    C'Z Me Buy Upld'    EU MEDIA BUY UPLOAD                          
         DC    C'  Unknown    '                                                 
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DCBS & ADCONS                                      *                   
*************************************************************                   
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
ADATAMGR DC    V(DATAMGR)                                                       
ALOCKSPC DC    V(LOCKSPC)                                                       
*                                                                               
ACOMM    DC    A(0)                                                             
MYASID   DC    H'0'                                                             
*                                                                               
       ++INCLUDE FACIDTABL                                                      
*                                                                               
UTL      DC    F'0',AL1(10),XL250'00'                                           
*                                                                               
SSB      DC    H'0',X'FF',X'14',1022X'00'                                       
         EJECT                                                                  
*************************************************************                   
*        ECBS AND TIMER FIELDS                              *                   
*************************************************************                   
         DS    0F                                                               
ECBLST2  EQU   *                                                                
ADSPECB  DC    A(DSPACECB)                                                      
AOPERECB DC    A(0)                                                             
         DC    X'80',AL3(TIMRECB2) A(TIMER ECB)                                 
*                                                                               
STIMERID DS    XL4                                                              
TIME     DS    XL4                                                              
DSPACECB DS    XL4                                                              
*                                                                               
$$CODE   LOCTR ,                                                                
         EJECT                                                                  
*************************************************************                   
* THE TIMER EXIT ROUTINE.  IT POSTS AN ECB.                 *                   
*************************************************************                   
TIMRXIT2 SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMRXIT2,RB                                                      
         LARL  R1,TIMRECB2                                                      
         POST  (R1)                                                             
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
*************************************************************                   
*        ERROR HANDLER MDUMPER EXIT ROUTINE                 *                   
*************************************************************                   
         DS    0D                                                               
         DROP  RB                                                               
         USING *,RB                                                             
ERREXT   LR    RB,RF               ESTABLISH BASE                               
         L     R2,0(R1)                                                         
         USING SDWA,R2             R2=SDWA                                      
         ST    R1,MYR1                                                          
         ST    RE,MYRE                                                          
*                                                                               
         ICM   R1,15,SDWAGR11      FIND ABENDING RB                             
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   22(8,R1),=C'**DMRC**' DIED IN DMRC - END JOB                     
         J     ERREOJ                                                           
*                                                                               
ERREOJ   L     R1,MYR1             IF ERREOJ RETURN ENDJOB                      
         LA    RF,=C'ENDJOB'                                                    
         ST    RF,4(R1)                                                         
         LARL  R1,ENDTASK                                                       
         MVI   0(R1),C'Y'                                                       
*                                                                               
ERRRET   L     R1,MYR1             RESTORE R1 AND RE                            
         L     RE,MYRE                                                          
         BR    RE                                                               
*                                                                               
MYR1     DC    A(0)                SAVED R1 AND RE                              
MYRE     DC    A(0)                                                             
MLINE    DC    CL64' '                                                          
         LTORG                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
K        EQU   1024                                                             
                                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVER2   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
AJOBTAB  DS    A                                                                
AJCLASS  DS    A                                                                
ACLASS   DS    A                                                                
JCOFFS   DS    A                                                                
JOBHDRL  DS    H                                                                
JOBTABL  DS    H                                                                
RQPURGE  DS    F                                                                
*                                                                               
         DS    18F                                                              
PLIST    DS    8F                                                               
HANDLE   DS    F                                                                
*                                                                               
DUB      DS    D                                                                
QUAD     DS    2D                                                               
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
FLAG1    DS    X                                                                
F1ENABLE EQU   X'80'                                                            
*                                                                               
SAVEF01  DS    4A                                                               
*                                                                               
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
SAVELOCL DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
ALET     DS    A                   DMGR ALET                                    
TBLET    DS    A                   TABS ALET                                    
*                                                                               
AFACITAB DS    A                   A(FACIDTAB) BY DSPACE                        
ASYSFLES DS    A                   A(SYSFLES)                                   
ASTATE   DS    A                   A(STATE TABLE)                               
*                                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
MYWORK   DS    CL64                                                             
*                                                                               
SVJNAM   DS    CL8                                                              
SVJNUM   DS    CL2                                                              
SVJASID  DS    CL2                                                              
SVJTYPE  DS    CL1                                                              
SVJADV   DS    CL1                                                              
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
*                                                                               
DINDEX   DS    A                   DATASPACE INDEX                              
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
STATSYS  DS    CL8                 STAT=SYS VALUE                               
*                                                                               
DAYNOW   DS    XL1                                                              
DATNOW   DS    XL1                                                              
MONNOW   DS    XL1                                                              
OPENFLG  DS    XL1                                                              
LOCKFLG  DS    X                                                                
BADJOB   DS    X                                                                
ABEND    DS    X                   FLAG Y IF WE MUST ABEND                      
ABENDS   DS    H                   COUNT ABENDS                                 
*                                                                               
TIMENTU  DS    A                   TIME NOW TUS USED FOR LOCGCHK                
CURRTOR  DS    A                   A CURRENT TOR                                
CURRFAC  DS    A                   A CURRENT FAC                                
CURRFACN DS    CL8                 CURRENT FAC NAME                             
*                                                                               
TIMENOW  DS    A                   BINARY TIME AT END OF WAIT                   
TIMEOLD  DS    A                   PREVIOUS POP TIME                            
DATENOW  DS    PL4                 BINARY DATE                                  
DATEOLD  DS    PL4                 BINARY DATE LAST TIME                        
HEADER   DS    0CL33                CURRENT HEADER                              
HEADER0  DS    CL11                                                             
HEADER1  DS    CL11                                                             
HEADER2  DS    CL11                                                             
*                                                                               
TOPJOB   DS    F                                                                
RESNUM   DS    XL1                 RESOURCE NUMBER                              
SEOPN    DS    CL1                 SE NUMBER                                    
SEOPNN   DS    CL7                 SE NAME                                      
SEFILN   DS    CL256               SYSTEMS FOR OPEN                             
*                                                                               
GRAREA   DS    0CL260                                                           
GRLEN    DS    XL4                                                              
GRLINE   DS    CL256                                                            
*                                                                               
TYPETAB  DS    20CL10                                                           
TYPETABX EQU   *                                                                
TYPETABL EQU   *-TYPETAB                                                        
*                                                                               
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
TODAY    DS    F                   WORKING AREAS FOR BILLDATE                   
TOMORROW DS    F                                                                
YESTERDY DS    F                                                                
BILLTIME DS    F                                                                
SYSNAME  DS    CL8                                                              
*                                                                               
ASOONSV  DS    A                   ADDCONS FOR SOONCOPY                         
ACLASSV  DS    A                                                                
ACLAS    DS    A                                                                
AJOBS    DS    A                                                                
*                                                                               
LOGEOF   DS    X                                                                
DSTYP    DS    X                   DATASPACE TYPE - R(EP)/A(DV)/T(ST)           
REBUILD  DS    X                   DATASPACE TYPE - R(EP)/A(DV)/T(ST)           
FILMANI  DS    X                   FILMAN INIT DONE                             
DYNTAPE  DS    X                   DYNAMIC TAPE ALLOCATE                        
*                                                                               
CLASSCNT DS    F                                                                
CLASSWAI DS    F                                                                
CLASSRUN DS    F                                                                
*                                                                               
RUNSTATS DS    (RNSMAXQ)XL(RNSLNQ)                                              
RUNSTLNQ EQU   *-RUNSTATS                                                       
*                                                                               
USSQEXCL DS    CL256                                                            
*                                                                               
CARD     DS    CL80                                                             
CARD1    DS    CL80                                                             
CARD2    DS    CL80                                                             
*                                                                               
       ++INCLUDE DMUSSKEY                                                       
*                                                                               
SAVEJBXQ EQU   10                                                               
SAVEJOB# DS    H                                                                
SAVEJOB  DS    (SAVEJBXQ)CL(L'TBJNTRY)                                          
*                                                                               
         DS    0D                                                               
IOAREA   DS    4096C                                                            
*                                                                               
CONTENT  DS    256XL32                                                          
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
SORTD    DSECT                                                                  
SORTNAME DS    CL8                 JOBNAME                                      
SORTPRTY DS    CL1                 PRIORITY                                     
SORTWAIT DS    CL1                 WAIT SEQUENCE                                
SORTTIME DS    CL3                 SUBMIT TIME                                  
SORTINDX DS    XL2                 INDEX TO ENTRY                               
SORTPOS  DS    XL1                 POSITION                                     
                                                                                
RTYPED   DSECT ,                   Runner types                                 
RTYPE    DS    C                   Type                                         
         DS    X                                                                
RTYPJOB  DS    CL8                 Job name                                     
RTYPLNQ  EQU   *-RTYPED                                                         
*                                                                               
RUNSTATD DSECT ,                   Stats based on type                          
RNSTYPE  DS    C                   RUNNER Type                                  
         DS    X                                                                
RNSCOUNT DS    XL2                 Total wait time                              
RNSWAIT  DS    XL4                                                              
RNSLNQ   EQU   *-RUNSTATD                                                       
RNSMAXQ  EQU   20                  Max types                                    
*                                                                               
                                                                                
         DSECT                                                                  
         PUSH  ACONTROL                                                         
         ACONTROL COMPAT(NOCASE)                                                
***********************************************************************         
* IBM MACRO DSECTS                                                              
***********************************************************************         
         IEZCIB                                                                 
         IEZCOM                                                                 
         IEFZB4D0                                                               
         IEFZB4D2                                                               
         IEFJFCBN                                                               
         IHAASCB                                                                
         IHAASSB                                                                
         IEECHAIN                                                               
         IAZJSAB                                                                
         POP   ACONTROL                                                         
*                                                                               
*IHASDWA MACRO                                                                  
         IHASDWA GR32=YES                                                       
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* DMPRTQK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DMDSYSHDR                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMDSYSHDR                                                      
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DMDSHDR                                                                       
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DMSYSFD                                                                       
       ++INCLUDE DMSYSFD                                                        
         PRINT ON                                                               
* DMDFTPH                                                                       
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DDPERVALD                                                                     
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* FATABSPQ                                                                      
       ++INCLUDE FATABSPQ                                                       
         PRINT ON                                                               
* FACIDTAJ                                                                      
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FATABSD                                                                       
       ++INCLUDE FATABSD                                                        
* FATABSJOB                                                                     
       ++INCLUDE FATABSJOB                                                      
* FATABSRUN                                                                     
       ++INCLUDE FATABSRUN                                                      
* FAPIGFACD                                                                     
       ++INCLUDE FAPIGFACD                                                      
         PRINT ON                                                               
* FAD                                                                           
       ++INCLUDE FAD                                                            
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014DDRSMFEED 09/13/19'                                      
         END                                                                    
