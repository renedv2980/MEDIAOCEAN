*          DATA SET DDOPER     AT LEVEL 009 AS OF 09/05/17                      
*PHASE DDOPERA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DDWTO                                                                  
*INCLUDE LOCKSPC                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE DDSTATE                                                                
*&&NOP   SET   N                                                                
         TITLE 'DDSOPER -HANDLE OPERATOR COMMANDS'                              
         PRINT NOGEN                                                            
DDOPERA  CSECT                                                                  
         ENTRY SSB                                                              
         NBASE WORKX-WORKD,**OPER**,=A(WORKAREA),RA,R9,CLEAR=YES                
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         LARL  R8,SSB                                                           
         USING SSOOFF,R8                                                        
*                                                                               
         BRAS  RE,INIT                                                          
         BRAS  RE,MAIN             MAIN PROGRAM                                 
         BE    XBASE                                                            
         MVI   RTERR,4             IF NEQ SET RC TO 4 AND EXIT                  
         B     XBASE                                                            
*                                                                               
EXITEQU  SAC   0                                                                
         CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITNEQ  SAC   0                                                                
         LTR   RB,RB                                                            
         B     EXIT                                                             
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE RC=RTERR,RL=1                                                    
         EJECT                                                                  
***********************************************************************         
*        INIT                                                         *         
***********************************************************************         
INIT     NTR1                                                                   
         LA    R2,FULL             GET JOB NAME INTO MVSNAME                    
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   MVSNAME,0(R2)                                                    
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         LA    R1,FULL                                                          
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R5 = A(ASSB)                                
*                                                                               
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     R1,ASSBJSAB-ASSB(R2) R2 = A(JSAB)                                
         USING JSAB,R1                                                          
         MVC   MVSJOB,JSABJBID     GET JOBID (E.G., JOB12345)                   
         PACK  DUB,MVSJOB+3(5)                                                  
         CVB   R1,DUB                                                           
         STCM  R1,3,MVSJNUM        CONVERT TO JOBNO                             
         DROP  R1                                                               
*                                                                               
         MVC   OPMSG,SPACES                                                     
         SAM24 ,                   SWITCH BACK TO 24 BT MODE                    
         B     EXITEQU                                                          
         EJECT                                                                  
***********************************************************************         
*        MAIN LOOP - READ CARDS AND SEND FOR VALIDATION               *         
***********************************************************************         
MAIN     NTR1  ,                                                                
         LA    R1,COMMS            SET POINTER                                  
         ST    R1,ACOMMS                                                        
         XC    NCOMMS,NCOMMS                                                    
*                                                                               
MAIN02   LA    R2,CARDWORK                                                      
         GOTO1 =V(CARDS),DMCB,(R2),=C'RE00'                                     
         CLC   =C'/*',0(R2)        END OF CARDS?                                
         BE    MAIN04              YES                                          
         CLI   0(R2),C'*'                                                       
         JE    MAIN02                                                           
         MVC   CARD,SPACES                                                      
         MVC   CARD(72),CARDWORK                                                
*                                                                               
         CLI   SSODSPAC,0                                                       
         BNE   MAIN03                                                           
*                                                                               
         CLC   CARD(7),=C'DSPACE=' MUST START DSPACE=X                          
         BNE   INVALID                                                          
         MVC   SSODSPAC,CARD+7                                                  
*                                                                               
         USING FACIDD,RE                                                        
         LA    RE,FACIDTAB         FIND CORRECT TABLE                           
MAIN02B  CLI   FACIDSPC,X'FF'                                                   
         JE    *+2                 NOT VALID DSPACE                             
         CLC   FACIDSPC,SSODSPAC   MATCH ON DSPACE                              
         BE    MAIN02C                                                          
         LA    RE,FACIDLNQ(,RE)                                                 
         B     MAIN02B                                                          
*                                                                               
MAIN02C  MVC   AFACTAB,FACAID      SAVE OFF TABLE ADDRESS                       
         DROP  RE                                                               
*                                                                               
         GOTO1 =V(LOCKSPC),DMCB,X'20000000',0                                   
         GOTO1 =V(LOCKSPC),DMCB,X'20008000',0                                   
*                                                                               
         USING DMDSHDR,R2                                                       
         SR    R2,R2                                                            
         LAM   AR2,AR2,SSOALET                                                  
         SAC   512                                                              
         MVC   WORK(64),DMDHDR                                                  
         SAC   0                                                                
         DROP  R2                                                               
*                                                                               
         CLI   CARD+8,C','         MORE                                         
         BNE   MAIN02                                                           
         MVC   CARD(70),CARD+9                                                  
*                                                                               
         CLC   CARD(5),=C'WAIT='   CAN ALSO HAVE WAIT=N                         
         BNE   MAIN03                                                           
         MVC   WAIT,CARD+5                                                      
         CLI   CARD+6,C','         MORE                                         
         BNE   MAIN02                                                           
         MVC   CARD(70),CARD+7                                                  
*                                                                               
MAIN03   LA    R1,CARD                                                          
         BRAS  RE,VALIDATE                                                      
         B     MAIN02                                                           
*                                                                               
MAIN04   BRAS  RE,WAITFOR                                                       
         B     EXITEQU                                                          
*                                                                               
INVALID  WTO   '**ERROR** FIRST CARD MUST BE DSPACE=',MCSFLAG=HRDCPY            
         B     EXITNEQ                                                          
         EJECT                                                                  
***********************************************************************         
*        VALIDATE EACH CARD AND SEND FOR PROCESSING                   *         
***********************************************************************         
VALIDATE NTR1                                                                   
*                                                                               
         BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
         BNE   VALID090            NEQ SO EXIT                                  
*                                                                               
         CLC   ACTNUM,=H'27'       REMOTE STOP AND GO                           
         JE    VALID060                                                         
         CLC   ACTNUM,=H'28'       ARE ALLOWED FOR DDOPER                       
         JE    VALID060                                                         
         CLC   ACTNUM,=H'35'       STATE CHANGE                                 
         JE    VALID070                                                         
         TM    FLAG,FACOADV        ADV COMMAND?                                 
         BZ    VALID050            NO                                           
*                                                                               
         CLC   ACTNUM,=H'24'       NEXT SCAN FIELD (FILENAME)                   
         BNE   VALID090                                                         
         BRAS  RE,NEWDAY                                                        
         B     VALIDXXX                                                         
*                                                                               
VALID050 CLC   ACTNUM,=H'01'       IF EOJ                                       
         BNE   VALID060                                                         
         CLI   ADNUM,0             MUST NOT BE FOR ALL ADVS                     
         BE    VALID090                                                         
*                                                                               
VALID060 BRAS  RE,SETCOM           SEND COMMAND TO DSPACE                       
         B     VALIDXXX                                                         
*                                                                               
VALID070 XC    DMCB+4(4),DMCB+4    DO STATE CHANGE CALL                         
         MVC   DMCB+6(2),SYNUM                                                  
         GOTO1 =V(DDSTATE),DMCB,(0,STACHA),,0                                   
         J     VALIDXXX                                                         
*                                                                               
VALID090 WTO   '**ERROR** INVALID CARD',MCSFLAG=HRDCPY                          
*                                                                               
VALIDXXX SAM24                                                                  
         SAC   0                                                                
         B     EXITEQU                                                          
         EJECT                                                                  
*************************************************************                   
*        WAIT FOR COMMANDS TO BE EXECUTED                   *                   
*************************************************************                   
WAITFOR  NTR1                                                                   
         LAM   AR2,AR2,SSOALET                                                  
         LAM   AR3,AR3,SSOALET                                                  
         MVC   WAITTIME,=F'6000'   INITIALLY 60 SECS                            
*                                                                               
WAITFOR1 SAC   512                                                              
         MVI   BYTE,0                                                           
*                                                                               
         LA    R6,COMMS            LIST OF COMMANDS SENT                        
WAITFOR2 C     R6,ACOMMS           ARE WE AT END YET                            
         BE    WAITFOR5                                                         
         OC    0(4,R6),0(R6)       ALREADY DONE IT                              
         BZ    WAITFOR4                                                         
*                                                                               
         L     R2,0(,R6)                                                        
         USING DSCOMM,R2                                                        
         TM    DSCFLAG,DSCDONEQ    IS IT DONE YET                               
         BO    WAITFOR3                                                         
         OC    DSCOMMS,DSCOMMS     OR WAS IT CLEANED (RCVR PERHAPS)             
         BZ    WAITFO3A                                                         
         MVI   BYTE,1              FLAG NOT DONE YET                            
*                                                                               
         SAC   0                                                                
         TIME  BIN                 CALCULATE AGE OF MESSAGE                     
         SAC   512                                                              
         S     R0,DSCTIME                                                       
         C     R0,WAITTIME         60 SECONDS +30 PER RETRY                     
         BL    WAITFOR4                                                         
*                                                                               
         TM    DSCFLAG,DSCJNUMQ    JNUMQ IS AN ASID                             
         BO    WAITCHK5                                                         
*                                                                               
         USING FACITABD,R1                                                      
         L     R1,AFACTAB                                                       
         MVC   BYTE1,DSCDEST       MUST BE ADV SO STRIP OFF AOR BITS            
         NI    BYTE1,X'0F'                                                      
WAITCHK1 CLI   0(R1),X'FF'         EOT                                          
         JE    *+2                 NOT FOUND                                    
         CLC   BYTE1,FACIID        FIND SYSTEM IN FACIDTAB                      
         BE    WAITCHK2                                                         
         AHI   R1,L'FACITAB        NEXT ENTRY                                   
         B     WAITCHK1                                                         
*                                                                               
WAITCHK2 MVC   OPMSGTXT(39),OPERBUSY                                            
         MVC   OPFACID,MVSNAME                                                  
         MVC   OPMSGTXT(8),=C'SYSTEM  '                                         
         MVC   OPMSGTXT+9(7),SPACES                                             
         MVC   OPMSGTXT+9(4),FACISN4    SYSTEM NAME                             
         DROP  R1                                                               
*                                                                               
         SR    R1,R1               SET AOR CHR IN MSG                           
         IC    R1,DSCDEST                                                       
         SRL   R1,4                                                             
         LTR   R1,R1                                                            
         BZ    WAITCHK9                                                         
         LA    R1,X'C0'(R1)                                                     
         STC   R1,OPMSGTXT+13                                                   
         B     WAITCHK9                                                         
*                                                                               
WAITCHK5 XR    R3,R3                                                            
         L     R3,DHAJOBS-DMDHDR(,R3)                                           
*                                                                               
         USING DSJOBD,R3                                                        
         MVC   OPMSGTXT(39),OPERBUSY                                            
         MVC   OPFACID,MVSNAME                                                  
*                                                                               
         LA    R0,DSJOBMXQ                                                      
WAITCHK6 CLC   DSCDEST,DSJASID                                                  
         BE    WAITCHK7                                                         
         LA    R3,DSJOBLNQ(,R3)    NEXT ENTRY                                   
         BCT   R0,WAITCHK6                                                      
         B     WAITFOR3            JOB NOT FOUND SO MUST HAVE GONE              
*                                                                               
WAITCHK7 MVC   OPMSGTXT(8),DSJNAM                                               
         MVC   HALF,DSJNUM                                                      
         DROP  R3                                                               
         EDIT  (B2,HALF),(6,OPMSGTXT+10),ALIGN=LEFT                             
         SAC   0                                                                
*                                                                               
WAITCHK9 SAC   0                                                                
*                                  DEALS WITH MULTIPLE WAITS                    
         TIME  BIN                                                              
         S     R0,LASTWAIT         TIME OF LAST RETRY                           
         C     R0,WAITTIME         LESS THAT WAITTIME                           
         BL    WAITFOR4            JUST IGNORE THIS TOO                         
*                                                                               
         BRAS  RE,WTOR                                                          
         MVC   OPMSGTXT(40),SPACES                                              
         CLI   REPLY,C'I'                                                       
         BE    WAITFOR3                                                         
         TIME  BIN                 RESET TIME FOR RETRY                         
         ST    R0,LASTWAIT         SAVE TIME OF LAST WAIT MESSAGE               
         SAC   512                                                              
         L     R1,WAITTIME                                                      
         AHI   R1,6000             ADD 1 MIN TO WAITTIME                        
         C     R1,=F'60000'        UNTIL 10 MIN                                 
         BNL   *+8                                                              
         ST    R1,WAITTIME                                                      
         ST    R0,DSCTIME                                                       
         B     WAITFOR4                                                         
*                                                                               
WAITFOR3 SAC   512                                                              
         MVC   COACTN,=C'POSTED/CLEARED'                                        
         BRAS  RE,TRACEOUT                                                      
         XC    DSCOMMS,DSCOMMS     CLEAR IT                                     
         XC    0(4,R6),0(R6)                                                    
         J     WAITFOR4                                                         
*                                                                               
WAITFO3A SAC   512                                                              
         MVC   COACTN,=C'ENTRY CLEARED '                                        
         BRAS  RE,TRACEOUT                                                      
         XC    0(4,R6),0(R6)                                                    
*                                                                               
WAITFOR4 SAC   512                                                              
         AHI   R6,4                                                             
         B     WAITFOR2                                                         
         DROP  R2                                                               
*                                                                               
WAITFOR5 CLI   BYTE,0              ALL DONE                                     
         BE    WAITFOR7                                                         
*                                                                               
WAITFOR6 SAC   0                                                                
*                                                                               
         LA    R1,1                1 SECOND                                     
         SR    R0,R0                                                            
         M     R0,=F'38400'        * 38400 FOR TUS                              
         ST    R1,FULL                                                          
         STIMER WAIT,TUINTVL=FULL  WAIT 1 SECS THEN TRY AGAIN                   
         B     WAITFOR1                                                         
*                                                                               
WAITFOR7 CLI   WAIT,C'0'           ANY FORCED WAIT                              
         BNH   WAITFORX                                                         
         CLI   WAIT,C'9'           MUST BE 0 TO 9                               
         BH    WAITFORX                                                         
         LLC   R1,WAIT             COUNT DOWN                                   
         BCTR  R1,0                                                             
         STC   R1,WAIT                                                          
         B     WAITFOR6            AND WAIT 1 SECOND                            
*                                                                               
WAITFORX LAM   AR3,AR3,=F'0'       MUST CLEAR THIS                              
         B     EXITEQU                                                          
         EJECT                                                                  
*************************************************************                   
*        SET UP NEW BILLING DATE FOR SYSTEM                 *                   
*************************************************************                   
NEWDAY   NTR1                                                                   
         STAM  ARE,AR1,DUB         USE DUB DUB1 TO SAVE ARS                     
         TIME  TU                                                               
         LAM   ARE,AR1,DUB                                                      
         ST    R1,TODAY            SAVE DATE IN TODAY                           
         ST    R0,FULL             SAVE TIME IN FULL                            
*                                                                               
         MVC   HALF,SYNUM                                                       
         OC    SYNUM,SYNUM                                                      
         BNE   NEWD010             SPECIFIC SYSTEM                              
         LHI   R1,1                                                             
         STH   R1,HALF             OR BEGIN WITH SYSTEM 1                       
*                                                                               
NEWD010  GOTO1 =V(DATCON),DMCB,(6,TODAY),(0,DUB)                                
         GOTO1 =V(ADDAY),DMCB,(C'D',DUB),DUB1,F'1'                              
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(15,TOMORROW)                           
*                                                                               
NEWD020  LAM   AR2,AR2,SSOALET     PICK UP ALET                                 
         SAC   512                                                              
         SR    R2,R2               SET R2 TO ADVS BLOCK                         
         ICM   R2,3,HALF           SYSTEM NUMBER TO PROCESS                     
         JZ    NEWDAYXX                                                         
         SLL   R2,6                X 64 (ENTRY SIZE)                            
*                                                                               
         USING DMSPACED,R2                                                      
         MVC   SYNAME,DSPNAME                                                   
*                                                                               
         ICM   R2,7,DSPTFRST+1     LOAD ADDRESS OF SYSTEM BLOCK                 
         JZ    NEWDAYXX                                                         
*                                                                               
         USING DMSYSHDR,R2                                                      
*                                                                               
         CLI   SSODSPAC,C'T'       15:00 FOR TST                                
         JNE   NEWD021                                                          
         CLC   FULL,=AL4(15*60*60*38400) AFTER 15:00                            
         JL    NEWD030                                                          
         J     NEWD025                                                          
*                                                                               
NEWD021  CLC   FULL,=X'8C136000'   17*60*60*38400 = 17:00                       
         BL    NEWD030                                                          
*                                                                               
NEWD025  CLC   DSYBDATE,TODAY      IF TODAY THEN SET TOMORROW                   
         BNL   NEWD040                                                          
*                                                                               
         MVC   WORK(2),=X'002A'    ELSE WARN AND SET TO TODAY                   
         MVC   WORK+2(40),=C'*WARNING* LATE NEWDAY ISSUED FOR        '          
         MVC   WORK+35(7),SYNAME                                                
         SAC   0                                                                
         WTO   TEXT=WORK                                                        
         WTO   'DATE HAS BEEN SET TO TODAY'                                     
         SAC   512                                                              
*                                                                               
NEWD030  L     R1,TODAY                                                         
         ST    R1,DSYBDATE         UPDATE BILLING DATE TO TODAY                 
         B     NEWDAYX                                                          
*                                                                               
NEWD040  L     R1,TOMORROW                                                      
         ST    R1,DSYBDATE         UPDATE BILLING DATE TO TOMORROW              
*                                                                               
NEWDAYX  SAC   0                                                                
         ST    R1,DUB                                                           
         GOTO1 =V(DATCON),DMCB,(6,DUB),(17,DUB1)                                
         LA    R1,21               NEWDAY MESSAGE                               
         BRAS  RE,GETMSG                                                        
         MVC   OPMSGOPS+7(8),SYNAME                                             
         MVC   OPMSGOPS+23(7),DUB1                                              
         MVC   OPCMND2,COMMAND                                                  
         BRAS  RE,WTO                                                           
NEWDAYXX SAC   0                                                                
*                                                                               
         OC    SYNUM,SYNUM         SPECIFIC SYSTEM                              
         JZ    NEWDAY99                                                         
*                                                                               
         SAC   512                 REQUEST TABLE                                
         L     R2,DSYAREQT         CLEAR REQUEST LIST                           
         LH    R6,0(,R2)                                                        
         L     R7,2(,R2)                                                        
         AHI   R2,6                                                             
         XC    0(8,R2),0(R2)       CLEAR 00 ENTRY                               
         MVC   0(2,R2),=C'00'      AND REBUILD IT                               
         AHI   R2,8                                                             
*                                                                               
         XC    0(8,R2),0(R2)       CLEAR REMAINING ENTRIES                      
         BXLE  R2,R6,*-6                                                        
         SAC   0                                                                
         J     EXITEQU                                                          
         DROP  R2                                                               
*                                                                               
NEWDAY99 LH    R1,HALF             IF NEWDAY,,ALL                               
         AHI   R1,1                                                             
         STH   R1,HALF             NEXT SYSTEM                                  
         CHI   R1,256                                                           
         JL    NEWD020                                                          
         J     EXITEQU                                                          
         EJECT                                                                  
*************************************************************                   
*        BUILD COMMAND FOR DATASPACE                        *                   
*************************************************************                   
SETCOM   NTR1                                                                   
         XC    MYWORK,MYWORK       BUILD COMLINE IN MYWORK                      
         LA    R4,MYWORK                                                        
         USING DSCOMM,R4                                                        
         MVC   DSCSORC,=X'FFFE'    FROM ME                                      
         MVC   DSCJNUM,MVSJNUM     SAVE JOB NUMBER HERE                         
         MVC   DSCDEST,ADNUM       TO ADNUM (ZERO FOR ALL)                      
         MVI   DSCDEST+1,0                                                      
         MVC   DSCCOMM,ACTNUM      SET ACTION                                   
         MVC   DSCHEADR,HEADER                                                  
*                                                                               
         MVC   DSCDATA(2),SYNUM    SET SYS/FIL INFO IN DATA                     
         MVC   DSCDATA+2(1),FILNUM                                              
         MVC   DSCDATA+3(2),RESOURCE                                            
*                                                                               
         TM    FLAG,FACOFIL        IS FILENUM REQUIRED?                         
         BZ    *+12                NO                                           
         CLI   FILNUM,0            YES, DO WE HAVE ONE?                         
         BE    NOFILE              NO                                           
*                                                                               
         TM    FLAG,FACONSE        ANY DATA REQUIRED ?                          
         BO    *+14                                                             
         OC    DSCDATA,DSCDATA     MUST BE SOME DATA                            
         JZ    *+2                                                              
*        BZ    NODATA                                                           
*                                                                               
         TIME  BIN                 SAVE TIME IN DSCTIME                         
         ST    R0,DSCTIME                                                       
         OC    DSCTIME,DSCTIME     DON'T ALLOW ZERO AT MIDNIGHT                 
         BNZ   *+10                                                             
         MVC   DSCTIME,=X'00000001'                                             
*                                                                               
         SR    R2,R2               SET TO DATASPACE                             
         USING DMDSHDR,R2                                                       
         LAM   AR2,AR2,SSOALET                                                  
         LAM   AR3,AR3,SSOALET                                                  
         SAC   512                                                              
*                                                                               
SETCOM1  SR    R2,R2                                                            
         ICM   R1,15,SYGROUP       NO GROUP SO JUST DO ONE SYSTEM               
         BZ    SETCOM1A                                                         
         CLI   0(R1),X'FF'         END OF GROUP?                                
         BE    SETCOMX                                                          
         MVC   DSCDATA+1(1),0(R1)  MOVE SYSTEM FROM GROUP                       
*                                                                               
SETCOM1A L     R3,DHAJOBS          SET R3 TO JOBS BLOCK                         
         LA    R0,DSJOBMXQ                                                      
SETCOM2  CLI   0(R3),0             EMPTY ADV ENTRY                              
         BE    SETCOM6                                                          
*                                                                               
         USING DSJOBD,R3                                                        
         OC    DSJOPECB,DSJOPECB   IGNORE IF NO POST ECB                        
         BZ    SETCOM6                                                          
*                                                                               
         TM    FLAG,FACOMNTN       SPECIAL COMMANDS FOR MNTN                    
         JZ    *+14                                                             
         CLC   DSJNAM(4),=C'MNTN' MNTN JOB                                      
         JNE   SETCOM6                                                          
*                                                                               
         CLI   DSCDEST,0           DEST ZERO SEND TO ALL                        
         BE    SETCOM2A                                                         
         CLI   DSJADV,0            OTHERWISE IGNORE NON ADVS                    
         BE    SETCOM6                                                          
*                                                                               
         MVC   BYTE,DSJADV         ADV NUMBER SYSIX                             
         NI    BYTE,X'0F'          MATCH ON AOR AND TOR                         
         CLC   DSCDEST(1),BYTE     DOES DEST MATCH                              
         BNE   SETCOM6                                                          
*                                                                               
SETCOM2A OC    JOBNAME,JOBNAME     ANY JOBNAME SPECIFIED                        
         BZ    SETCOM2B                                                         
         LLC   R1,JOBNAMEL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   JOBNAME(0),DSJNAM   MATCH ON JOBNAME                             
         BNE   SETCOM6                                                          
         B     SETCOM3                                                          
         DROP  R4                                                               
*                                                                               
SETCOM2B EQU   *                                                                
*&&UK                                                                           
         ICM   R2,15,DSJLOCAL      ONLY SEND COM IF STATE ENTRY EXISTS          
         JZ    SETCOM3                                                          
         ICM   R2,15,DSLSTAT-DSLSTATE(R2)                                       
         JZ    SETCOM3                                                          
*                                                                               
         XR    RE,RE               STATIC STATE TABLE LOCATED                   
         ICM   RE,3,SYNUM                                                       
         SLL   RE,2                                                             
         LA    R2,DSSSYS-DSSSTATE(RE,R2)                                        
         OC    0(4,R2),0(R2)       IF ENTRY IS NULL IGNORE THIS JOB             
         JZ    SETCOM6                                                          
*&&                                                                             
*                                                                               
SETCOM3  SR    R2,R2               POINT TO DSPACE DHACOMM                      
         LA    RE,512                                                           
         L     R2,DHACOMM                                                       
         AHI   R2,4096             SKIP HEADERS                                 
*                                                                               
         USING DSCOMM,R2                                                        
SETCOM4  L     RF,=F'-1'           SET TO FFS TO GRAB ENTRY                     
         SR    R1,R1                                                            
         CS    R1,RF,DSCTIME                                                    
         BNE   SETCOM5                                                          
         MVC   DSCOMMS,MYWORK      MOVE IN COMMAND                              
*                                                                               
         MVC   DSCDEST(1),DSJADV   SEND TO THIS ADV                             
*                                                                               
         CLI   DSJADV,0            ADV OR JOB                                   
         BNE   SETCOM4A                                                         
         MVC   DSCDEST,DSJASID     JOB THEN USE ASID                            
         OI    DSCFLAG,DSCJNUMQ    INDICATE JOB TYPE                            
*                                                                               
SETCOM4A MVC   COACTN,=C'ADD COMMAND   '                                        
         BRAS  RE,TRACEOUT                                                      
*                                                                               
         MVC   WORK1,DSJNAM        SAVE OFF ENTRY                               
         SAC   0                   GO POST THE SSBOPECB                         
         BRAS  RE,POSTIT                                                        
         JE    SETCOM4C                                                         
*                                                                               
         ST    R0,FULL             SAVE COUNTER                                 
         WTO   'POST FAILED - COMMAND REMOVED',MCSFLAG=HRDCPY                   
         L     R0,FULL             RESTORE COUNTER                              
         SAC   512                 SWITCH BACK TO AR MODE                       
         XC    DSCOMMS,DSCOMMS     POST FAILED REMOVE IT                        
         J     SETCOM6                                                          
*                                                                               
SETCOM4C SAC   512                                                              
         L     R1,ACOMMS           SAVE IN LIST                                 
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         ST    R1,ACOMMS                                                        
         L     R1,NCOMMS                                                        
         AHI   R1,1                                                             
         ST    R1,NCOMMS                                                        
         CLC   NCOMMS,=F'400'      ALLOW 400 COMMANDS                           
         BL    SETCOM6                                                          
         DC    H'0'                TOO MUCH FOR ME TO HANDLE                    
*                                                                               
SETCOM5  LA    R2,L'DSCOMMS(,R2)                                                
         BCT   RE,SETCOM4          ALL ENTRIES FULL                             
         DC    H'0'                                                             
*                                                                               
SETCOM6  LA    R3,DSJOBLNQ(,R3)                                                 
         BCT   R0,SETCOM2          ALL DONE                                     
         DROP  R2,R3                                                            
*                                                                               
         ICM   R1,15,SYGROUP       TEST FOR GROUP                               
         BZ    SETCOMX                                                          
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'         TEST FOR END OF GROUP                        
         BE    SETCOMX                                                          
         ST    R1,SYGROUP                                                       
         B     SETCOM1                                                          
*                                                                               
SETCOMX  LAM   AR3,AR3,=F'0'       MUST CLEAR THIS                              
         SAC   0                                                                
         B     EXITEQU                                                          
*                                                                               
NOFILE   LA    R1,22                                                            
         J     *+8                                                              
NODATA   LA    R1,9                MISSING DATA                                 
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     EXITEQU                                                          
         EJECT                                                                  
*************************************************************                   
*        DO X MEMORY POST TO FACPAK (DETAILS IN CARD)       *                   
*************************************************************                   
POSTIT   NTR1                                                                   
         LH    R4,WORK1+12         ASID IS AT +12                               
         L     R2,WORK1+16         SSBOPECB IS AT +16                           
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         LTR   RF,RF                                                            
         BNZ   NOPOST                                                           
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   NOPOST                                                           
         L     R4,ASCBASSB                                                      
*                                                                               
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     R4,ASSBJSAB-ASSB(R4) R4 = A(JSAB)                                
         USING JSAB,R4                                                          
         CLC   WORK1(8),JSABJBNM                                                
         BNE   NOPOST                                                           
*                                                                               
         SAM24 ,                   SWITCH BACK TO 24 BT MODE                    
         LA    R5,99               SET COMPLETION CODE                          
         POST  (R2),(R5),ASCB=(R3),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTX)         
         B     EXITEQU                                                          
*                                                                               
NOPOST   SAM24 ,                   SWITCH BACK TO 24 BT MODE                    
         MVC   WORK1+10(8),WORK1                                                
         MVC   WORK1+2(8),WORK1+10                                              
         MVC   WORK1+10(18),=C' SYSTEM NOT POSTED'                              
         MVC   WORK1+0(2),=H'26'                                                
         WTO   TEXT=WORK1                                                       
         B     EXITNEQ                                                          
POSTX    POST  ERRET=DEAD,ECBKEY=YES,MF=L                                       
DEAD     DC    H'0'                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*************************************************************                   
*        OUTPUT TRACE                                       *                   
*************************************************************                   
TRACEOUT NTR1                                                                   
         USING DSCOMM,R2                                                        
*                                                                               
         MVC   COSPACE,SPACES                                                   
         XC    FULL,FULL                                                        
         MVC   FULL(2),DSCDEST     DISPLAY ADV OR JOB DEST                      
         MVC   FULL+2(1),DSCFLAG                                                
         BRAS  RE,DISJOB                                                        
         MVC   CODEST,DUB1                                                      
         MVC   CODESTN,DUB2                                                     
*                                                                               
         LARL  R1,COMTAB           SCAN COMTAB FOR ACTION                       
COM070   CLC   DSCCOMM,8(R1)                                                    
         BE    COM075                                                           
         LA    R1,16(R1)                                                        
         CLI   8(R1),X'FF'                                                      
         BNE   COM070                                                           
COM075   MVC   COCOMM(8),0(R1)                                                  
*                                                                               
         TM    11(R1),X'10'        IF SET NO SE INFORMATION                     
         BO    COMNXT                                                           
*                                                                               
         MVC   BYTE,DSCDATA+1                                                   
         BAS   RE,FINDNUM          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   COCOMM+9(7),SENAME  SHOW SYSTEM NAME                             
*                                                                               
         L     R5,APGMS            CLEAR REQUEST LIST                           
         LH    R6,0(,R5)                                                        
         L     R7,2(,R5)                                                        
         AHI   R5,6                                                             
         USING PGMLSTD,R5                                                       
*                                                                               
COM080   CLC   PGMNUM,DSCDATA+2                                                 
         JNE   *+14                                                             
         MVC   COCOMM+19(7),PGMNAME SHOW PROG NAME                              
         J     COM090                                                           
                                                                                
         BXLE  R5,R6,COM080                                                     
COM090   EQU   *                                                                
         DROP  R5                                                               
*                                                                               
COMNXT   EQU   *                                                                
         MVC   WORK1(2),=X'003C'    ELSE WARN AND SET TO TODAY                  
         MVC   WORK1+2(60),COACTN                                               
         SAC   0                                                                
         WTO   TEXT=WORK1,MCSFLAG=HRDCPY                                        
         SAC   512                                                              
*                                                                               
         J    EXIT                                                              
         EJECT                                                                  
*************************************************************                   
*        FIND ASYSHDR FROM SYS DATA                         *                   
*************************************************************                   
         USING DMDSHDR,R2                                                       
FINDSYS  NTR1                                                                   
         XR    R2,R2                                                            
         LAM   AR2,AR2,SSOALET                                                  
         SAC   512                                                              
         L     R1,DHAECBS          GET A(ECB TABLE)                             
         ST    R1,SYSMAX           SET UPPER LIMIT FOR SYSTEM SEARCH            
*                                                                               
         USING DMSPACED,R2                                                      
         ICM   R1,1,SYNAME         PREPARE FOR NAME SEARCH                      
         BCTR  R1,0                                                             
*                                                                               
         CLC   SYNAME+1(3),=C'CON' UNLESS IT'S CON                              
         JE    FINDS010                                                         
         LA    R1,5                FORCE 6 CHR MATCH ON SYSTEM                  
*                                                                               
FINDS010 EX    R1,*+8              FIND SELECTED SYS                            
         B     *+10                                                             
         CLC   DSPNAME(0),SYNAME+1                                              
         BE    FINDSX              GOT IT                                       
*                                                                               
FINDS090 AHI   R2,L'DSPHDR         NEXT SYSTEM                                  
         C     R2,SYSMAX                                                        
         BL    FINDS010                                                         
         SAC   0                                                                
         B     EXITNEQ                                                          
*                                                                               
FINDSX   ST    R2,ASYSHDR          SAVE ASYSHDR                                 
         MVC   SYNAME+1(7),DSPNAME                                              
         MVI   SYNAME,7                                                         
         XC    SYNUM,SYNUM                                                      
         SRL   R2,6                                                             
         STC   R2,SYNUM+1                                                       
         SAC   0                                                                
         B     EXITEQU                                                          
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        FIND FILE FROM FILENAME ALONE                      *                   
*************************************************************                   
         USING DMDSHDR,R2                                                       
FSYSFIL  NTR1                                                                   
         XR    R2,R2                                                            
         LAM   AR2,AR2,SSOALET                                                  
         SAC   512                                                              
         L     R1,DHAECBS          GET A(ECB TABLE)                             
         ST    R1,SYSMAX           SET UPPER LIMIT FOR SYSTEM SEARCH            
*                                                                               
         ICM   R2,15,ASYSHDR       IS SYSTEM ALREADY FOUND                      
         BNZ   FSYSF01                                                          
*                                                                               
         XC    FULL,FULL           USE FULL AS SYSTEM POINTER                   
         LA    R2,1                                                             
         SLL   R2,6                                                             
         ST    R2,FULL                                                          
*                                                                               
         USING DMSPACED,R2                                                      
FSYSF01  ICM   R2,15,DSPECB        SET R2 TO SYSTEM ENTRY                       
         N     R2,=X'3FFFFFFF'                                                  
         USING DSYHDR,R2                                                        
         LTR   R2,R2               IGNORE IF NO ECB                             
         JZ    FSYSF090                                                         
*                                                                               
         OC    ASYSHDR,ASYSHDR     IF SPECIFIC SYSTEM                           
         JZ    *+14                                                             
         CLC   SYNUM,DSYSENUM      MAKE SURE                                    
         JNE   *+2                                                              
*                                                                               
         SR    RE,RE               SET RE TO NUM OF FILES                       
         ICM   RE,3,DSYFILES                                                    
         LA    R2,DSYDATA          POINT TO FILE AREA                           
         USING DSFILHDR,R2                                                      
         IC    R1,DSFILNUM                                                      
         SRL   R1,4                                                             
         STC   R1,OVSYS            SET OV FROM FIRST FILE                       
*                                                                               
FSYSF02  CLC   DSFILDD,FILNAM      FILE MATCH                                   
         BNE   FSYSF03                                                          
*                                                                               
         XR    R1,R1                                                            
         IC    R1,DSFILNUM                                                      
         SRL   R1,4                                                             
         CLM   R1,1,OVSYS          SAME OV AS FIRST FILE                        
         BE    FSYSF10                                                          
         CLM   RE,1,=X'0F'         SERVICE FILES ARE OK                         
         BE    FSYSF10                                                          
*                                                                               
FSYSF03  LA    R2,32(R2)                                                        
         BCT   RE,FSYSF02          NEXT FILE                                    
*                                                                               
FSYSF090 OC    ASYSHDR,ASYSHDR     ARE WE ON AN INDIVIDUAL SYS                  
         BNZ   FSYSF099                                                         
*                                                                               
         L     R2,FULL             SYSTEM POINTER                               
         AHI   R2,64                                                            
         C     R2,SYSMAX           NEXT UNTIL MAX                               
         ST    R2,FULL             SYSTEM POINTER                               
         BL    FSYSF01                                                          
*                                                                               
FSYSF099 XC    ADTF,ADTF           NOT FOUND CLEAR DTF                          
         XC    FFLAGS,FFLAGS                                                    
         SAC   0                                                                
         B     EXITNEQ                                                          
*                                                                               
FSYSF10  MVC   FILNUM,DSFILNUM                                                  
         OC    ASYSHDR,ASYSHDR     DID WE HAVE SPECIFIC SYSTEM                  
         BNZ   FSYSF200                                                         
*                                                                               
         L     R2,FULL             SAVE ASYSHDR                                 
         ST    R2,ASYSHDR                                                       
         USING DMSPACED,R2                                                      
         MVC   SYNAME+1(7),DSPNAME                                              
         MVI   SYNAME,7                                                         
         XC    SYNUM,SYNUM                                                      
         SRL   R2,6                                                             
         STC   R2,SYNUM+1                                                       
*                                                                               
FSYSF200 SAC   0                                                                
         B     EXITEQU                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY JOB DETAILS                                                 *         
***********************************************************************         
DISJOB   MVC   DUB2,SPACES                                                      
         TM    FULL+2,X'01'        TEST OFFLINE JOB                             
         BO    DJOB085                                                          
*                                                                               
         CLI   FULL+3,0                                                         
         BE    *+14                                                             
         MVC   FULL+0(1),FULL+3                                                 
         MVI   FULL+1,0                                                         
*                                                                               
         MVC   BYTE,FULL+0                                                      
         L     R1,AFACTAB                                                       
         NI    BYTE,X'0F'                                                       
DJOB081  CLC   BYTE,4(R1)          FIND ADV SYSTEM                              
         BE    DJOB082                                                          
         LA    R1,8(R1)                                                         
         CLI   5(R1),X'FF'         CHECK EOT                                    
         BNE   DJOB081                                                          
         DC    H'0'                                                             
DJOB082  MVC   DUB1(4),0(R1)                                                    
         CLI   DUB1+3,C' '                                                      
         BE    *+14                                                             
         MVC   DUB1+2(1),DUB1+3                                                 
         MVI   DUB1+3,C' '                                                      
*                                                                               
         MVC   DUB1+4(2),=C'  '                                                 
         MVC   DUB1+6(1),FULL+1                                                 
         TM    FULL+0,X'F0'                                                     
         BZR   RE                                                               
         SR    R1,R1                                                            
         IC    R1,FULL+0                                                        
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,DUB1+3                                                        
         BR    RE                                                               
*                                                                               
DJOB085  SR    R3,R3               SET TO DATASPACE                             
         USING DMDSHDR,R3                                                       
         LAM   AR3,AR3,SSBALET                                                  
         L     R3,DHAJOBS          SET R3 TO JOBS BLOCK                         
         USING DSJOBD,R3                                                        
*                                                                               
         LA    R0,DSJOBMXQ                                                      
DJOB090  CLI   0(R3),0             EMPTY ADV ENTRY                              
         BE    DJOB100                                                          
         CLC   FULL(2),DSJASID     MATCH ON ASID                                
         BNE   DJOB100                                                          
         MVC   DUB1,DSJNAM         MOVE IN JOBNAME                              
         MVI   DUB2,C'J'                                                        
         ST    R0,SAVER0           EDIT USES R0, SO SAVE IT                     
         EDIT  (B2,DSJNUM),(5,DUB2+1)                                           
         L     R0,SAVER0                                                        
*                                                                               
DJOB100  LA    R3,DSJOBLNQ(,R3)                                                 
         BCT   R0,DJOB090          ALL JOBS DONE                                
*                                                                               
         BR    RE                                                               
         DROP  R3                                                               
***********************************************************************         
* EDIT TUS IN FULL TO WORK HH:MM:SS.SS                                *         
***********************************************************************         
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         EDIT  (RE),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* FIND ASENTRY FROM BYTE=NUMBER                                       *         
***********************************************************************         
FINDNUM  NTR1                                                                   
         L     R6,=V(SELIST)       START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
FINDN10  CLC   SESYS,BYTE          FIND SELECTED SYS                            
         BE    FINDNX                                                           
*                                                                               
         BXLE  R6,R4,FINDN10                                                    
         LTR   RB,RB               SET CC NEQ (NOT FOUND)                       
         B     EXIT                                                             
FINDNX   ST    R6,ASENTRY          SAVE ASENTRY                                 
         MVC   APGMS,SEPGMS                                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIND ADTF FOR FILE IN FILNUM                                        *         
***********************************************************************         
FINDFIL  NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R4,SEFILES                                                       
         L     R4,0(R4)            R4=A(SYSFLES)                                
         USING SYSFLSTD,R4                                                      
         LH    R0,SYSF#FLS         R0=NUMBER OF FILES                           
         LA    R4,SYSFLIST         R4=A(FILE LIST ENTRY)                        
*                                                                               
FINDF01  CLC   FILNUM,SYSFILE#                                                  
         BE    FINDF02                                                          
*                                                                               
         LA    R4,SYSFLNQ(R4)      BUMP TO NEXT FILE                            
         BCT   R0,FINDF01                                                       
         DC    H'0'                                                             
*                                                                               
FINDF02  MVC   FFLAGS,SYSFIND1     SAVE FILE FLAGS                              
         SR    R6,R6                                                            
         ICM   R6,7,SYSFADTF                                                    
         ST    R6,ADTF             SAVE A(DTF)                                  
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*************************************************************                   
*        VALIDATE OPER INPUT                                *                   
*************************************************************                   
VALCARD  NTR1                                                                   
*                                                                               
         XC    ACTNUM,ACTNUM       CLEAR RETURNED VALUES                        
         XC    ADNAME,ADNAME                                                    
         XC    ADNUM,ADNUM                                                      
         XC    JOBNAME,JOBNAME                                                  
         XC    SYNAME,SYNAME                                                    
         XC    SYNUM,SYNUM                                                      
         XC    ASYSHDR,ASYSHDR                                                  
         XC    FFLAGS,FFLAGS                                                    
         XC    FILNUM,FILNUM                                                    
         XC    FILNAM,FILNAM                                                    
         XC    ADTF,ADTF                                                        
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(C'C',CARD),(8,SCANBLK),0                       
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)          TEST FOR NO INPUT                            
         BZ    VALCARDX                                                         
*                                                                               
         USING SCANBLKD,R2                                                      
VALC010  LA    R2,SCANBLK          POINT TO SCANNER BLOCK                       
         SR    R1,R1                                                            
         ICM   R1,1,SC1STLEN       ERROR IF NOT INPUT                           
         BZ    VALC022                                                          
*                                                                               
         USING FACOMMD,RF                                                       
         LA    RF,COMTAB           SEARCH COMTAB                                
VALC020  CLM   R1,1,FACOMINL       MINIMUM LENGTH                               
         BL    VALC021             REJECT IF < MIN LENGTH                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FACOMAND(0),SC1STFLD    COMPARE COMMAND TEXT                     
         BE    VALC023                                                          
         LA    R1,1(R1)            ADD BACK FOR BCTR                            
VALC021  AHI   RF,FACOMLNQ         NEXT ENTRY                                   
         CLI   FACOACT#,X'FF'      EOT ?                                        
         BNE   VALC020             NO                                           
*                                                                               
VALC022  MVC   COMMAND,SC1STFLD    SAVE COMMAND TEXT                            
         LTR   RB,RB               RETURN CC NEQ                                
         B     EXITEQU                                                          
*                                                                               
VALC023  MVC   ACTNUM,FACOACT#     SAVE ACTION NUMBER                           
         MVC   FLAG,FACOFLAG                                                    
         CLC   ACTNUM,=H'35'       STATE CHANGE DO SE NEXT                      
         JE    VALC200                                                          
         B     VALC100                                                          
         DROP  RF                                                               
         EJECT                                                                  
************************************************************                    
*        VALIDATE ADV NAME                                 *                    
************************************************************                    
VALC100  LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD (ADV NAME)                   
         BCT   R3,*+8                                                           
         B     VALCARDX                                                         
*                                                                               
         CLI   SC1STLEN,0          NO ADV NAME IS OK                            
         BE    VALC200             <== DISABLE SPECIFICS                        
         CLI   SC1STLEN,2                                                       
         BE    VALC180                                                          
         CLI   SC1STLEN,3          4 IS ADVN                                    
         BNL   *+12                                                             
         CLI   SC1STLEN,1          1 IS ADVCHR                                  
         BNE   VALC190                                                          
*                                                                               
         USING FACITABD,R1                                                      
         L     R1,AFACTAB          GET FACID TAB                                
VALC110  CLI   SC1STLEN,1                                                       
         BNE   *+14                                                             
         CLC   FACISN1,SC1STFLD    1 CHR TEST                                   
         B     *+10                                                             
         CLC   FACISN4,SC1STFLD    4 CHR TEST                                   
         BE    VALC120                                                          
         AHI   R1,L'FACITAB        NEXT ENTRY                                   
         CLI   0(R1),X'FF'         TEST EOT                                     
         BNE   VALC110                                                          
         B     VALC180                                                          
*                                                                               
VALC120  MVC   ADNUM,FACIID        SAVE ADV NUM & NAME                          
         MVC   ADNAME,FACISN4                                                   
         B     VALC200                                                          
*                                                                               
VALC180  MVC   JOBNAME,SC1STFLD    MUST BE JOBNAME THEN                         
         MVC   JOBNAMEL,SC1STLEN                                                
         B     VALC200                                                          
         DROP  R1                                                               
*                                                                               
VALC190  LA    R1,4                UNKNOWN ADV SYSTEM                           
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     XBASE                                                            
         EJECT                                                                  
************************************************************                    
*        VALIDATE SE SYSTEM NAME OR RESOURCE               *                    
************************************************************                    
VALC200  TM    FLAG,FACORSN        TEST RESOURCE REQUIRED                       
         BO    VALC250                                                          
         TM    FLAG,FACOSYPG       TEST SYS/PGM REQUIRED                        
         BO    VALC600                                                          
*                                                                               
VALC210  LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD (SE SYS)                     
         BCT   R3,*+8                                                           
         B     VALCARDX                                                         
*                                                                               
         CLC   =C'ALL',SC1STFLD    ALLOW "ALL"                                  
         BNE   VALC220                                                          
         XC    SYNUM,SYNUM                                                      
         CLC   ACTNUM,=H'15'       STATUS,,ALL SET FFFF                         
         JNE   VALC300                                                          
         MVC   SYNUM,=X'FFFF'                                                   
         J     VALC300                                                          
*                                                                               
VALC220  XC    SYGROUP,SYGROUP                                                  
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALC300             NO SYSTEM IS OK FOR NOW                      
         CLI   SC1STLEN,4                                                       
         BL    VALC240             BUT < 4 CHRS FOR COMPARE IS NOT              
         STC   R1,SYNAME                                                        
         MVC   SYNAME+1(7),SC1STFLD                                             
         BRAS  RE,FINDSYS                                                       
         BE    VALC300                                                          
*                                                                               
VALC240  LA    R1,5                UNKNOWN SYSTEM                               
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     XBASE                                                            
         EJECT                                                                  
************************************************************                    
*        VALIDATE RESOURCE NAME                            *                    
************************************************************                    
VALC250  LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD (SE SYS)                     
         BCT   R3,*+8                                                           
         B     VALCARDX                                                         
*                                                                               
         XC    RESOURCE,RESOURCE                                                
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALC270             NO RESOURCE NAMED                            
         CLI   SC1STLEN,4                                                       
         BL    VALC270             < 4 CHRS FOR COMPARE                         
         BCTR  R1,0                                                             
*                                                                               
         LA    RF,RESRCTBL         FIND RESOURCE IN TABLE                       
VALC052  CLI   0(RF),X'FF'                                                      
         BE    VALC270             EOT - RESOURCE NOT FOUND                     
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SC1STFLD    CHECK RESOURCE NAME                          
         BE    *+12                                                             
         LA    RF,10(RF)           NEXT TABLE ENTRY                             
         B     VALC052                                                          
*                                                                               
         MVC   RESOURCE,8(RF)      SAVE RESOURCE NUMBER                         
         B     VALC300                                                          
*                                                                               
VALC270  LA    R1,10               UNKNOWN RESOURCE                             
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     XBASE                                                            
         EJECT                                                                  
************************************************************                    
*        VALIDATE FILENAME                                 *                    
************************************************************                    
VALC300  CLC   ACTNUM,=H'12'       BROADCAST GOTO VALC400                       
         BE    VALC400                                                          
         CLC   ACTNUM,=H'35'       STATE GOTO VALC500                           
         BE    VALC500                                                          
         LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD (FILENAME)                   
         BCT   R3,*+8                                                           
         B     VALCARDX                                                         
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALCARDX            NO FILENAME OK FOR NOW                       
         CLI   SC1STLEN,4                                                       
         BL    VALC390             BUT < 4 CHRS FOR COMPARE IS NOT              
*                                                                               
         MVC   FILNAM,SC1STFLD                                                  
         BRAS  RE,FSYSFIL          VALIDATE FILENAME                            
         BE    VALCARDX                                                         
*                                                                               
VALC390  LA    R1,6                UNKNOWN FILENAME                             
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     XBASE                                                            
*                                                                               
VALCARDX CR    R1,R1               EXIT EQU                                     
         B     EXITEQU                                                          
         EJECT                                                                  
************************************************************                    
*        VALIDATE BROADCAST                                *                    
************************************************************                    
VALC400  LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD (FILENAME)                   
         BCT   R3,*+8                                                           
         B     VALC490                                                          
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALC490             NO BROADCAST                                 
         LA    RF,BRDTABL                                                       
         BCTR  R1,0                                                             
*                                                                               
VALC410  EX    R1,*+8              SCAN THE TABLE FOR BROADCAST EQU             
         B     *+10                                                             
         CLC   0(0,RF),SC1STFLD                                                 
         BE    VALC420                                                          
         LA    RF,8(RF)                                                         
         CLI   7(RF),X'FF'                                                      
         BNE   VALC410                                                          
         B     VALC490                                                          
*                                                                               
VALC420  MVC   BRDNUM,7(RF)        SAVE THE NUMBER                              
         B     VALCARDX                                                         
*                                                                               
VALC490  LA    R1,8                UNKNOWN BROADCAST                            
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     XBASE                                                            
         EJECT                                                                  
************************************************************                    
*        VALIDATE STATE CHANGE                             *                    
************************************************************                    
VALC500  LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD STATE                        
         BCT   R3,*+8                                                           
         B     VALC590                                                          
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALC590                                                          
         LA    RF,STATABLE                                                      
         BCTR  R1,0                                                             
*                                                                               
VALC510  EX    R1,*+8              SCAN THE TABLE FOR STATE                     
         B     *+10                                                             
         CLC   0(0,RF),SC1STFLD                                                 
         BE    VALC520                                                          
         LA    RF,8(RF)                                                         
         CLI   7(RF),X'FF'                                                      
         BNE   VALC510                                                          
         B     VALC590                                                          
*                                                                               
VALC520  MVC   STACHA,0(RF)        SAVE THE ACTION                              
         B     VALCARDX                                                         
*                                                                               
VALC590  LA    R1,23               UNKNOWN STATE                                
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     XBASE                                                            
         EJECT                                                                  
************************************************************                    
*        VALIDATE SYS PRG                                  *                    
************************************************************                    
VALC600  LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD STATE                        
         BCT   R3,*+8                                                           
         B     VALC690                                                          
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALC690                                                          
         BCTR  R1,0                                                             
*                                                                               
         LA    R6,SYSLST           R2=SYSLST                                    
         LH    RE,0(R6)                                                         
         L     RF,2(R6)                                                         
         AHI   R6,6                                                             
         USING SYSLSTD,R6                                                       
*                                                                               
VALC605  EX    R1,*+8              TEST SHORT NAME                              
         J     *+10                                                             
         CLC   SYSLSHRT(0),SC1STFLD                                             
         JE    VALC620                                                          
         BXLE  R6,RE,VALC605                                                    
         J     VALC690                                                          
*                                                                               
VALC620  MVC   SYNUM+1(1),SYSLNUM  SAVE THE SYS NUMBER                          
*                                                                               
         L     R6,=V(SELIST)                                                    
         LH    RE,0(R6)                                                         
         L     RF,2(R6)                                                         
         AHI   R6,6                                                             
         USING SELISTD,R6                                                       
*                                                                               
VALC625  CLC   SYNUM+1(1),SEOVSYS  TEST SYNUM WITH OVSYS                        
         BE    VALC630                                                          
*                                                                               
         BXLE  R6,RE,VALC625       NEXT                                         
         B     VALC690                                                          
*                                                                               
VALC630  MVC   APGMS,SEPGMS        SAVE A(PGMS)                                 
         ICM   R6,15,APGMS                                                      
         BZ    VALC690                                                          
*                                                                               
         LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD MUST BE PGM                  
         BCT   R3,*+8                                                           
         B     VALC690                                                          
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALC690                                                          
         BCTR  R1,0                                                             
*                                                                               
         LH    RE,0(R6)            SET BXLE                                     
         L     RF,2(R6)                                                         
         AHI   R6,6                                                             
         USING PGMLSTD,R6                                                       
*                                                                               
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   PGMNAME(0),SC1STFLD PROG NAME                                    
         JE    VALC640                                                          
         BXLE  R6,RE,*-10          NEXT                                         
         B     VALC690                                                          
*                                                                               
VALC640  MVC   FILNUM,PGMNUM       SAVE PGMNUM IN FILENUM SLOT                  
         J     VALCARDX                                                         
*                                                                               
VALC690  LA    R1,24               UNKNOWN SYS PRG                              
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     XBASE                                                            
         EJECT                                                                  
************************************************************                    
*        ERRORS                                            *                    
************************************************************                    
ERR1     MVC   OPFILID,=CL8'IS BUSY'                                            
         MVC   OPCMND,=CL8'NO EOJ  '                                            
         BRAS  RE,WTO                                                           
         B     XBASE                                                            
*                                                                               
ERRX     LA    R1,1                UNKNOWN COMMAND                              
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTOE                                                          
         B     XBASE                                                            
         EJECT                                                                  
******************************************************                          
*        WRITE TO OPERATOR                           *                          
******************************************************                          
WTO      NTR1                                                                   
         MVC   OPLEN,=H'42'                                                     
         WTO   TEXT=OPLEN                                                       
         B     EXITEQU                                                          
*                                                                               
WTOE     NTR1                                                                   
         MVC   OPLEN,=H'42'                                                     
         WTO   TEXT=OPLEN                                                       
         MVI   RTERR,4                                                          
         B     EXITEQU                                                          
*                                                                               
WTOR     NTR1                                                                   
         XC    ECBAD,ECBAD                                                      
         MVC   OPLEN,=H'50'                                                     
         WTOR  TEXT=(OPLEN,REPLY,1,ECBAD,ROUTCDE=(1))                           
         WAIT  ECB=ECBAD                                                        
         B     EXITEQU                                                          
*                                                                               
OPLEN    DS    H                                                                
*                                                                               
OPMSG    DS    0CL80  '+DDOPER+ SSSSSSSS FFFFFFFF CCCCCCCCCC                    
OPFACID  DS    CL8                                                              
         DS    CL1                                                              
OPMSGOPS DS    0CL31                                                            
OPMSGTXT DS    0CL70                                                            
OPSYSID  DS    CL8                                                              
         DS    CL1                                                              
OPFILID  DS    CL8                                                              
         DS    CL1                                                              
OPCMND   DS    CL10                                                             
         DS    CL3                                                              
OPEXTRA  DS    CL39                                                             
         ORG   OPMSG                                                            
OPTSKID  DS    CL2                 T#                                           
         DS    CL1                                                              
OPTSKWT  DS    CL2                 T# - WAITING ON TASK                         
         DS    CL1                                                              
OPTSKLU  DS    CL8                                                              
         DS    CL1                                                              
OPCMND2  DS    0CL10               ALTERNATE AREA TO SHOW COMMAND               
OPTSKSP  DS    CL10                                                             
         ORG                                                                    
OPMSGL   EQU   *-OPMSG                                                          
         EJECT                                                                  
         EJECT                                                                  
*************************************************************                   
*        RESOURCE TABLE                                    *                    
*************************************************************                   
RESRCTBL DC    CL8'MEDDSPC ',AL2(0001)                                          
         DC    CL8'MEDDSPC1',AL2(0002)                                          
         DC    CL8'MEDDSPC2',AL2(0003)                                          
         DC    CL8'MEDDSPC3',AL2(0004)                                          
         DC    CL8'MEDDSPC4',AL2(0005)                                          
         DC    CL8'MEDDSPC5',AL2(0006)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
*************************************************************                   
*        BROADCAST TABLE                                   *                    
*************************************************************                   
BRDTABL  DC    CL7'RDWEEK ',AL1(03)                                             
         DC    CL7'RDSAT  ',AL1(04)                                             
         DC    CL7'RDSUN  ',AL1(05)                                             
         DC    CL7'READ10 ',AL1(06)                                             
         DC    CL7'RDONLY ',AL1(07)                                             
         DC    CL7'XXXXXXX',AL1(255)                                            
*************************************************************                   
*        STATE TABLE                                       *                    
*************************************************************                   
STATABLE DC    CL7'OPEN   ',AL1(01)                                             
         DC    CL7'READ   ',AL1(02)                                             
         DC    CL7'CLOSE  ',AL1(03)                                             
         DC    CL7'USER   ',AL1(04)                                             
         DC    CL7'XXXXXXX',AL1(255)                                            
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        OPERATOR MESSAGES                                 *                    
*************************************************************                   
GETMSG   NTR1                                                                   
         BCTR  R1,0                                                             
         SLL   R1,5                                                             
         LA    R1,OPER1(R1)                                                     
         MVC   OPMSGOPS,0(R1)                                                   
         MVC   OPFACID,MVSNAME                                                  
         J     EXITEQU                                                          
*                                                                               
OPER1    DC    CL32'UNKNOWN COMMAND'                                            
OPER2    DC    CL32'EOJ COMMAND ACCEPTED'                                       
OPER3    DC    CL32'FORCED END OF JOB   '                                       
OPER4    DC    CL32'UNKNOWN ADV SYSTEM  '                                       
OPER5    DC    CL32'UNKNOWN SE SYSTEM   '                                       
OPER6    DC    CL32'UNKNOWN FILENAME    '                                       
OPER7    DC    CL32'........ J000000 HAS NOT REPLIED'                           
OPER8    DC    CL32'UNKNOWN BROADCAST      '                                    
OPER9    DC    CL32'MISSING DATA           '                                    
OPER10   DC    CL32'UNKNOWN / MISSING RESOURCE '                                
OPER11   DC    CL32'UNABLE TO ACCESS MEDDSPACE '                                
OPER12   DC    CL32'MED DSPACE ENABLED  '                                       
OPER13   DC    CL32'MED DSPACE DISABLED'                                        
OPER14   DC    CL32'XX COMMANDS OUTSTANDING'                                    
OPER15   DC    CL32'COMMAND          COMPLETED'                                 
OPER16   DC    CL32'STARTTTS REQUESTED       '                                  
OPER17   DC    CL32'NOT AUTHORIZED ON THIS ADV'                                 
OPER18   DC    CL32'SSB STOP - USER INPUT INHIBITED'                            
OPER19   DC    CL32'SSB GO - USER INPUT ENABLED'                                
OPER20   DC    CL32'RESET SIN AND DATE TO DD/MM/YY'                             
OPER21   DC    CL32'NEWDAY          SET TO DDMMM'                               
OPER22   DC    CL32'NO FILE SPECIFIED           '                               
OPER23   DC    CL32'UNKNOWN STATE               '                               
OPER24   DC    CL32'UNKNOWN SYS/PRG             '                               
*                                                                               
OPERBUSY DC    C'........ J...... IS BUSY RETRY/IGNORE  '                       
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS & LTORG                                            *         
***********************************************************************         
ARZERO   DC    16F'0'                                                           
FFS      DC    X'FFFFFFFF'                                                      
RTERR    DC    XL1'00'                                                          
SPACES   DC    CL80' '                                                          
TUHOUR   EQU   38400*60*60                                                      
*                                                                               
         LTORG                                                                  
MVSNAME  DC    CL8' '                                                           
MVSJNUM  DC    XL2'0000'           NUMERIC PART OF JOBID                        
MVSJOB   DC    CL8'JOB00000'       JOB ID                                       
*                                                                               
ECBAD    DC    F'0'                WTOR FIELDS                                  
REPLY    DC    CL4' '                                                           
         EJECT                                                                  
       ++INCLUDE FACOMTAB                                                       
       ++INCLUDE FASYSLST                                                       
       ++INCLUDE FACIDTABL                                                      
         EJECT                                                                  
*************************************************************                   
*        SSB                                                *                   
*************************************************************                   
         DS    0F                                                               
SSB      DC    H'0',X'FF',X'14',1022X'00'                                       
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
ASYSHDR  DS    A                                                                
SYSMAX   DS    A                   UPPER ADDRESS FOR SYSTEM SEARCH              
AFACTAB  DS    A                   A(FACIDTAB BASED ON DSPACE)                  
SAVER0   DS    A                                                                
*                                                                               
ASENTRY  DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
*                                                                               
BYTE1    DS    X                                                                
*                                                                               
TODAY    DS    F                                                                
TOMORROW DS    F                                                                
*                                                                               
COMMAND  DS    CL10                                                             
HEADER   DS    X                                                                
*                                                                               
ACTNUM   DS    H                   OPER ACTION NUMBER                           
*                                                                               
ADNAME   DS    XL3                 ADV NAME                                     
ADNUM    DS    XL1                 ADV NUMBER                                   
*                                                                               
JOBNAME  DS    CL8                 JOBNAME MATCH                                
JOBNAMEL DS    X                   JOBNAME LENGTH                               
*                                                                               
SYNAME   DS    CL8                 SYS NAME                                     
SYNUM    DS    XL2                 SYS NUM                                      
SYGROUP  DS    A                   A(GROUP LIST)                                
RESOURCE DS    XL2                 RESOURCE NUMBER                              
APGMS    DS    A                   A(PROGRAMS)                                  
*                                                                               
DISPLOOP DS    F                                                                
DISPFLAG DS    C                                                                
ONLINE   DS    C                   SET TO Y IF =OPER                            
*                                                                               
OVSYS    DS    C                                                                
*                                                                               
WAIT     DS    X                   WAIT 1 TO 9                                  
WAITTIME DS    F                                                                
LASTWAIT DS    F                                                                
*                                                                               
FFLAGS   DS    XL2                 FILE FLAGS                                   
FILNUM   DS    X                   FILE EXT NUMBER                              
         ORG   *-1                                                              
BRDNUM   DS    X                   BROADCAST NUMBER                             
FILNAM   DS    CL8                 FILE NAME                                    
STACHA   DS    CL4                 STATE CHANGE                                 
ADTF     DS    A                   DTF FOR FILE                                 
*                                                                               
SAVEF01  DS    4A                                                               
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
COACTN   DS    CL14                                                             
COSPACE  DS    CL64                                                             
         ORG   COSPACE                                                          
         DS    CL1                                                              
CODEST   DS    CL8                                                              
         DS    CL1                                                              
CODESTN  DS    CL8                                                              
         DS    CL1                                                              
COCOMM   DS    CL32                                                             
         ORG                                                                    
*                                                                               
         DS    0F                                                               
SCANBLK  DS    CL256                                                            
*                                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
MYWORK   DS    CL64                                                             
CARD     DS    CL80                                                             
CARDWORK DS    CL80                                                             
*                                                                               
ACOMMS   DS    AL4                 POINTER                                      
NCOMMS   DS    AL4                 COUNTER                                      
COMMS    DS    400AL4              LIST OF COMMANDS                             
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        DSECTS                                             *                   
*************************************************************                   
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DMSYSFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMSYSFD                                                        
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMDSYSHDR                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMDSYSHDR                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FAPGMLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FACOMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACOMTABD                                                      
         PRINT ON                                                               
         IHAASCB LIST=YES                                                       
         PUSH ACONTROL                                                          
         ACONTROL COMPAT(NOCASE),FLAG(NOPAGE0),TYPECHECK(NOREGISTER)            
         IHAASSB LIST=YES                                                       
         POP  ACONTROL                                                          
         IAZJSAB LIST=YES                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDOPER    09/05/17'                                      
         END                                                                    
