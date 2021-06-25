*          DATA SET DMENQTEST  AT LEVEL 004 AS OF 11/30/11                      
*PHASE ENQTESTA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE STXITER                                                                
         TITLE 'ENQTEST - PROGRAM TO TEST ENQ/DEQ MODULE'                       
         PRINT NOGEN                                                            
ENQTEST  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         ENTRY TICTOC                                                           
         ENTRY LOCKER                                                           
         ENTRY ADWAIT                                                           
         NBASE 0,**ENQT**,RA,WORK=A(ENQWORK)                                    
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
         EJECT ,                                                                
***********************************************************************         
* TEST ENQUEUES                                                                 
* AHYD NOVEMBER 30TH, 2011 - CONVERTED TO USE DDSIO                             
*                                                                               
***********************************************************************         
ENQNEXT  GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    ENQEOJ                                                           
         CLI   C,C'*'              IGNORE COMMENT CARDS                         
         BE    ENQNEXT                                                          
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),DMCB,PCC,=C'BL02'                                      
         MVI   P,C'-'              PRINT LINE OF HYPHENS                        
         MVC   P+1(79),P                                                        
         GOTO1 =V(PRINT),DMCB,PCC,=C'BL01'                                      
         MVC   P(80),C             PRINT CARD                                   
         GOTO1 =V(PRINT),DMCB,PCC,=C'BL01'                                      
         MVC   P,SPACES                                                         
*                                                                               
         CLC   C(7),=CL8'DSPACE='  DSPACE=X TO SET THE DATA SPACE               
         BNE   ENQCARD2                                                         
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),C+7                                        
         B     ENQNEXT                                                          
*                                                                               
ENQCARD2 CLC   =C'DDSIO=',C        DDSIO=DDSIOX                                 
         BNE   ENQCARD3                                                         
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),C+6                                                      
         B     ENQNEXT                                                          
*                                                                               
ENQCARD3 CLC   C(4),=C'DUMP'       SPECIAL TO CAUSE DUMP                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   C(4),=C'TASK'       SPECIAL FOR TASK SIMULATION                  
         BE    ENQTASK                                                          
         CLC   C(5),=C'MULTI'      SPECIAL TO SET OFFLINE MULTI TASKING         
         BE    ENQMULTI                                                         
         CLC   C(4),=C'OPEN'       SPECIAL TO OPEN FILES                        
         BE    ENQOPEN                                                          
         CLC   C(5),=C'CLOSE'      SPECIAL TO CLOSE FILES                       
         BE    ENQCLOSE                                                         
         B     ENQ                                                              
*                                                                               
ENQTASK  MVC   TASK,C+4            ONLINE TASK SIMULATION IF TASKN              
         NI    TASK,X'0F'                                                       
         L     RF,=A(TSK1)                                                      
         CLI   TASK,1                                                           
         BE    *+8                                                              
         L     RF,=A(TSK2)                                                      
         L     RE,=A(SSB)          SET SSB TO ONLINE FOR TASK 1/2               
         MVI   1(RE),1                                                          
         ST    RF,20(RE)                                                        
         B     ENQNEXT                                                          
*                                                                               
ENQMULTI L     RE,=A(SSB)          SET OFFLINE MULTI TASKING FLAGS              
         MVI   2(RE),X'FF'                                                      
         OI    4(RE),X'80'                                                      
         B     ENQNEXT                                                          
*                                                                               
ENQOPEN  XC    DMCB(24),DMCB       SET DADDS PARAM LIST FOR OPEN                
         LA    R1,DMCB                                                          
*                                                                               
ENQOPQ1  CLC   C+5(4),=C'PRTQ'     OPEN PRTQX IF REQUIRED                       
         BNE   ENQOPQ1X                                                         
         L     RF,=V(DATAMGR)      INIT CALL WILL DO THIS                       
         L     R0,=A(CTBUFF)                                                    
         GOTO1 (RF),DMCB,=C'INIT',C+5,CTKEY,CTREC,(R0)                          
ENQOPQ1X EQU   *                                                                
*                                                                               
ENQOWF1  CLC   C+5(4),=C'WRKF'     OPEN WRKFX IF REQUIRED                       
         BNE   ENQOWF1X                                                         
         L     RF,=V(DATAMGR)      INIT CALL WILL DO THIS                       
         L     R0,=A(CTBUFF)                                                    
         GOTO1 (RF),DMCB,=C'INIT',C+5,CTKEY,CTREC,(R0)                          
ENQOWF1X EQU   *                                                                
*                                                                               
ENQOWZ1  CLC   C+5(4),=C'WRKZ'     OPEN WRKZX IF REQUIRED                       
         BNE   ENQOWZ1X                                                         
         L     RF,=V(DATAMGR)      INIT CALL WILL DO THIS                       
         L     R0,=A(CTBUFF)                                                    
         GOTO1 (RF),DMCB,=C'INIT',C+5,CTKEY,CTREC,(R0)                          
ENQOWZ1X EQU   *                                                                
*                                                                               
         XC    DMCB,DMCB                                                        
         MVI   P3+3,X'01'          SERVICE SYSTEM                               
         GOTO1 =V(DATAMGR),DMCB,DMREAD,=C'SYSFLES'                              
         LT    RE,P4                                                            
         JZ    ENQOCTL                                                          
*                                                                               
         USING SYSFLSTD,RE                                                      
         XR    R0,R0                                                            
         ICM   R0,3,SYSF#FLS       NUMBER OF FILES                              
         LA    RE,SYSFLIST                                                      
ENQOWK02 CLI   SYSFILE#,X'F4'      WKFILE                                       
         BNE   ENQOWK03                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,AWKFILE                                                       
                                                                                
ENQOWK03 CLI   SYSFILE#,X'FA'      FACWRK                                       
         BNE   ENQOWK06                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,AFACWRK                                                       
                                                                                
ENQOWK06 LA    RE,SYSFLNQ(,RE)                                                  
         BCT   R0,ENQOWK02                                                      
         DROP  RE                                                               
*                                                                               
         L     RF,=V(DADDS)                                                     
ENQOWRK  CLC   C+5(6),=C'WKFILE'   OPEN WKFILE IF REQUIRED                      
         BNE   ENQOWRKX                                                         
         LT    RE,AWKFILE                                                       
         BZ    ENQOWRKX                                                         
         CLC   C+12(7),SPACES      OPEN WKFILE XXXXXXX                          
         BE    *+10                                                             
         MVC   22(7,RE),C+12       OVERRIDE STANDARD FILE ID                    
         ST    RE,P4                                                            
         MVC   P1,=A(DAOPEN)                                                    
         BASR  RE,RF                                                            
         MVC   P1,=A(DACPUID)      DO DUMMY I/O TO FIX EXTENTS                  
         BASR  RE,RF                                                            
ENQOWRKX EQU   *                                                                
*                                                                               
ENQOFAC  CLC   C+5(6),=C'FACWRK'   OPEN FACWRK IF REQUIRED                      
         BNE   ENQOFACX                                                         
         LT    RE,AFACWRK                                                       
         BZ    ENQOFACX                                                         
         CLC   C+12(7),SPACES      OPEN WKFILE XXXXXXX                          
         BE    *+10                                                             
         MVC   22(7,RE),C+12       OVERRIDE STANDARD FILE ID                    
         ST    RE,P4                                                            
         MVC   P1,=V(DAOPEN)                                                    
         BASR  RE,RF                                                            
         MVC   P1,=V(DACPUID)      DO DUMMY I/O TO FIX EXTENTS                  
         BASR  RE,RF                                                            
ENQOFACX EQU   *                                                                
*                                                                               
ENQOCTL  CLC   C+5(6),=C'CTFILE'   OPEN=CTFILE CTFILE0CTRCVR CTRCVR0            
         BNE   ENQOCTLX                                                         
         XC    DMCB,DMCB                                                        
         MVI   P3+3,X'0A'          CONTROL SYSTEM                               
         GOTO1 =V(DATAMGR),DMCB,DMREAD,=C'SYSFLES'                              
         LT    RE,P4                                                            
         JZ    ENQOCTLX                                                         
*                                                                               
         USING SYSFLSTD,RE                                                      
         LA    R5,LIST                                                          
         LA    R6,C+5                                                           
         MVI   SYSFSTAT,SYSFQDP    SET TO ENQ/DEQ MODE PROTECTION               
*                                                                               
         L     R7,=A(UTL)                                                       
         MVI   4(R7),X'0A'                                                      
         XR    R0,R0                                                            
         ICM   R0,3,SYSF#FLS       NUMBER OF FILES                              
         LA    RE,SYSFLIST                                                      
ENQOCT02 CLI   SYSFILE#,X'A1'      CTFILE                                       
         BNE   ENQOCT03                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,ACTFILE                                                       
                                                                                
ENQOCT03 CLI   SYSFILE#,X'A4'      CTRCVR                                       
         BNE   ENQOCT04                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,ACTRCVR                                                       
*                                                                               
ENQOCT04 CLI   SYSFILE#,X'AE'      GENDIR                                       
         BNE   ENQOCT05                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,AGENDIR                                                       
*                                                                               
ENQOCT05 CLI   SYSFILE#,X'AF'      GENFIL                                       
         BNE   ENQOCT06                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,AGENFIL                                                       
*                                                                               
ENQOCT06 LA    RE,SYSFLNQ(,RE)                                                  
         BCT   R0,ENQOCT02                                                      
         DROP  RE                                                               
*                                                                               
         L     RE,ACTFILE                                                       
         MVC   22(7,RE),7(R6)      SET CTFILE NAME                              
         MVI   0(R5),C'U'                                                       
         MVC   1(7,R5),22(RE)                                                   
         LA    R5,8(R5)                                                         
         LA    R6,14(R6)                                                        
*                                                                               
         L     RE,ACTRCVR                                                       
         MVC   22(7,RE),7(R6)      SET CTRCVR NAME                              
         MVI   0(R5),C'U'                                                       
         MVC   1(7,R5),22(RE)                                                   
         LA    R5,8(R5)                                                         
         LA    R6,14(R6)                                                        
                                                                                
ENQOCTL1 CLI   0(R6),C' '          ANY MORE FILES                               
         BE    ENQOCTL2            NO                                           
         LT    RE,AGENDIR          GENDIR GENDIR0GENFIL GENFIL0                 
         BZ    ENQOCTL2                                                         
         MVC   22(7,RE),7(R6)      SET GENDIR NAME                              
         MVI   0(R5),C'U'                                                       
         MVC   1(7,R5),22(RE)                                                   
         LA    R5,8(R5)                                                         
         LA    R6,14(R6)                                                        
*                                                                               
         L     RE,AGENFIL                                                       
         MVC   22(7,RE),7(R6)      SET GENFIL NAME                              
         MVI   0(R5),C'U'                                                       
         MVC   1(7,R5),22(RE)                                                   
         LA    R5,8(R5)                                                         
         LA    R6,14(R6)                                                        
*                                                                               
ENQOCTL2 MVI   0(R5),C'X'                                                       
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',LIST,CTREC               
         MVI   CTOPEN,C'Y'                                                      
         XC    CTKEY,CTKEY                                                      
         MVI   CTKEY+24,1                                                       
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMREAD),CTFILE,CTKEY,CTREC               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
ENQOCTLX EQU   *                                                                
*&&UK                                                                           
ENQOMEZ  CLC   C+5(7),=C'MEDDIRZ'  OPEN=MEDDIRZMEDDIR?MEDFILZMEDFIL?            
         BNE   ENQOMEZX                 MEDRCVZMEDRCV?                          
         XC    DMCB,DMCB                                                        
         MVI   P3+3,X'14'          MEDZ                                         
         GOTO1 =V(DATAMGR),DMCB,DMREAD,=C'SYSFLES'                              
         LT    RE,P4                                                            
         JZ    ENQOMEZX                                                         
*                                                                               
         USING SYSFLSTD,RE                                                      
         LA    R5,LIST                                                          
         LA    R6,C+5                                                           
         MVI   SYSFSTAT,SYSFQDP    SET TO ENQ/DEQ MODE PROTECTION               
         L     R7,=A(UTL)                                                       
         MVI   4(R7),X'14'                                                      
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,SYSF#FLS       NUMBER OF FILES                              
         LA    RE,SYSFLIST                                                      
ENQOMZ02 CLI   SYSFILE#,X'41'      MEDDIRZ                                      
         BNE   ENQOMZ03                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,AMEDDIRZ                                                      
                                                                                
ENQOMZ03 CLI   SYSFILE#,X'42'      MEDFILZ                                      
         BNE   ENQOMZ04                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,AMEDFILZ                                                      
*                                                                               
ENQOMZ04 CLI   SYSFILE#,X'44'      MEDRCVZ                                      
         BNE   ENQOMZ06                                                         
         ICM   R7,15,SYSFADTF-1                                                 
         ST    R7,AMEDRCVZ                                                      
*                                                                               
ENQOMZ06 LA    RE,SYSFLNQ(,RE)                                                  
         BCT   R0,ENQOMZ02                                                      
         DROP  RE                                                               
*                                                                               
         L     RE,AMEDDIRZ                                                      
         MVC   22(7,RE),7(R6)      SET MEDDIRZ NAME                             
         MVI   0(R5),C'U'                                                       
         MVC   1(7,R5),22(RE)                                                   
         LA    R5,8(R5)                                                         
         LA    R6,14(R6)                                                        
         L     RE,AMEDFILZ                                                      
         MVC   22(7,RE),7(R6)      SET MEDFILZ NAME                             
         MVI   0(R5),C'U'                                                       
         MVC   1(7,R5),22(RE)                                                   
         LA    R5,8(R5)                                                         
         LA    R6,14(R6)                                                        
         L     RE,AMEDRCVZ                                                      
         MVC   22(7,RE),7(R6)      SET MEDRCVZ NAME                             
         MVI   0(R5),C'U'                                                       
         MVC   1(7,R5),22(RE)                                                   
         LA    R5,8(R5)                                                         
         LA    R6,14(R6)                                                        
*                                                                               
ENQOMEZ1 CLI   0(R6),C' '          ANY MORE FILES                               
         BE    ENQOMEZ2            NO                                           
                                                                                
ENQOMEZ2 MVI   0(R5),C'X'                                                       
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'MEDIA',LIST,CTREC                 
         MVI   MEOPEN,C'Y'                                                      
         XC    CTKEY,CTKEY                                                      
         MVI   CTKEY+19,1                                                       
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMREAD),MEDDIR,CTKEY,CTREC               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
ENQOMEZX EQU   *                                                                
*&&                                                                             
ENQOPEX  B     ENQNEXT                                                          
*                                                                               
ENQCLOSE EQU   *                                                                
ENQCCTL  CLC   C+6(6),=C'CTFILE'                                                
         BNE   ENQCCTLX                                                         
         GOTO1 =V(DATAMGR),DMCB,=C'DMCLSE',=C'CONTROL',LIST,CTREC               
         MVI   CTOPEN,C'N'                                                      
         B     ENQNEXT                                                          
ENQCCTLX EQU   *                                                                
*&&UK                                                                           
ENQCMEZ  CLC   C+6(7),=C'MEDDIRZ'                                               
         BNE   ENQCMEZX                                                         
         GOTO1 =V(DATAMGR),DMCB,=C'DMCLSE',=C'MEDIA',LIST,CTREC                 
         MVI   MEOPEN,C'N'                                                      
         B     ENQNEXT                                                          
ENQCMEZX EQU   *                                                                
*&&                                                                             
         B     ENQNEXT                                                          
*                                                                               
TICTOC   BR    RE                                                               
LOCKER   BR    RE                                                               
ADWAIT   BR    RE                                                               
*                                                                               
ENQEOJ   CLI   CTOPEN,C'Y'                                                      
         BNE   ENQEOJX                                                          
         GOTO1 =V(DATAMGR),DMCB,=C'DMCLSE',=C'CONTROL',LIST,CTREC               
ENQEOJX  XBASE                                                                  
         EJECT                                                                  
ENQ      XC    ENQP1(12),ENQP1     BUILD ENQDEQ PARAM LIST FROM CARD            
         LA    R2,C+1              RESOURCE                                     
         ST    R2,ENQP1                                                         
         MVC   ENQP1(1),C          DMENQDEQ ACTION                              
*                                                                               
         CLI   C,C'E'              ENQUEUE RESOURCE                             
         BE    ENQ1                                                             
         CLI   C,C'D'              DEQUEUE RESOURCE                             
         BE    ENQ1                                                             
         CLI   C,C'A'              ADD A NEW ENQUEUE RECORD                     
         BE    ENQ1                                                             
         CLI   C,C'F'              ENQUEUE RESOURCE IF FREE                     
         BE    ENQ1                                                             
         CLI   C,C'R'              READ ENQUEUE COUNT/KEY/DATA                  
         BE    ENQ1                                                             
         CLI   C,C'T'              TEST RESOURCE                                
         BE    ENQ1                                                             
         CLI   C,C'X'              DUMMY CALL                                   
         BE    ENQ1                                                             
         CLI   C,C'1'              READ CTRL RECORD                             
         BE    ENQ1                                                             
         CLI   C,C'3'              ADD CTRL RECORD                              
         BE    ENQ1                                                             
*&&UK                                                                           
         CLI   C,C'4'              READ MEDZ RECORD                             
         BE    ENQ1                                                             
         CLI   C,C'6'              ADD MEDZ RECORD                              
         BE    ENQ1                                                             
*&&                                                                             
         B     ENQ2                                                             
*                                                                               
ENQ1     BAS   RE,QDQ              SINGLE ACTION                                
         BAS   RE,PRTHDR           PRINT PARAMETER LIST                         
         BAS   RE,PRTTBL           PRINT ENQUEUE RECORD                         
         CLC   C+1(4),=C'CTRL'                                                  
         BNE   *+8                                                              
         BAS   RE,PRTCTL           PRINT CTRL RECORD                            
         CLC   C+1(4),=C'MEDZ'                                                  
         BNE   *+8                                                              
         BAS   RE,PRTCTL           PRINT MEDZ RECORD                            
         CLI   TASK,0                                                           
         BE    ENQNEXT                                                          
         L     R0,=A(SSB)                                                       
         GOTO1 VPRNTBL,DMCB,0,(R0),8,48,=C'2H'                                  
         L     R0,=A(TSK1)                                                      
         GOTO1 (RF),(R1),0,(R0),3,48,=C'2H'                                     
         L     R0,=A(TSK2)                                                      
         GOTO1 (RF),(R1),0,(R0)                                                 
         B     ENQNEXT                                                          
*                                                                               
ENQ2     CLI   C,C'W'              WAIT ACTION - WNNNN SECS                     
         BNE   ENQ3                                                             
         MVC   FULL,C+1                                                         
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         CVB   R1,DUB                                                           
         CHI   R1,1000                                                          
         BNH   *+8                                                              
         LA    R1,1                                                             
         ST    R1,TIM                                                           
         BAS   R9,SETTIM                                                        
         B     ENQNEXT                                                          
*                                                                               
ENQ3     CLI   C,C'M'              MULTIPLE MXXXX WNNNN RNNNN X                 
         BNE   ENQ4                                                             
         MVC   FULL,C+7            GET WAIT TIME                                
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         CVB   R1,DUB                                                           
         CHI   R1,1000                                                          
         BNH   *+8                                                              
         LA    R1,1                                                             
         ST    R1,WTIM                                                          
         MVC   FULL,C+13           GET REPEAT COUNT                             
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         CVB   R1,DUB                                                           
         CH    R1,=H'1000'                                                      
         BNH   *+8                                                              
         LA    R1,1                                                             
         ST    R1,REPEAT                                                        
*                                                                               
         BAS   R9,GETTIM                                                        
         MVC   RSTIM,TIM                                                        
         L     R5,REPEAT                                                        
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         LA    R5,1                                                             
*                                                                               
ENQ3A    MVI   ENQP1,C'E'          ENQUEUE                                      
         BAS   RE,QDQ                                                           
         CLI   C+18,C' '           TEST TO PRINT                                
         BNE   *+12                                                             
         BAS   RE,PRTHDR                                                        
         BAS   RE,PRTTBL                                                        
         MVC   TIM,WTIM            WAIT                                         
         BAS   R9,SETTIM                                                        
         MVI   ENQP1,C'D'          DEQUEUE                                      
         BAS   RE,QDQ                                                           
         CLI   C+18,C' '           TEST TO PRINT                                
         BNE   *+12                                                             
         BAS   RE,PRTHDR                                                        
         BAS   RE,PRTTBL                                                        
         BCT   R5,ENQ3A            REPEAT                                       
*                                                                               
ENQ3B    CLI   C+18,C' '           TEST TO PRINT ONE LOOP ENTRY                 
         BE    ENQNEXT                                                          
         L     R1,XTIM             SET STIM/XTIM/DTIM                           
         MVC   STIM,RSTIM                                                       
         S     R1,STIM                                                          
         ST    R1,DTIM                                                          
         BAS   RE,PRTHDR                                                        
         BAS   RE,PRTTBL                                                        
         B     ENQNEXT                                                          
*                                                                               
ENQ4     CLI   C,C'I'              INITIALISE ENQREC AND CONTROL                
         BNE   ENQ5                                                             
         XC    ENQP1(12),ENQP1     BUILD ENQDEQ PARAM LIST                      
         LA    R2,=C'ENQR'                                                      
         ST    R2,ENQP1                                                         
         MVI   ENQP1,C'A'          ADD NEW ENQUEUE RECORD                       
         BAS   RE,QDQ              SINGLE ACTION                                
         BAS   RE,PRTHDR           PRINT PARAMETER LIST                         
         BAS   RE,PRTTBL           PRINT ENQUEUE RECORD                         
         XC    ENQP1(12),ENQP1                                                  
         LA    R2,=C'CTRL'                                                      
         ST    R2,ENQP1                                                         
         MVI   ENQP1,C'3'          ADD NEW CTRL RECORD                          
         BAS   RE,QDQ              SINGLE ACTION                                
         BAS   RE,PRTHDR           PRINT PARAMETER LIST                         
         BAS   RE,PRTCTL           PRINT CTRL RECORD                            
*&&UK                                                                           
         XC    ENQP1(12),ENQP1                                                  
         LA    R2,=C'MEDZ'                                                      
         ST    R2,ENQP1                                                         
         MVI   ENQP1,C'6'          ADD NEW MEDZ RECORD                          
         BAS   RE,QDQ              SINGLE ACTION                                
         BAS   RE,PRTHDR           PRINT PARAMETER LIST                         
         BAS   RE,PRTCTL           PRINT MEDZ RECORD                            
*&&                                                                             
ENQ5     CLC   C(5),=C'%CTRL'      CONTROL FILE TESTING                         
         BNE   ENQ5X                                                            
ENQ5C    CLI   C+5,C'C'            %CTRLC FOR CONTROL FILE                      
         BNE   ENQ5G                                                            
         CLI   C+6,C'A'            TEST A FOR ADD OR U FOR UPDATE               
         BE    ENQ5C1                                                           
         XC    CTKEY,CTKEY                                                      
         MVI   CTKEY+24,1                                                       
         GOTO1 =V(DATAMGR),DMCB,(X'80',DMREAD),CTFILE,CTKEY,CTREC               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CTREC+27,X'10'      TURN ON/OFF STATUS BIT                       
         BZ    *+12                                                             
         NI    CTREC+27,255-X'10'                                               
         B     *+8                                                              
         OI    CTREC+27,X'10'                                                   
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMWRT),CTFILE,CTKEY,CTREC                
         CLI   DMCB+8,0                                                         
         BE    ENQ5X                                                            
         DC    H'0'                                                             
ENQ5C1   GOTO1 =V(HEXIN),DMCB,C+7,CTKEY,50                                      
         OC    DMCB+12(4),DMCB+12                                               
         BZ    ENQ5X               IGNORE INVALID HEX KEY VALUE                 
         MVC   CTREC+00(25),CTKEY                                               
         MVC   CTREC+25(02),=H'38'                                              
         MVC   CTREC+27(01),=X'80'                                              
         MVC   CTREC+28(02),=X'AA09'                                            
         MVC   CTREC+30(07),=C'ENQTEST'                                         
         MVI   CTREC+37,X'00'                                                   
         GOTO1 =V(DATAMGR),DMCB,(X'00',DMADD),CTFILE,CTKEY,CTREC                
         CLI   DMCB+8,0                                                         
         BE    ENQ5X                                                            
         DC    H'0'                                                             
ENQ5G    CLI   C+5,C'G'            %CTRLG FOR GENFIL                            
         BNE   ENQ5X                                                            
         CLI   C+6,C'A'            TEST A FOR ADD OR U FOR UPDATE               
         BE    ENQ5G1                                                           
         MVC   CTKEY(4),=X'00010101'                                            
         GOTO1 =V(DATAMGR),DMCB,(X'00',GETREC),GENFIL,CTKEY,CTREC,CTWRK         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CTREC+34,X'10'      TURN ON/OFF STATUS BIT                       
         BZ    *+12                                                             
         NI    CTREC+34,255-X'10'                                               
         B     *+8                                                              
         OI    CTREC+34,X'10'                                                   
         GOTO1 =V(DATAMGR),DMCB,(X'00',PUTREC),GENFIL,CTKEY,CTREC,CTWRK         
         CLI   DMCB+8,0                                                         
         BE    ENQ5X                                                            
         DC    H'0'                                                             
ENQ5G1   GOTO1 =V(HEXIN),DMCB,C+7,CTKEY,64                                      
         OC    DMCB+12(4),DMCB+12                                               
         BZ    ENQ5X               IGNORE INVALID HEX KEY VALUE                 
         MVC   CTREC+00(32),CTKEY                                               
         MVC   CTREC+32(02),=H'52'                                              
         MVC   CTREC+34(08),=X'8000000000000000'                                
         MVC   CTREC+42(02),=X'AA09'                                            
         MVC   CTREC+44(07),=C'ENQTEST'                                         
         MVI   CTREC+51,X'00'                                                   
         GOTO1 =V(DATAMGR),DMCB,(X'00',ADDREC),GENFIL,CTKEY,CTREC,CTWRK         
         CLI   DMCB+8,0                                                         
         BE    ENQ5X                                                            
         DC    H'0'                                                             
ENQ5X    EQU   *                                                                
*&&UK                                                                           
ENQ6     CLC   C(5),=C'%MEDZ'      %MEDZ FOR MEDZ TESTING                       
         BNE   ENQ6X                                                            
ENQ6F    CLI   C+5,C'F'            %MEDZF FOR MEDFIL                            
         BNE   ENQ6X                                                            
         CLI   C+6,C'A'            TEST A FOR ADD OR U FOR UPDATE               
         BE    ENQ6FA                                                           
         CLI   C+6,C'U'            TEST A FOR ADD OR U FOR UPDATE               
         BE    ENQ6FU                                                           
         B     ENQ6X                                                            
*                                                                               
ENQ6FA   GOTO1 =V(HEXIN),DMCB,C+7,CTKEY,40                                      
         OC    DMCB+12(4),DMCB+12                                               
         BZ    ENQ6X               IGNORE INVALID HEX KEY VALUE                 
         XC    CTREC(255),CTREC                                                 
         MVC   CTREC+00(20),CTKEY                                               
         MVC   CTREC+20(02),=H'45'                                              
         MVC   CTREC+22(01),=X'80'                                              
         MVC   CTREC+34(02),=X'AA0A'                                            
         MVC   CTREC+36(08),=C'ENQTEST '                                        
         MVI   CTREC+44,X'00'                                                   
         GOTO1 =V(DATAMGR),DMCB,(X'00',ADDREC),MEDFIL,CTADR,CTREC,CTWRK         
         CLI   DMCB+8,0                                                         
         BE    ENQ6X                                                            
         DC    H'0'                                                             
*                                                                               
ENQ6FU   MVC   CTADR,=X'00010101'  %MEDZFU UPDATE 00010101                      
         GOTO1 =V(DATAMGR),DMCB,(X'80',GETREC),MEDFIL,CTADR,CTREC,CTWRK         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CTREC+22,X'10'      TURN ON/OFF STATUS BIT                       
         BZ    *+12                                                             
         NI    CTREC+22,255-X'10'                                               
         B     *+8                                                              
         OI    CTREC+22,X'10'                                                   
         GOTO1 =V(DATAMGR),DMCB,(X'00',PUTREC),MEDFIL,CTADR,CTREC,CTWRK         
         CLI   DMCB+8,0                                                         
         BE    ENQ6X                                                            
         DC    H'0'                                                             
ENQ6X    EQU   *                                                                
*&&                                                                             
ENQ7     B     ENQNEXT             INVALID ACTION                               
         EJECT                                                                  
QDQ      NTR1                                                                   
         BAS   R9,GETTIM           GET START TIME                               
         MVC   STIM,TIM                                                         
         BAS   R9,GETIOS           GET I/O COUNT                                
         MVC   SIOS,IOS                                                         
         LA    RE,=C'ENQDEQ'                                                    
         CLC   C+1(4),=C'CTRL'                                                  
         BNE   *+8                                                              
         LA    RE,=C'ENQCTL'                                                    
         ST    RE,ENQP0                                                         
         GOTO1 =V(DATAMGR),ENQP0                                                
         BAS   R9,GETTIM                                                        
         MVC   XTIM,TIM            GET END TIME                                 
         BAS   R9,GETIOS                                                        
         MVC   XIOS,IOS            GET END I/O COUNT                            
         L     R1,TIM                                                           
         S     R1,STIM                                                          
         ST    R1,DTIM             GET DELTA TIME MILLISECS                     
         L     R1,IOS                                                           
         S     R1,SIOS                                                          
         ST    R1,DIOS             GET DELTA IOS                                
         XIT1                                                                   
                                                                                
PRTHDR   NTR1                                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),DMCB,ENQP1,P+00,4,=C'MIX'                             
         GOTO1 =V(HEXOUT),DMCB,ENQP2,P+09,4                                     
         GOTO1 =V(HEXOUT),DMCB,ENQP3,P+18,4                                     
         MVC   P+30(20),C                                                       
         L     R1,STIM                                                          
         CVD   R1,DUB                                                           
         UNPK  P+51(8),DUB                                                      
         OI    P+58,X'F0'                                                       
         L     R1,XTIM                                                          
         CVD   R1,DUB                                                           
         UNPK  P+60(8),DUB                                                      
         OI    P+67,X'F0'                                                       
         L     R1,DTIM                                                          
         CVD   R1,DUB                                                           
         UNPK  P+69(5),DUB                                                      
         OI    P+73,X'F0'                                                       
         L     R1,SIOS                                                          
         CVD   R1,DUB                                                           
         UNPK  P+75(5),DUB                                                      
         OI    P+79,X'F0'                                                       
         GOTO1 VPRNTBL,DMCB,0,P,1,80,=C'2C'                                     
         XIT1                                                                   
                                                                                
PRTTBL   NTR1                                                                   
         L     R2,ENQP2            GET ADDR LIST PASSED FROM DMENQDEQ           
         L     R2,12(R2)           GET A(ENQREC KEY AND DATA)                   
         GOTO1 VPRNTBL,DMCB,0,(R2),6,30,=C'2H'                                  
         XIT1                                                                   
                                                                                
PRTCTL   NTR1                                                                   
         L     R2,ENQP2            GET ADDR LIST PASSED FROM DMENQDEQ           
         L     R2,20(R2)           GET A(CONTROL RECORD)                        
         CLC   C+1(4),=C'MEDZ'                                                  
         BNE   *+12                                                             
         L     R2,ENQP2            GET ADDR LIST PASSED FROM DMENQDEQ           
         L     R2,24(R2)           GET A(MEDZ RECORD)                           
         GOTO1 VPRNTBL,DMCB,0,(R2),12,32,=C'2H'                                 
         XIT1                                                                   
         EJECT                                                                  
GETTIM   TIME  TU                                                               
         SRDL  R0,32                                                            
         LA    R0,1000                                                          
         MR    R0,R0                                                            
         D     R0,=F'38400'                                                     
         ST    R1,TIM              TIME IN MILLI SECS                           
         BR    R9                                                               
         SPACE 2                                                                
SETTIM   L     R1,TIM              TIME IN SECONDS                              
         LTR   R1,R1                                                            
         BZR   R9                                                               
         MH    R1,=H'100'                                                       
         ST    R1,DUB                                                           
         STIMER WAIT,BINTVL=DUB                                                 
         BR    R9                                                               
         SPACE 2                                                                
GETIOS   EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
         L     R1,=A(ASIDFLD)                                                   
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)          GET ASCB ADDRESS INTO R1                    
         SR    R3,R3                                                            
         ICM   R3,3,ASCBXCNT-ASCB(R1)                                           
         ST    R3,IOS                                                           
         BR    R9                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMADD    DC    CL8'DMADD'                                                       
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
GENDIR   DC    CL8'GENDIR'                                                      
GENFIL   DC    CL8'GENFIL'                                                      
*                                                                               
MEDDIR   DC    CL8'MEDDIR'                                                      
MEDFIL   DC    CL8'MEDFIL'                                                      
*                                                                               
AWKFILE  DC    A(0)                                                             
AFACWRK  DC    A(0)                                                             
ACTFILE  DC    A(0)                                                             
ACTRCVR  DC    A(0)                                                             
AGENDIR  DC    A(0)                                                             
AGENFIL  DC    A(0)                                                             
AMEDDIRZ DC    A(0)                                                             
AMEDFILZ DC    A(0)                                                             
AMEDRCVZ DC    A(0)                                                             
*                                                                               
DUB      DC    D'0'                                                             
DUB1     DC    D'0'                                                             
DUB2     DC    D'0'                                                             
FULL     DC    F'0'                                                             
FULL1    DC    F'0'                                                             
VPRNTBL  DC    V(PRNTBL)                                                        
P0       DC    F'0'                                                             
DMCB     DC    6F'0'                                                            
         ORG   DMCB                                                             
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
DUMPLIST DS    0F                                                               
         DC    X'00',AL3(ENQTEST)                                               
         DC    X'80',VL3(STXITER)                                               
*                                                                               
         DC    C'**PARM**'                                                      
ENQP0    DC    F'0'                                                             
ENQP1    DC    F'0'                                                             
ENQP2    DC    F'0'                                                             
ENQP3    DC    F'0'                                                             
ENQP4    DC    F'0'                                                             
*                                                                               
ECB      DC    F'0'                                                             
TIM      DC    F'0'                                                             
STIM     DC    F'0'                                                             
XTIM     DC    F'0'                                                             
DTIM     DC    F'0'                                                             
WTIM     DC    F'0'                                                             
RSTIM    DC    F'0'                                                             
RXTIM    DC    F'0'                                                             
REPEAT   DC    F'0'                                                             
ASIDFLD  DC    F'0'                                                             
IOS      DC    F'0'                                                             
SIOS     DC    F'0'                                                             
XIOS     DC    F'0'                                                             
DIOS     DC    F'0'                                                             
*                                                                               
SPACES   DC    CL120' '                                                         
C        DC    CL80' '                                                          
PCC      DC    CL1' '                                                           
P        DC    CL120' '                                                         
HDR      DC    CL120' '                                                         
LIST     DC    CL80' '                                                          
*                                                                               
TASK     DC    X'00'                                                            
PQOPEN   DC    X'00'                                                            
WKOPEN   DC    X'00'                                                            
FLAG     DC    X'00'                                                            
MEOPEN   DC    C'N'                                                             
CTOPEN   DC    C'N'                                                             
STATS    DC    C'N'                                                             
*                                                                               
         DS    0F                                                               
CTADR    DC    X'00010101'                                                      
CTWRK    DC    XL96'00'                                                         
CTKEY    DC    XL32'00'                                                         
         DC    XL32'00'                                                         
CTREC    DC    2000X'00'                                                        
CTBUFF   DC    20000X'00'                                                       
         EJECT                                                                  
         DC    C'**WORK**'                                                      
ENQWORK  DC    4000D'0'                                                         
                                                                                
         DC    C'**UTL***'                                                      
UTL      DC    F'0',X'01000000',98F'0'                                          
                                                                                
         DC    C'**SSB***'                                                      
SSB      DC    X'0000FF08',249F'0' RECOVER OFFLINE COPIES                       
                                                                                
TSK1     DC    C'**TSK1**',99D'0'                                               
TSK2     DC    C'**TSK2**',99D'0'                                               
         EJECT ,                                                                
* IHAASCB                                                                       
         IHAASCB                                                                
* FASSBOFF                                                                      
       ++INCLUDE FASSBOFF                                                       
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
* DMSYSFLSD                                                                     
       ++INCLUDE DMSYSFD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DMENQTEST 11/30/11'                                      
         END                                                                    
