*          DATA SET DDEDICT    AT LEVEL 012 AS OF 03/04/21                      
*PHASE EDICTA                                                                   
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CALLOFF                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE REPPNTAB                                                               
**NCLUDE FATABOFF                                                               
*INCLUDE DMDMGR      <--FROM THIS LINE BEYOND, COPIED FROM DMDDSIO              
*INCLUDE DMDTFS                                                                 
*INCLUDE DMDADDS                                                                
*INCLUDE DMISDDS                                                                
*INCLUDE DMIS20                                                                 
*INCLUDE DMDDNAME                                                               
*INCLUDE DMDYNDD                                                                
*INCLUDE DMDALINK                                                               
*INCLUDE DMDAPTRS                                                               
*INCLUDE DMLOCKER                                                               
*INCLUDE DMPRTQSH                                                               
*INCLUDE DMRCVR                                                                 
*INCLUDE DMWRKFSH                                                               
*INCLUDE DMWRKR                                                                 
*INCLUDE DMACCEMU                                                               
*INCLUDE DMDABUFF                                                               
*INCLUDE DMDANDX                                                                
*&&US                                                                           
*INCLUDE DMENQCTL                                                               
*&&                                                                             
*INCLUDE DMENQDEQ                                                               
*INCLUDE DMISGENQ                                                               
*INCLUDE DMRCVUSS                                                               
*INCLUDE DMSHMUSS                                                               
*INCLUDE DMSYSFIL                                                               
*INCLUDE LOCKSPC                                                                
*INCLUDE LOCKUP                                                                 
*INCLUDE DYNALLOC                                                               
*INCLUDE GETRET                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE ARREDIT                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*                                                                               
* THIS IS A VERSION OF THE 24 HR EDICT                                          
* NOT INCLUDING THE REMOVING OF THE REPDIR BUILDING.                            
*                                                                               
*CHANGES IN THIS PROGRAM ABOUT RELEASING XMTTABLE BEFORE ENQ PRTQU              
*THEN BINSEARCH THE ENTRY AFTER DEQ PRTQU AND ENQ XMTTABLE AGAIN                
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        EDICT -- ELECTRONIC DATA INTERCHANGE COMBINED XMIT   *         
*                                                                     *         
*  COMMENTS:     FIND ELECTRONIC TRANSMISSIONS                        *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- PRINT QUEUE TABLE                              *         
*                R7 -- WORK                                           *         
*                R8 -- WORK                                           *         
*                R9 -- WORK                                           *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDICT -- LOCATE AND TRANSFER CLASS G REPORTS'                 
         MACRO                                                                  
&NAME    PRNT  &A,&PRINT=                                                       
&NAME    MVC   P(17),=CL17'&A'                                                  
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+18(2),PRNTTIME                                                 
         MVI   P+20,C':'                                                        
         MVC   P+21(2),PRNTTIME+2                                               
         MVI   P+23,C':'                                                        
         MVC   P+24(2),PRNTTIME+4                                               
         MVI   P+26,C'.'                                                        
         MVC   P+27(2),PRNTTIME+6                                               
         AIF   ('&PRINT' EQ 'ALWAYS').SKIP                                      
         CLI   TRACEFLG,C'Y'                                                    
         BNE   *+10                                                             
.SKIP    L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MEND                                                                   
         EJECT                                                                  
*                                                                               
EDICT    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**EDICT*,=A(R13CHAIN)                                          
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         USING CIDATAD,R6                                                       
*                                                                               
         LR    R1,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         USING COMMWORK,RC                                                      
         ST    RD,SAVERD                                                        
*                                                                               
         AHI   R1,-4                                                            
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
         CLC   =H'3',0(R1)         MUST BE LENGTH OF 3                          
         JNE   *+2                                                              
*                                                                               
* THIS MAJORNAM KEY IS A STEP LEVEL ENQUEUE (NOT ACROSS JOBS)                   
*                                                                               
         MVC   MAJORNAM+5(3),2(R1)                                              
         CLC   =C'ADV',2(R1)                                                    
         BE    PARMOK                                                           
         CLC   =C'REP',2(R1)                                                    
         BE    PARMOK                                                           
         CLC   =C'R#2',2(R1)                                                    
         BE    PARMOK                                                           
         CLC   =C'QA',2(R1)        WORKS FOR QAA AND QAR (FQA)                  
         BE    PARMOK                                                           
         CLC   =C'CSC',2(R1)                                                    
         BE    PARMOK                                                           
         CLC   =C'TST',2(R1)                                                    
         JNE   *+2                                                              
*                                                                               
PARMOK   L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'MASTER  '  DDNAME=MASTER                
*                                                                               
         MVC   TITLE(15),=C'EDICT MAIN TASK'                                    
         PRNT  START_TASK,PRINT=ALWAYS                                          
*                                                                               
         BRAS  RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         CLI   EDICTTYP,C'R'                                                    
         BNE   INIT00                                                           
         BRAS  RE,BLDPNTAB         BUILD DARE PERSONAL NOTIFICATION             
*                                  TABLE FOR REPPAK                             
INIT00   DS    0H                                                               
*                                                                               
         BRAS  RE,INITIAL          INITIALIZE                                   
*                                                                               
         LA    R0,4                                                             
         LNR   R0,R0               R0 = -4                                      
         SVC   247                 SO EDICT IS NOT SWAPPABLE                    
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB     A(ECB)                                       
         L     R2,COMCIBPT         GET A(CIB)                                   
         LA    R3,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTART    WAS EDICT BROUGHT UP WITH 'START'?           
         BNE   NOSTART                                                          
         DROP  R2                                                               
*                                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  YES -- FREE THE CIB                      
NOSTART  QEDIT ORIGIN=(R3),CIBCTR=1    NOW ALLOW MODIFIES                       
         EJECT                                                                  
***********************************************************************         
* MAIN LOOP                                                                     
***********************************************************************         
MAINLOOP EQU   *                                                                
         BRAS  RE,SWEEPEDF         SWEEP THE EDICT FILE PERIODICALLY            
*                                                                               
         BRAS  RE,FINDREPS         FIND REPORTS TO TRANSMIT                     
         BRAS  RE,PURGEOLD         PURGE SENT PRINT QUEUE REPORTS               
*                                                                               
         GOTO1 =V(DMISGENQ),DMCB,C'#00K' RELEASE ALL MAIN TASK ENQUEUES         
*                                                                               
         CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    MLOOPX              YES - WRAP UP                                
         BRAS  RE,BLDDEST          REBUILD DESTINATION TABLE                    
*                                                                               
         BRAS  RE,WAITABIT         WAIT BEFORE SEARCHING AGAIN                  
         BRAS  RE,REATTACH         RE-ATTACH DEAD SUBTASKS IF WE SHOULD         
*                                                                               
         BRAS  RE,NEWDAY                                                        
*                                                                               
         BRAS  RE,CHKOPER          CHECK FOR OPERATOR INTERVENTION              
         CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BNE   MAINLOOP            NO -- LOOK FOR MORE REPORTS                  
*                                                                               
MLOOPX   BRAS  RE,SHUTDOWN         WRAP UP                                      
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRAP UP.                                                                      
***********************************************************************         
SHUTDOWN NTR1  BASE=*,LABEL=*                                                   
         CLI   XMITNFX,C'Y'                                                     
         BNE   SHUT05                                                           
         MVI   BYTE,C'Q'                                                        
         BRAS  RE,DETACH           DETACH NFX SUBTASK                           
         MVC   OPMS1(3),=C'NFX'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT05   CLI   XMITENC,C'Y'                                                     
         BNE   SHUT10                                                           
         MVI   BYTE,C'Z'                                                        
         BRAS  RE,DETACH           DETACH ENCODA SUBTASK                        
         MVC   OPMS1(3),=C'ENC'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT10   CLI   XMITBDE,C'Y'                                                     
         BNE   SHUT12                                                           
         MVI   BYTE,C'T'                                                        
         BRAS  RE,DETACH           DETACH BDE-EMAIL SUBTASK                     
         MVC   OPMS1(3),=C'BDE'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT12   CLI   XMITBDF,C'Y'                                                     
         BNE   SHUT15                                                           
         MVI   BYTE,C'P'                                                        
         BRAS  RE,DETACH           DETACH BDE-FTP SUBTASK                       
         MVC   OPMS1(3),=C'BDF'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT15   CLI   XMITMQM,C'Y'                                                     
         BNE   SHUT20                                                           
         MVI   BYTE,C'M'                                                        
         BRAS  RE,DETACH           DETACH MQM SUBTASK                           
         MVC   OPMS1(3),=C'MQM'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT20   CLI   XMITNJE,C'Y'                                                     
         BNE   SHUT25                                                           
         MVI   BYTE,C'J'                                                        
         BRAS  RE,DETACH           DETACH NJE SUBTASK                           
         MVC   OPMS1(3),=C'NJE'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT25   CLI   XMITFTP,C'Y'                                                     
         BNE   SHUT30                                                           
         MVI   BYTE,C'F'                                                        
         BRAS  RE,DETACH           DETACH FTP SUBTASK                           
         MVC   OPMS1(3),=C'FTP'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT30   CLI   XMITEZ,C'Y'                                                      
         BNE   SHUT35                                                           
         MVI   BYTE,C'E'                                                        
         BRAS  RE,DETACH           DETACH EASYLINK SUBTASK                      
         MVC   OPMS1(3),=C'ATT'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT35   CLI   XMITADVN,C'Y'                                                    
         BNE   SHUT40                                                           
         MVI   BYTE,C'A'                                                        
         BRAS  RE,DETACH           DETACH ADVANTIS SUBTASK                      
         MVC   OPMS1(3),=C'ADN'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT40   CLI   XMITDARE,C'Y'                                                    
         BNE   SHUT45                                                           
         MVI   BYTE,C'D'                                                        
         BRAS  RE,DETACH           DETACH DARE SUBTASK                          
         MVC   OPMS1(3),=C'DAR'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT45   CLI   XMITCOL,C'Y'                                                     
         BNE   SHUT50                                                           
         MVI   BYTE,C'C'                                                        
         BRAS  RE,DETACH           DETACH COLUMBINE SUBTASK                     
         MVC   OPMS1(3),=C'COL'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT50   CLI   XMITFAXG,C'Y'                                                    
         BNE   SHUT55                                                           
         MVI   BYTE,C'X'                                                        
         BRAS  RE,DETACH           DETACH FAXGATE SUBTASK                       
         MVC   OPMS1(3),=C'FAX'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT55   CLI   XMITPDF,C'Y'                                                     
         BNE   SHUT60                                                           
         MVI   BYTE,C'O'                                                        
         BRAS  RE,DETACH           DETACH PDF SUBTASK                           
         MVC   OPMS1(3),=C'PDF'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT60   CLI   XMITBIAS,C'Y'                                                    
         BNE   SHUT70                                                           
         MVI   BYTE,C'B'                                                        
         BRAS  RE,DETACH           DETACH BIAS SUBTASK                          
         MVC   OPMS1(3),=C'BIA'                                                 
         MVC   OPMS1+18(8),=C'DISABLED'                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
SHUT70   MVC   P(22),=C'REPORTS FOUND TODAY:  '                                 
         L     RF,=A(NUMREPS)                                                   
         L     RF,0(RF)                                                         
         EDIT  (RF),(7,P+22),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 =V(PRINTER)                                                      
         MVC   P(26),=C'MAXIMUM REPORTS PER DAY:  '                             
         EDIT  MAXREPS,(7,P+26),ALIGN=LEFT                                      
         GOTO1 =V(PRINTER)                                                      
         SR    R0,R0                                                            
         L     R1,=A(NUMREPS)                                                   
         L     R1,0(R1)            NUMBER OF REPORTS FOUND TODAY                
         MHI   R1,100              TO GET PERCENTAGE                            
         D     R0,MAXREPS                                                       
         MVC   P(23),=C'DAILY PARTITION USED:  '                                
         EDIT  (R1),(2,P+23),ALIGN=LEFT,ZERO=NOBLANK                            
         MVI   P+25,C'%'                                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BRAS  RE,PRTUSAGE         PRINT THE CURRENT %USAGE OF XMITTBL          
*                                                                               
         MVC   P(43),=C'ABOUT TO PERFORM ASYNC. MANAGER FOR CLEANUP'            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SAM31                                                                  
         L     RF,VATBAMR1                                                      
         CALL  (15),(FUNCTION,ASYNCHRONOUS_NUMBER,APPC_RETURN_CODE),VL          
         SAM24                                                                  
*                                                                               
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  APPC_RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OC    APPC_RETURN_CODE,APPC_RETURN_CODE                                
         BZ    ALLCLEAN                                                         
         CLC   APPC_RETURN_CODE,=F'4'                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(13),=C'TP CLEANED UP'                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ALLCLEAN PRNT  TERMINATE,PRINT=ALWAYS                                           
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD IN CORE TABLE OF DARE NOTIFICATION ASSIGNMENTS FOR REPPAK               
***********************************************************************         
BLDPNTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* ALLOCATE/INIT TSAR BUFFER                                                     
*                                                                               
         MVC   P(30),=C'*** GET ADDRESS OF TSAROFF ***'                         
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(LOADER),DMCB,=CL8'T00A7D',0,0                                 
         OC    DMCB+4(4),DMCB+4    LOADED OKAY?                                 
         BZ    BLP010              YES                                          
         MVC   ATSAROFF,4(R1)      SAVE A(TSAROFF)                              
*                                                                               
         MVC   P(51),=C'*** BUILDING DARE NOTIFICATION ASSIGNMENT TABLEX        
                ***'                                                            
         GOTO1 =V(PRINTER)                                                      
*                                  REQUEST TSAROFF                              
         XC    TSARBLK,TSARBLK                                                  
         XC    PARM,PARM                                                        
         MVC   PARM(4),DATAMGR                                                  
         MVC   PARM+4(4),=V(CALLOFF)                                            
         MVC   PARM+8(4),ATSAROFF                                               
         LA    RE,TSARBLK                                                       
         ST    RE,PARM+16                                                       
         GOTO1 =V(REPPNTAB),DMCB,(1,PARM),0                                     
         MVC   ATSARBUF,DMCB       SAVE ADDRESS OF TSAR BUFFER TO BE            
*                                  PASSED TO EDICTS THEN TO EDICTR              
         TM    DMCB+4,TSEINIF      INITIALIZATION FAILED?                       
         BZ    BLP020                                                           
*                                                                               
BLP010   MVC   P(41),=C'*** TSAR BUFFER INITIALIZATION FAILED ***'              
         GOTO1 =V(PRINTER)                                                      
         B     BLPXIT                                                           
*                                                                               
         USING SYSTABD,R3                                                       
BLP020   L     R3,=V(SYSTABL)                                                   
         SR    R4,R4                                                            
*                                                                               
BLP030   CHI   R4,X'FF'                                                         
         BE    BLP200              DONE                                         
         CLI   DMSYOVNO,X'08'      REP SYSTEM                                   
         BNE   BLP100              SKIP THIS ONE                                
         LARL  R5,NOOPREPS         REPS TO EXCLUDE                              
BLP040   CLI   0(R5),C' '          NONE LEFT                                    
         BE    BLP050              SO THIS ONE IS GOOD                          
         CLC   DMSYNAM1+1(2),0(R5) MATCH ON REP SYSTEM                          
         BE    BLP100              SKIP THIS ONE                                
         AHI   R5,2                NEXT ENTRY                                   
         B     BLP040              CHECK FOR ANOTHER                            
*                                                                               
         USING UTLD,RF                                                          
BLP050   L     RF,=A(UTL)          SE# IS IN R4                                 
         STC   R4,TSYS                                                          
         STC   R4,BYTE                                                          
         DROP  RF                                                               
*                                                                               
         MVC   P(29),=C'*** PROCESSING REPFILE    ***'                          
         GOTO1 =V(HEXOUT),DMCB,BYTE,P+23,1,=C'TOG'                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'REP',                    +        
               =C'NREPFIL NREPDIR X',A(IO),0                                    
*                                                                               
* READ RECORDS IN TO TSAR BUFFER                                                
*                                                                               
         XC    PARM,PARM                                                        
         MVC   PARM(4),DATAMGR                                                  
         MVC   PARM+4(4),=V(CALLOFF)                                            
         MVC   PARM+8(4),ATSAROFF                                               
         MVC   PARM+12(4),ATSARBUF                                              
         LA    RE,TSARBLK                                                       
         ST    RE,PARM+16                                                       
         GOTO1 =V(REPPNTAB),DMCB,(2,PARM),0                                     
*                                                                               
         MVC   P(26),=C'*** RECORDS ADDED TO TSAR:'                             
         EDIT  (4,DMCB),(7,P+27),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         TM    DMCB+4,TSEEOF       EOF/BOF??                                    
         BZ    BLP100                                                           
         MVC   P(38),=C'*** END OF TSAR BUFFER ENCOUNTERED ***'                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
BLP100   AHI   R4,1                ADD ONE TO SE COUNT                          
         AHI   R3,DMSYLEN          ADD TABLE ENTRY LENGTH                       
         B     BLP030              LOOP TO NEXT SYSTEM                          
*                                                                               
BLP200   MVC   P(31),=C'*** TABLE SUCCESFULLY BUILT ***'                        
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         USING UTLD,RF                                                          
BLPXIT   L     RF,=A(UTL)                                                       
         MVI   TSYS,X'0A'          RESET BACK TO CONTROL SYS                    
         XIT1                                                                   
         DROP  R3,RF                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INITIAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* DUMMY LOCKSPC CALL TO OBTAIN TABS DATASPACE ALET                              
         GOTO1 =V(LOCKSPC),DMCB,(X'20',A(DTGETRET)),0                           
*                                                                               
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         USING SSBD,RF                                                          
         L     RF,=A(SSB)                                                       
         OI    SSOMTIND,SSOMTAPL   TURN ON MULTI PROC                           
         DROP  RF                                                               
*                                                                               
         GOTO1 =V(DMISGENQ),DMCB,C'#00K'      SET MAIN TASK #00                 
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(3,TODAY3)                                 
         GOTO1 =V(DATCON),DMCB,(5,0),TODAY6                                     
         BRAS  RE,DAYTIMER         RESET THE TIMER FOR NEXT DAY                 
*                                                                               
*        ESTAE EXTRTN,CT,ASYNCH=YES,TERM=YES                                    
*                                                                               
         BRAS  RE,BLDPQTAB         BUILD PRINT QUEUE TABLE                      
*                                                                               
         BRAS  RE,OPENEDFL         OPEN EDICT TRANSACTN FILE, FIND EOF          
*                                                                               
         BRAS  RE,CHKXEDFL         ENSURE REST OF EDICT FILE IS CLEARED         
*                                                                               
         BRAS  RE,PRTUSAGE         PRINT THE CURRENT %USAGE OF XMITTBL          
*                                                                               
         BRAS  RE,ALODEST          ALLOCATE DESTTBL                             
         BRAS  RE,BLDDEST          BUILD TABLE OF DESTINATION NAMES             
*                                                                               
         BRAS  RE,BLDLUTAB         BUILD TABLE OF COLUMBINE LUNAMES             
*                                                                               
*                                  GET SPACE FOR BROADCAST DST TABLE            
         LH    R2,MAXDESTS         MAX NUMBER OF DESTS/REPORT                   
         BCTR  R2,0                -1, 1ST DEST WON'T BE ON THIS TABLE          
         MHI   R2,BCDSTABQ         X LENGTH OF EACH DEST ENTRY                  
         LA    R2,8(R2)            FOR EYE-CATCHER(8)                           
         STORAGE OBTAIN,LENGTH=(2)  ... BROADCAST DESTS TABLE                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         AR    R2,R1                                                            
         ST    R2,EBCDSTAB         A(END OF BROADCAST DESTS TABLE)              
         MVC   0(8,R1),=C'*BCDSTAB'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,ABCDSTAB         A(BROADCAST DESTS TABLE)                     
*                                                                               
         LA    R2,MAX_FAXGATE_PARTNERS    NUMBER OF FAXGATE PCS                 
         SLL   R2,1                TIMES 2 (2 CONVERSATIONS PER PC)             
         MHI   R2,APPCDLEN         R2 = SPACE NEEDED FOR APPC/MVS TABLE         
         LA    R2,12(R2)           ADD ROOM FOR EYE-CATCHER, EOT MARKER         
         STORAGE OBTAIN,LENGTH=(2)  ... APPC/MVS CONTROL TABLE                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*APPCTAB'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AFXTABLE         A(FAXGATE APPC/MVS CONTROL TABLE)            
*                                                                               
         BLDL  0,ENTRYPTS          BUILD TABLE OF APPC/MVS ENTRY POINTS         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LOAD  DE=ATBAMR1                                                       
         ST    R0,VATBAMR1                                                      
*                                                                               
         MVI   BYTE,C'E'                                                        
         CLI   XMITEZ,C'Y'                                                      
         BNE   INIT05                                                           
         BRAS  RE,ATTACH           ATTACH EASYLINK SUBTASK                      
*                                                                               
INIT05   MVI   BYTE,C'Q'                                                        
         CLI   XMITNFX,C'Y'                                                     
         BNE   INIT10                                                           
         BRAS  RE,ATTACH           ATTACH NFX SUBTASK                           
*                                                                               
INIT10   MVI   BYTE,C'Z'                                                        
         CLI   XMITENC,C'Y'                                                     
         BNE   INIT12                                                           
         BRAS  RE,ATTACH           ATTACH ENCODA SUBTASK                        
*                                                                               
INIT12   MVI   BYTE,C'T'                                                        
         CLI   XMITBDE,C'Y'                                                     
         BNE   INIT15                                                           
         BRAS  RE,ATTACH           ATTACH BDE-EMAIL SUBTASK                     
*                                                                               
INIT15   MVI   BYTE,C'P'                                                        
         CLI   XMITBDF,C'Y'                                                     
         BNE   INIT20                                                           
         BRAS  RE,ATTACH           ATTACH BDE-FTP SUBTASK                       
*                                                                               
INIT20   MVI   BYTE,C'M'                                                        
         CLI   XMITMQM,C'Y'                                                     
         BNE   INIT25                                                           
         BRAS  RE,ATTACH           ATTACH E-MAIL SUBTASK                        
*                                                                               
INIT25   MVI   BYTE,C'J'                                                        
         CLI   XMITNJE,C'Y'                                                     
         BNE   INIT30                                                           
         BRAS  RE,ATTACH           ATTACH NJE SUBTASK                           
*                                                                               
INIT30   MVI   BYTE,C'F'                                                        
         CLI   XMITFTP,C'Y'                                                     
         BNE   INIT40                                                           
         BRAS  RE,ATTACH           ATTACH FTP SUBTASK                           
*                                                                               
INIT40   MVI   BYTE,C'D'                                                        
         CLI   XMITDARE,C'Y'                                                    
         BNE   INIT50                                                           
         BRAS  RE,ATTACH           ATTACH DARE SUBTASK                          
*                                                                               
INIT50   MVI   BYTE,C'C'                                                        
         CLI   XMITCOL,C'Y'                                                     
         BNE   INIT60                                                           
         BRAS  RE,ATTACH           ATTACH COLUMBINE SUBTASK                     
*                                                                               
INIT60   MVI   BYTE,C'A'                                                        
         CLI   XMITADVN,C'Y'                                                    
         BNE   INIT70                                                           
         BRAS  RE,ATTACH           ATTACH ADVANTIS SUBTASK                      
*                                                                               
INIT70   MVI   BYTE,C'X'                                                        
         CLI   XMITFAXG,C'Y'                                                    
         BNE   INIT80                                                           
         BRAS  RE,ATTACH           ATTACH FAXGATE SUBTASK                       
*                                                                               
INIT80   MVI   BYTE,C'B'                                                        
         CLI   XMITBIAS,C'Y'                                                    
         BNE   INIT85                                                           
         BRAS  RE,ATTACH           ATTACH BIAS SUBTASK                          
*                                                                               
INIT85   MVI   BYTE,C'O'                                                        
         CLI   XMITPDF,C'Y'                                                     
         BNE   INIT90                                                           
         BRAS  RE,ATTACH           ATTACH PDF SUBTASK                           
*                                                                               
INIT90   EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* LOCATE ALL REPORTS TO BE TRANSMITTED ON ALL PRINT QUEUES                      
***********************************************************************         
FINDREPS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PQSCAN,C'Y'         SHOULD WE SCAN PRINT QUEUES?                 
         BNE   FRX                 NO                                           
*                                                                               
         TIME  BIN                                                              
         ST    R0,LASTTIME         SAVE PQ SCAN @ THIS TIME                     
         PRNT  FINDREPORTS,PRINT=ALWAYS                                         
*                                                                               
         L     R6,=A(CITABLE)      PRINT QUEUE INFO                             
*                                                                               
FR20     L     RE,=A(RPTNUMS)                                                   
         LA    RF,NUMRPTSQ         NUMBER OF TABLE ENTRIES. . .                 
         SLL   RF,1                . . . TIMES 2 BYTES/ENTRY                    
         XCEFL                                                                  
         L     R5,=A(RPTNUMS)                                                   
*                                                                               
FR30     XC    PQINDEX,PQINDEX                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'SEQ'),CFPQID,PQINDEX,R,CXREC                  
         TM    DMCB+8,X'80'        END OF FILE?                                 
         BO    FR40                YES                                          
*                                                                               
         LA    R3,R                                                             
         USING PQPLD,R3                                                         
         CLI   QLCLASS,C'G'        CLASS G?                                     
         BNE   FR30                                                             
         TM    QLSTAT,X'80'        STATUS ACTIVE?                               
         BZ    FR30                                                             
         TM    QLATTB,QLATJOBI     DOES REPORT CONTAIN JCL?                     
         BO    FR30                YES -- IGNORE IT FOR NOW                     
         TM    QLATTB,QLATERR      IS REPORT IN ERROR?                          
         BO    FR30                YES -- IGNORE IT                             
*                                                                               
         MVI   SKIPTHIS,C'N'                                                    
*                                                                               
         L     RF,=A(USERFILT)                                                  
         OC    0(L'USERFILT,RF),0(RF)    IS THERE A USERID FILTER?              
         BZ    FR33X                     NO                                     
         LA    RE,USRFMAXQ                                                      
FR33K    OC    0(L'USERFILT,RF),0(RF)    ANY MORE USERID FILTER?                
         BNZ   FR33M                                                            
         MVI   SKIPTHIS,C'Y'       NO - NOT A MATCH, FLAG TO SKIP               
         B     FR33X                                                            
*                                                                               
FR33M    CLC   QLSRCID,0(RF)       YES - DOES THIS REPORT MATCH FILTER?         
         BE    FR33X               YES - GOT A MATCH                            
         AHI   RF,L'USERFILT                                                    
         BCT   RE,FR33K            TRY NEXT FILTER                              
*                                                                               
FR33X    CLC   SUBFILT,MYSPACES    IS THERE A SUB-ID FILTER?                    
         BE    FR34                NO - CHECK IF EXSUBID FILTER                 
         CLC   QLSUBID,SUBFILT     YES - DOES THIS REPORT MATCH FILTER?         
         BE    FR35                                                             
         MVI   SKIPTHIS,C'Y'       NO - NOT A MATCH, FLAG TO SKIP               
         B     FR35                                                             
*                                                                               
FR34     EQU   *                   CAN ONLY HAVE SUBID/EXSUBID FILTER           
         CLC   EXSUBFT,MYSPACES    IS THERE EX SUB-ID FILTER?                   
         BE    FR35                NO                                           
         CLC   QLSUBID,EXSUBFT     YES - DOES THIS REPORT MATCH FILTER?         
         BNE   FR35                NO - CONTINUE                                
         MVI   SKIPTHIS,C'Y'       YES - MATCH, FLAG TO SKIP                    
*                                                                               
FR35     L     RF,=A(EXUFILT)                                                   
         OC    0(L'EXUFILT,RF),0(RF)    IS THERE A NEG USERID FILTER?           
         BZ    FR35X                    NO                                      
*                                                                               
         LA    RE,EXUFMAXQ                                                      
FR35K    OC    0(L'EXUFILT,RF),0(RF)    ANY MORE NEG USERID FILTER?             
         BZ    FR35X               NO - NOT A MATCH - OKAY                      
         CLC   QLSRCID,0(RF)       YES - DOES THIS REPORT MATCH FILTER?         
         BNE   FR35N               NO: BUMP TO NEXT USERID IN LIST              
         CLI   CBSFIX,C'Y'         DO SPECIAL CBS CODE?                         
         BE    *+12                                                             
         MVI   SKIPTHIS,C'Y'       NO - NOT A MATCH, FLAG TO SKIP               
         B     FR35X                                                            
         CLC   QLSUBID,=C'ECR'     USERID EXCLUSION ONLY APPLIES TO ECR         
         BNE   FR35X                                                            
         MVI   SKIPTHIS,C'Y'       NO - NOT A MATCH, FLAG TO SKIP               
         B     FR35X                                                            
*                                                                               
FR35N    AHI   RF,L'EXUFILT                                                     
         BCT   RE,FR35K            TRY NEXT FILTER                              
*                                                                               
FR35X    MVC   0(2,R5),QLREPNO     REMEMBER THIS REPORT NUMBER                  
         LA    R5,2(R5)                                                         
         C     R5,=A(RPTNUMSX)                                                  
         BL    FR30                                                             
         DC    H'0'                INCREASE NUMRPTSQ                            
         DROP  R3                                                               
*                                                                               
FR40     L     R5,=A(RPTNUMS)      EXAMINE THE REPORTS                          
FR50     OC    0(2,R5),0(R5)       END OF LIST?                                 
         BZ    FR60                YES                                          
*                                                                               
         LA    RF,WORK             USE REP NUM TO INITIALIZE BUFFER             
         XC    WORK,WORK                                                        
         USING UKRECD,RF                                                        
         MVC   UKSRCID,=H'1'                                                    
         MVC   UKREPNO,0(R5)                                                    
         MVI   UKFLAG,UKFLNUM+UKFLCIA                                           
         DROP  RF                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'INDEX'),CFPQID,WORK,R,CXREC                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,XFERREP          BREAK UP THE REPORT                          
*                                                                               
         LA    R5,2(R5)            CHECK OUT NEXT REPORT                        
         B     FR50                                                             
*                                                                               
FR60     EQU   *                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'BUFFER'),CFPQID,0,0,CXREC                     
*                                                                               
         LA    R6,CITBLLNQ(R6)     BUMP TO NEXT PQ                              
         CLI   0(R6),X'FF'         END OF LIST?                                 
         BNE   FR20                                                             
*                                                                               
FRX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ATTACH A SUBTASK RESPONSIBLE FOR A GIVEN METHOD OF TRANSMISSION.              
* FIELD BYTE TELLS WHICH METHOD TO START UP.                                    
***********************************************************************         
ATTACH   NTR1  BASE=*,LABEL=*                                                   
         USING ATTACH+4096,R7                                                   
         LR    R7,RB                                                            
         AHI   R7,4096                                                          
         USING SUBPARMD,R2                                                      
*                                                                               
         CLI   BYTE,C'E'           ATTACH EASYLINK SUBTASK?                     
         BNE   ATT05                                                            
         XC    EASYPARM,EASYPARM                                                
         LA    R2,EASYPARM         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(ESTPECB)                                             
         MVC   SLOOKECB,=A(ELOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         PRNT  ATTACHINGEASYLINK,PRINT=ALWAYS                                   
         XC    EASYECB,EASYECB                                                  
         XC    ESTPECB,ESTPECB                                                  
         ATTACH EPLOC=EZSTASK,ECB=EASYECB,PARAM=EASYPARM,SZERO=NO,     +        
               ALCOPY=YES                                                       
         ST    R1,EASYTCB                                                       
         OC    EASYTCB,EASYTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITEZ,C'Y'         IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT05    CLI   BYTE,C'Q'           ATTACH NFX SUBTASK?                          
         BNE   ATT10                                                            
         XC    NFXPARMS,NFXPARMS                                                
         LA    R2,NFXPARMS         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(QSTPECB)                                             
         MVC   SLOOKECB,=A(QLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         PRNT  ATTACHINGNFX,PRINT=ALWAYS                                        
         XC    NFXECB,NFXECB                                                    
         XC    QSTPECB,QSTPECB                                                  
         ATTACH EPLOC=NFXTASK,ECB=NFXECB,PARAM=NFXPARMS,SZERO=NO,      +        
               ALCOPY=YES                                                       
         ST    R1,NFXTCB                                                        
         OC    NFXTCB,NFXTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITNFX,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT10    CLI   BYTE,C'Z'           ATTACH ENCODA SUBTASK?                       
         BNE   ATT12                                                            
         XC    ENCPARMS,ENCPARMS                                                
         LA    R2,ENCPARMS         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(ZSTPECB)                                             
         MVC   SLOOKECB,=A(ZLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         PRNT  ATTACHINGENC,PRINT=ALWAYS                                        
         XC    ENCECB,ENCECB                                                    
         XC    ZSTPECB,ZSTPECB                                                  
         ATTACH EPLOC=ENCTASK,ECB=ENCECB,PARAM=ENCPARMS,SZERO=NO,      +        
               ALCOPY=YES                                                       
         ST    R1,ENCTCB                                                        
         OC    ENCTCB,ENCTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITENC,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT12    CLI   BYTE,C'T'           ATTACH BDE-EMAIL SUBTASK?                    
         BNE   ATT15                                                            
         XC    BDEPARMS,BDEPARMS                                                
         LA    R2,BDEPARMS         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(TSTPECB)                                             
         MVC   SLOOKECB,=A(TLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         PRNT  ATTACHINGBDE,PRINT=ALWAYS                                        
         XC    BDEECB,BDEECB                                                    
         XC    TSTPECB,TSTPECB                                                  
         ATTACH EPLOC=BDETASK,ECB=BDEECB,PARAM=BDEPARMS,SZERO=NO,      +        
               ALCOPY=YES                                                       
         ST    R1,BDETCB                                                        
         OC    BDETCB,BDETCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITBDE,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT15    CLI   BYTE,C'P'           ATTACH BDE-FTP SUBTASK?                      
         BNE   ATT20                                                            
         XC    BDFPARMS,BDFPARMS                                                
         LA    R2,BDFPARMS         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(PSTPECB)                                             
         MVC   SLOOKECB,=A(PLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         PRNT  ATTACHINGBDF,PRINT=ALWAYS                                        
         XC    BDFECB,BDFECB                                                    
         XC    PSTPECB,PSTPECB                                                  
         ATTACH EPLOC=BDFTASK,ECB=BDFECB,PARAM=BDFPARMS,SZERO=NO,      +        
               ALCOPY=YES                                                       
         ST    R1,BDFTCB                                                        
         OC    BDFTCB,BDFTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITBDF,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT20    CLI   BYTE,C'M'           ATTACH E-MAIL SUBTASK?                       
         BNE   ATT25                                                            
         XC    MQMPARMS,MQMPARMS                                                
         LA    R2,MQMPARMS         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(MSTPECB)                                             
         MVC   SLOOKECB,=A(MLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         PRNT  ATTACHINGMQM,PRINT=ALWAYS                                        
         XC    MQMECB,MQMECB                                                    
         XC    MSTPECB,MSTPECB                                                  
         ATTACH EPLOC=MQMTASK,ECB=MQMECB,PARAM=MQMPARMS,SZERO=NO,      +        
               ALCOPY=YES                                                       
         ST    R1,MQMTCB                                                        
         OC    MQMTCB,MQMTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITMQM,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT25    CLI   BYTE,C'J'           ATTACH NJE SUBTASK?                          
         BNE   ATT30                                                            
         XC    NJEPARMS,NJEPARMS                                                
         LA    R2,NJEPARMS         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(JSTPECB)                                             
         MVC   SLOOKECB,=A(JLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         PRNT  ATTACHINGNJE,PRINT=ALWAYS                                        
         XC    NJEECB,NJEECB                                                    
         XC    JSTPECB,JSTPECB                                                  
         ATTACH EPLOC=NJETASK,ECB=NJEECB,PARAM=NJEPARMS,SZERO=NO,      +        
               ALCOPY=YES                                                       
         ST    R1,NJETCB                                                        
         OC    NJETCB,NJETCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITNJE,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT30    CLI   BYTE,C'F'           ATTACH FTP SUBTASK?                          
         BNE   ATT40                                                            
         XC    FTPPARMS,FTPPARMS                                                
         LA    R2,FTPPARMS         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(FSTPECB)                                             
         MVC   SLOOKECB,=A(FLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         XC    FTPECB,FTPECB                                                    
         XC    FSTPECB,FSTPECB                                                  
         PRNT  ATTACHINGFTP,PRINT=ALWAYS                                        
         ATTACH EPLOC=FTPTASK,ECB=FTPECB,PARAM=FTPPARMS,SZERO=NO,      +        
               ALCOPY=YES                                                       
         ST    R1,FTPTCB                                                        
         OC    FTPTCB,FTPTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITFTP,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT40    CLI   BYTE,C'D'           ATTACH DARE SUBTASK?                         
         BNE   ATT45                                                            
         XC    DAREPARM,DAREPARM                                                
         LA    R2,DAREPARM         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(DSTPECB)                                             
         MVC   SLOOKECB,=A(DLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         XC    DAREECB,DAREECB                                                  
         XC    DSTPECB,DSTPECB                                                  
         MVC   SDNATAB,ATSARBUF                                                 
         MVC   STSAROFF,ATSAROFF                                                
         PRNT  ATTACHINGDARE,PRINT=ALWAYS                                       
         ATTACH EPLOC=DARETASK,ECB=DAREECB,PARAM=DAREPARM,SZERO=NO,    +        
               ALCOPY=YES                                                       
         ST    R1,DARETCB                                                       
         OC    DARETCB,DARETCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITDARE,C'Y'       IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT45    CLI   BYTE,C'A'           ATTACH ADVANTIS SUBTASK?                     
         BNE   ATT50                                                            
         XC    ADVNPARM,ADVNPARM                                                
         LA    R2,ADVNPARM         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(ASTPECB)                                             
         MVC   SLOOKECB,=A(ALOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         XC    ADVNECB,ADVNECB                                                  
         XC    ASTPECB,ASTPECB                                                  
         PRNT  ATTACHINGADVANTIS,PRINT=ALWAYS                                   
         ATTACH EPLOC=ADVNTASK,ECB=ADVNECB,PARAM=ADVNPARM,SZERO=NO,    +        
               ALCOPY=YES                                                       
         ST    R1,ADVNTCB                                                       
         OC    ADVNTCB,ADVNTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITADVN,C'Y'       IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT50    CLI   BYTE,C'C'           ATTACH COLUMBINE SUBTASK?                    
         BNE   ATT60                                                            
         XC    COLPARMS,COLPARMS                                                
         LA    R2,COLPARMS         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(CSTPECB)                                             
         MVC   SLOOKECB,=A(CLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         MVC   SLUTABLE,=A(LUTABLE)                                             
         XC    CFTPECB,CFTPECB                                                  
         XC    CSTPECB,CSTPECB                                                  
         PRNT  ATTACHINGCOLUMB,PRINT=ALWAYS                                     
         ATTACH EPLOC=COLTASK,ECB=CFTPECB,PARAM=COLPARMS,SZERO=NO,     +        
               ALCOPY=YES                                                       
         ST    R1,CFTPTCB                                                       
         OC    CFTPTCB,CFTPTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITCOL,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT60    CLI   BYTE,C'X'           ATTACH FAXGATE SUBTASK?                      
         BNE   ATT70                                                            
         XC    FAXGPARM,FAXGPARM                                                
         LA    R2,FAXGPARM         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(XSTPECB)                                             
         MVC   SLOOKECB,=A(XLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SAPPCTAB,AFXTABLE                                                
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         XC    FAXGECB,FAXGECB                                                  
         XC    XSTPECB,XSTPECB                                                  
         PRNT  ATTACHINGFAXGATE,PRINT=ALWAYS                                    
         ATTACH EPLOC=FAXGTASK,ECB=FAXGECB,PARAM=FAXGPARM,SZERO=NO,    +        
               ALCOPY=YES                                                       
         ST    R1,FAXGTCB                                                       
         OC    FAXGTCB,FAXGTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITFAXG,C'Y'       IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT70    CLI   BYTE,C'O'           ATTACH PDF SUBTASK?                          
         BNE   ATT80                                                            
         XC    PDFSPARM,PDFSPARM                                                
         LA    R2,PDFSPARM         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(PDFPECB)                                             
         MVC   SLOOKECB,=A(OLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         MVC   SUBASSB,=A(SSB)                                                  
         XC    PDFSECB,PDFSECB                                                  
         XC    PDFPECB,PDFPECB                                                  
         PRNT  ATTACHINGPDF,PRINT=ALWAYS                                        
         ATTACH EPLOC=PDFTASK,ECB=PDFSECB,PARAM=PDFSPARM,SZERO=NO,     +        
               ALCOPY=YES                                                       
         ST    R1,PDFSTCB                                                       
         OC    PDFSTCB,PDFSTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITPDF,C'Y'        IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATT80    CLI   BYTE,C'B'           ATTACH BIAS SUBTASK?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    BIASPARM,BIASPARM                                                
         LA    R2,BIASPARM         BUILD SUBTASK PARAMETER LIST                 
         MVC   SSTOPECB,=A(BSTPECB)                                             
         MVC   SLOOKECB,=A(BLOOKECB)                                            
         MVC   SXMTTBLE,AXMTTBL                                                 
         MVC   SDSTTBLE,ADESTTBL                                                
         MVC   SCITABLE,=A(CITABLE)                                             
         MVC   SMAJORNM,=A(MAJORNAM)                                            
         MVC   SEDCTADD,=A(EDICTADD)                                            
         MVC   SADTAMGR,DATAMGR                                                 
         MVC   STRACEON,TRACEFLG                                                
         MVC   SLOGID,LOGID                                                     
         MVC   SDRTEST,DRTEST                                                   
         MVC   SEDCTTYP,EDICTTYP                                                
         MVC   SMAXDSTS,MAXDESTS                                                
         XC    BIASECB,BIASECB                                                  
         XC    BSTPECB,BSTPECB                                                  
         PRNT  ATTACHINGBIAS,PRINT=ALWAYS                                       
         ATTACH EPLOC=BIASTASK,ECB=BIASECB,PARAM=BIASPARM,SZERO=NO,    +        
               ALCOPY=YES                                                       
         ST    R1,BIASTCB                                                       
         OC    BIASTCB,BIASTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         MVI   XMITBIAS,C'Y'       IT SHOULD BE RUNNING FROM NOW ON             
         B     ATTOK                                                            
*                                                                               
ATTOK    PRNT  ATTACHCOMPLETE,PRINT=ALWAYS                                      
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DETACH A SUBTASK RESPONSIBLE FOR A GIVEN METHOD OF TRANSMISSION.              
* FIELD BYTE TELLS WHICH METHOD TO SHUT DOWN.                                   
**********************************************************************          
DETACH   NTR1  BASE=*,LABEL=*                                                   
         CLI   BYTE,C'E'           DETACH EASYLINK SUBTASK?                     
         BNE   DET05                                                            
         CLI   XMITEZ,C'Y'         COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  ESTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=EASYECB         WAIT FOR ANSWER FROM SUBTASK                 
         TM    EASYECB,X'40'       SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGEASYLINK,PRINT=ALWAYS                                   
         DETACH EASYTCB            DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITEZ,C'N'         RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET05    CLI   BYTE,C'Q'           DETACH NFX SUBTASK?                          
         BNE   DET10                                                            
         CLI   XMITNFX,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  QSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=NFXECB          WAIT FOR ANSWER FROM SUBTASK                 
         TM    NFXECB,X'40'        SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGNFX,PRINT=ALWAYS                                        
         DETACH NFXTCB             DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITNFX,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET10    CLI   BYTE,C'Z'           DETACH ENCODA SUBTASK?                       
         BNE   DET12                                                            
         CLI   XMITENC,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  ZSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=ENCECB          WAIT FOR ANSWER FROM SUBTASK                 
         TM    ENCECB,X'40'        SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGENC,PRINT=ALWAYS                                        
         DETACH ENCTCB             DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITENC,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET12    CLI   BYTE,C'T'           DETACH BDE-EMAIL SUBTASK?                    
         BNE   DET15                                                            
         CLI   XMITBDE,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  TSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=BDEECB          WAIT FOR ANSWER FROM SUBTASK                 
         TM    BDEECB,X'40'        SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGBDE,PRINT=ALWAYS                                        
         DETACH BDETCB             DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITBDE,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET15    CLI   BYTE,C'P'           DETACH BDE-FTP SUBTASK?                      
         BNE   DET20                                                            
         CLI   XMITBDF,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  PSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=BDFECB          WAIT FOR ANSWER FROM SUBTASK                 
         TM    BDFECB,X'40'        SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGBDF,PRINT=ALWAYS                                        
         DETACH BDFTCB             DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITBDF,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET20    CLI   BYTE,C'M'           DETACH E-MAIL SUBTASK?                       
         BNE   DET25                                                            
         CLI   XMITMQM,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  MSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=MQMECB          WAIT FOR ANSWER FROM SUBTASK                 
         TM    MQMECB,X'40'        SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGMQM,PRINT=ALWAYS                                        
         DETACH MQMTCB             DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITMQM,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET25    CLI   BYTE,C'J'           DETACH NJE SUBTASK?                          
         BNE   DET30                                                            
         CLI   XMITNJE,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  JSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=NJEECB          WAIT FOR ANSWER FROM SUBTASK                 
         TM    NJEECB,X'40'        SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGNJE,PRINT=ALWAYS                                        
         DETACH NJETCB             DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITNJE,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET30    CLI   BYTE,C'F'           DETACH FTP SUBTASK?                          
         BNE   DET40                                                            
         CLI   XMITFTP,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  FSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=FTPECB          WAIT FOR ANSWER FROM SUBTASK                 
         TM    FTPECB,X'40'        SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGFTP,PRINT=ALWAYS                                        
         DETACH FTPTCB             DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITFTP,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET40    CLI   BYTE,C'D'           DETACH DARE SUBTASK?                         
         BNE   DET45                                                            
         CLI   XMITDARE,C'Y'       COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  DSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=DAREECB         WAIT FOR ANSWER FROM SUBTASK                 
         TM    DAREECB,X'40'       SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGDARE,PRINT=ALWAYS                                       
         DETACH DARETCB            DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITDARE,C'N'       RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET45    CLI   BYTE,C'A'           DETACH ADVANTIS SUBTASK?                     
         BNE   DET50                                                            
         CLI   XMITADVN,C'Y'       COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  ASTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=ADVNECB         WAIT FOR ANSWER FROM SUBTASK                 
         TM    ADVNECB,X'40'       SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGADVANTIS,PRINT=ALWAYS                                   
         DETACH ADVNTCB            DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITADVN,C'N'       RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET50    CLI   BYTE,C'C'           DETACH COLUMBINE SUBTASK?                    
         BNE   DET60                                                            
         CLI   XMITCOL,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  CSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=CFTPECB         WAIT FOR ANSWER FROM SUBTASK                 
         TM    CFTPECB,X'40'       SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGCOLUMB,PRINT=ALWAYS                                     
         DETACH CFTPTCB            DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITCOL,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET60    CLI   BYTE,C'X'           DETACH FAXGATE SUBTASK?                      
         BNE   DET70                                                            
         CLI   XMITFAXG,C'Y'       COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  XSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=FAXGECB         WAIT FOR ANSWER FROM SUBTASK                 
         TM    FAXGECB,X'40'       SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGFAXGATE,PRINT=ALWAYS                                    
         DETACH FAXGTCB            DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITFAXG,C'N'       RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET70    CLI   BYTE,C'O'           DETACH PDF SUBTASK?                          
         BNE   DET80                                                            
         CLI   XMITPDF,C'Y'        COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  PDFPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=PDFSECB         WAIT FOR ANSWER FROM SUBTASK                 
         TM    PDFSECB,X'40'       SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGPDFS,PRINT=ALWAYS                                       
         DETACH PDFSTCB            DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITPDF,C'N'        RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DET80    CLI   BYTE,C'B'           DETACH BIAS SUBTASK?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   XMITBIAS,C'Y'       COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETOK               NO                                           
         POST  BSTPECB             TELL SUBTASK TO STOP                         
         PRNT  WAITINGFORSTOP,PRINT=ALWAYS                                      
         WAIT  ECB=BIASECB         WAIT FOR ANSWER FROM SUBTASK                 
         TM    BIASECB,X'40'       SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  DETACHINGBIAS,PRINT=ALWAYS                                       
         DETACH BIASTCB            DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   XMITBIAS,C'N'       RESET ATTACH FLAG                            
         B     DETOK                                                            
*                                                                               
DETOK    PRNT  DETACHCOMPLETE,PRINT=ALWAYS                                      
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* MARK PQ REPORTS PRINTED WHICH HAVE BEEN TRANSMITTED IN ENTIRETY.              
**********************************************************************          
PURGEOLD NTR1  BASE=*,LABEL=*                                                   
         MVI   POLDEXIT,C'N'                                                    
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         L     R7,AXMTTBL          A(REPORT TABLE)                              
         MVC31 XMTENTRY,0(R7)                                                   
         USING XMTTABLD,R3                                                      
         LA    R3,XMTENTRY                                                      
*                                                                               
         CLI   XMTTABLD,0          END OF TABLE?                                
         BE    POX                                                              
         CLI   XMTSOURC,XMTSRCPQ   LOOK FOR FIRST PRINT QUEUE ENTRY             
         BNE   *-12                                                             
*                                                                               
PO5      CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    POX                 YES - EXIT                                   
         CLI   POLDEXIT,C'Y'                                                    
         BE    POX                 YES - EXIT                                   
*                                                                               
         MVI   BYTE2,0                                                          
         MVC31 XMTENTRY,0(R7)                                                   
         MVC   XMTENTR2,XMTENTRY   SAVE 1ST LOGICAL REPORT                      
*                                                                               
PO10     CLI   0(R3),XMTSRCPQ      STILL LOOKING AT PQ ENTRIES?                 
         BNE   POX                                                              
         TM    XMTSTAT,EDFSTWTG+EDFSTPRT  IF NOT WAITING AND NOT PRTD           
         BZ    PO20                       THEN MARK ENTRIES PRINTED             
*                                                                               
         MVC   WORK(XMTPQKYQ),XMTTABLD    SKIP PAST LOGICAL REPORTS             
PO11     AHI   R7,XMTTBLQ                                                       
         MVC31 XMTENTRY,0(R7)                                                   
         CLC   0(XMTPQKYQ,R3),WORK                                              
         BE    PO11                                                             
         B     PO5                 THIS IS A DIFFERENT PQ REPORT                
*                                                                               
PO20     MVC   WORK(XMTPQKYQ),XMTTABLD                                          
         CLI   XMTMETH,C'F'        FTP REPORT?                                  
         BNE   *+8                                                              
         MVI   BYTE2,C'F'          YES - WE WON'T CHANGE RETAIN TIME            
         AHI   R7,XMTTBLQ                                                       
         MVC31 XMTENTRY,0(R7)                                                   
         CLC   0(XMTPQKYQ,R3),WORK                                              
         BE    PO10                SAME PQ REPORT                               
         DROP  R3                                                               
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
* GET PQ CLASS/STATUS OVERRIDE IF ANY                                           
*                                                                               
         MVC   XMTENTRY,XMTENTR2                                                
         LA    RE,XMTENTRY         WORK HOLDS PQ ENTRY TO BE MARKED             
         USING XMTTABLD,RE                                                      
         TM    XMTFLAGS,XMTCPQSQ   CHANGE PQ CLASS/STATUS?                      
         BZ    PO25                NO                                           
         DROP  RE                                                               
*                                  YES - GET EDICT FILE ENTRY                   
         MVI   BYTE,C'G'                                                        
         BRAS  RE,POSTEDFL         RETURN EDICT FILE ENTRY IN EDFENTRY          
*                                                                               
PO25     L     R6,=A(CITABLE)      PRINT QUEUE INFO TABLE                       
         LA    R4,XMTENTRY         PQ ENTRY TO MARK PRINTED                     
         USING XMTTABLD,R4                                                      
         SR    R0,R0                                                            
         IC    R0,XMTPRTQ                                                       
         BCTR  R0,0                                                             
         MHI   R0,CITBLLNQ                                                      
         AR    R6,R0               R6 = A(THIS PQ ENTRY)                        
*                                                                               
         XC    PQINDEX,PQINDEX                                                  
         USING UKRECD,R5                                                        
         LA    R5,PQINDEX          BUILD PRINT QUEUE INDEX                      
         MVC   UKSRCID,XMTUSRID                                                 
         MVC   UKSUBID,XMTSUBID                                                 
         MVC   UKREPNO,XMTREFNO                                                 
         DROP  R4                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,UKSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),UKSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,UKREPNO                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         PRNT  CHECKIFPRINTED,PRINT=ALWAYS                                      
*                                                                               
         LA    R9,CFPQID           MINOR RESOURCE NAME                          
*========                                                                       
         LA    R9,=C'PRTQU'        *** UNTIL DMENQDEQ IS RE-ENTRANT ***         
*========                                                                       
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,5)  ENQUEUE THE PRINT QUEUE                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'INDEX'),CFPQID,PQINDEX,R,CXREC                
         CLI   DMCB+8,0            REPORT FOUND?                                
         BE    PO30                                                             
         PRNT  REPORTWASPURGED,PRINT=ALWAYS                                     
         B     PO50                NO                                           
*                                                                               
PO30     TM    UKSTAT,X'20'+QLSTHO PRINTED OR ON HOLD?                          
         BZ    PO40                                                             
         PRNT  PRINTEDORHELD,PRINT=ALWAYS                                       
         B     PO50                YES                                          
*                                                                               
PO40     EQU   *                                                                
         LA    RE,XMTENTRY         WORK HOLDS PQ ENTRY TO BE MARKED             
         USING XMTTABLD,RE                                                      
         TM    XMTFLAGS,XMTCPQSQ   CHANGE PQ CLASS/STATUS?                      
         BZ    PO45                NO - MARK PQ PRINTED                         
*                                  YES - UPDATE PQ CLASS/STATUS                 
*                                                                               
         L     R4,=A(EDFENTRY)                                                  
         MVC   UKINFO(1),EDFCPQCL-EDFILD(R4)                                    
*ANOTHER NEW FIELD "EDFCPQST" IS NOT BEEN USED YET                              
         GOTO1 DATAMGR,DMCB,(0,=C'CLARET'),CFPQID,PQINDEX,R,CXREC               
         MVC   P+30(1),EDFCPQCL-EDFILD(R4)                                      
         PRNT  CHANGE_PQS,PRINT=ALWAYS                                          
         B     PO50                                                             
         DROP  RE                                                               
*                                                                               
PO45     GOTO1 DATAMGR,DMCB,(0,=C'PRINTED'),CFPQID,PQINDEX,R,CXREC              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  MARKEDPRINTEDOK1,PRINT=ALWAYS                                    
*                                                                               
PO48     CLI   BYTE2,C'F'          ANY FTP TRANSMISSION IN THIS REPORT?         
         BE    PO50                YES - DON'T CHANGE THE RETAIN TIME           
         MVI   UKFLAG,UKFLHRS      HOURS PASSED BACK IN UKINFO                  
         MVC   UKINFO,=H'1'        SET RETAIN TIME TO ONE HOUR                  
         GOTO1 DATAMGR,DMCB,(0,=C'RETAIN'),CFPQID,PQINDEX,R,CXREC               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  RETAINCHANGED,PRINT=ALWAYS                                       
         DROP  R5                                                               
*                                                                               
PO50     LA    R9,CFPQID           MINOR RESOURCE NAME                          
*========                                                                       
         LA    R9,=C'PRTQU'        *** UNTIL DMENQDEQ IS RE-ENTRANT ***         
*========                                                                       
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),5)    DEQUEUE THE PRINT QUEUE                      
*                                                                               
         BRAS  RE,CHKOPER          CHECK FOR OPERATOR INTERVENTION              
*                                                                               
         OC    LASTTIME,LASTTIME                                                
         BZ    PO70                                                             
*                                                                               
         TIME  BIN                 TIME NOW                                     
         S     R0,LASTTIME         TIME DIFFERENCE FROM LAST PQ SCAN            
         L     RE,WAITSECS                                                      
         MHI   RE,3                3X WAITSECS                                  
         SR    RE,R0               TOO LONG IN THIS PURGEOLD ROUNTINE?          
         BP    PO70                NO                                           
         MVI   POLDEXIT,C'Y'       YES                                          
*                                                                               
PO70     LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
*                                  FIND 1ST LOGICAL REPORT                      
         GOTO1 =V(BINSRCH),DMCB,XMTENTR2,AXMTTBL,,XMTTBLQ,XMTKEYQ,0             
         SAM24                                                                  
*                                                                               
         ICM   R7,15,DMCB          A(1ST LOGICAL REPORT)                        
         TM    DMCB,X'80'          REPORT FOUND?                                
         BNO   *+6                 YES                                          
         DC    H'0'                NO                                           
*                                                                               
         MVC31 XMTENTRY,0(R7)                                                   
*                                                                               
PO90     OI    XMTSTAT-XMTTABLD+XMTENTRY,EDFSTPRT                               
         MVC31 0(L'XMTENTRY,R7),XMTENTRY   REMEMBER WE MARKED PRTD              
         MVI   BYTE,C'P'           WE'VE MARKED THE REPORT PRINTED              
         BRAS  RE,POSTEDFL         POST THIS IN EDICT FILE                      
         AHI   R7,XMTTBLQ          R7 = A(PRINTED ENTRY)                        
         MVC31 XMTENTRY,0(R7)                                                   
         CLC   XMTENTRY(XMTPQKYQ),XMTENTR2                                      
         BE    PO90                MARK EACH LOGICAL REPORT IN PQ ENTRY         
         B     PO5                 CHECK OUT THE NEXT REPORT                    
*                                                                               
POX      LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         XIT1                                                                   
POLDEXIT DC    C'N'                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ PARAMETER CARDS BETWEEN "++MAIN" AND "++MAINEND" CARDS                   
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
* * DSPACE=X         DSPACE CONTROL CARD                                        
*   WAITSECS=N       WAIT N SECONDS BETWEEN PQ SEARCHES (DEFAULT = 60)          
*   MAXDESTS=N       MAXINUM NUMBER DESTINATIONS/REPORT (DEFAULT = 100)         
*   EDICTTYPE=A/R    THIS EDICT ACTS LIKE 'EDICTA' OR 'EDICTR'                  
*   XMITNJE=YES      ATTACH NJE SENDING SUBTASK (DEFAULT=NO)                    
*   XMITNFX=NO       DON'T ATTACH NFX SENDING SUBTASK (DEFAULT=YES)             
*   XMITENCODA=NO    DON'T ATTACH ENCODA SUBTASK (DEFAULT=YES)                  
*   XMITBDE=NO       DON'T ATTACH BDE-EMAIL SUBTASK (DEFAULT=YES)               
*   XMITBDF=NO       DON'T ATTACH BDE-FTP SUBTASK (DEFAULT=NO)                  
*   XMITMQM=NO       DON'T ATTACH E-MAIL SUBTASK (DEFAULT=YES)                  
*   XMITFTP=NO       DON'T ATTACH FTP SENDING SUBTASK (DEFAULT=YES)             
*   XMITPDF=NO       DON'T ATTACH PDF SENDING SUBTASK (DEFAULT=YES)             
*   XMITEASYLINK=NO  DON'T ATTACH EASYLINK SUBTASK (DEFAULT=YES)                
*   XMITDARE=NO      DON'T ATTACH DARE SUBTASK (DEFAULT=YES)                    
*   XMITCOL=NO       DON'T ATTACH COLUMBINE SUBTASK (DEFAULT=YES)               
*   XMITADVN=YES     DON'T ATTACH ADVANTIS SUBTASK (DEFAULT=NO)                 
*   XMITFAXGATE=NO   DON'T ATTACH FAXGATE SUBTASK (DEFAULT=YES)                 
*   XMITBIAS=NO      DON'T ATTACH BIAS SUBTASK (DEFAULT=YES)                    
*   NOOP=REPX        WHICH REP SYSTEMS NOT TO OPEN                              
*                                                                               
*   "*" MEANS REQUIRED CARD                                                     
*   THE FOLLOWING PARAMETERS ARE REALLY FOR TESTING/DEBUGGING ONLY              
*                                                                               
*   USERID=UUUUUUUU  TRANSFER REPORTS FROM THIS USERID ONLY (MAX 50)            
*   EXUSERID=UUUUUUUU EXCLUDE REPORTS FROM THIS USERID ONLY (MAX 50)            
*    (CAN HAVE EITHER USERID OR EXUSERID, BUT NOT BOTH)                         
*   SUBID=CCC        TRANSFER REPORTS WITH THIS SUB-ID ONLY                     
*   EZSTASK=CCCCCCCC  NAME OF AT&T EASYLINK SUBTASK (DEFAULT=EDIEZS)            
*   NJETASK=CCCCCCCC  NAME OF NJE SUBTASK (DEFAULT=EDINJE)                      
*   NFXTASK=CCCCCCCC  NAME OF NFX SUBTASK (DEFAULT=EDINFX)                      
*   ENCTASK=CCCCCCCC  NAME OF ENCODE SUBTASK (DEFAULT=EDIENCS)                  
*   BDETASK=CCCCCCCC  NAME OF BDE-EMAIL SUBTASK (DEFAULT=EDIBDES)               
*   BDFTASK=CCCCCCCC  NAME OF BDE-FTP SUBTASK (DEFAULT=EDIBDFS)                 
*   MQMTASK=CCCCCCCC  NAME OF E-MAIL SUBTASK (DEFAULT=EDIMQMS)                  
*   FTPTASK=CCCCCCCC  NAME OF FTP SUBTASK (DEFAULT=EDIFTP)                      
*   DARETASK=CCCCCCCC NAME OF DARE SUBTASK (DEFAULT=EDIDARS)                    
*   COLTASK=CCCCCCCC  NAME OF COLUMBINE SUBTASK (DEFAULT=EDICOL)                
*   ADVNTASK=CCCCCCCC NAME OF ADVANTIS SUBTASK (DEFAULT=EDIADVN)                
*   BIASTASK=CCCCCCCC NAME OF BIAS SUBTASK (DEFAULT=EDIBIAS)                    
*   FAXGATETASK=CCCCCCCC NAME OF FAXGATE SUBTASK (DEFAULT=EDIFAX)               
*   PQSCAN=NO        DON'T SCAN THE PRINT QUEUES AT ALL (DEFAULT=YES)           
*   TRACE=YES        PRINT DETAILED TRACE (DEFAULT=NO)                          
*   LOGGINGID=C      APPEND THIS CHARACTER TO DARE LOGGING INFO                 
*   DRTEST=YES/NO    Y = DISASTER RECOVERY TEST MODE                            
*   XMITTBLSIZE=N    N = DAYS OF TRANSACTIONS WILL STORE IN XMITTBLE            
*                        (DEFAULT=5 DAYS)                                       
*                                                                               
*   CBSFIX=YES       SPECIAL HARD CODE TO GET AROUND CBS BDF BACKLOG            
*   BDF2=YES         SPECIAL CODE TO POST BDE SUBTASK AS 2ND BDF                
*   ALWAYS_ADD_METHOD=?  ONE CHAR FOR METHOD TYPE, ALWAYS ADD THIS (10)         
*   NEVER_ADD_METHOD=?   ONE CHAR FOR METHOD TYPE, NEVER ADD THIS (10)          
*                                                                               
**********************************************************************          
READCRDS NTR1  BASE=*,LABEL=*                                                   
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++MAIN',CARD     LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =C'DSPACE=',CARD    DSPACE=X                                     
         JNE   *+2                                                              
         MVC   DSPACE,CARD+7       PASS DSPACE CARD TO MASTER                   
         USING SSBD,RF                                                          
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC,DSPACE                                                  
         DROP  RF                                                               
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
         BRAS  RE,OPENCTFL         OPEN CONTROL FILE                            
*                                                                               
RC20     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++MAINEND',CARD  LOOK FOR END OF PARAMETERS                   
         BE    RCXIT                                                            
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC20                YES                                          
*                                                                               
         LA    RE,CARD             BLANK DELIMITS A COMMENT                     
         LA    R0,C' '                                                          
         SRST  R0,RE               R0 = A(FIRST BLANK)                          
         LR    R1,R0                                                            
         LA    R1,1(R1)            BUMP TO NEXT CHARACTER IN CARD               
         C     R1,=A(CARD+79)      STOP AT END OF CARD                          
         BH    *+12                                                             
         MVI   0(R1),C' '          REPLACE COMMENT WITH BLANKS                  
         B     *-16                                                             
*                                                                               
         CLC   =C'WAITSECS=',CARD  WAITSECS=N                                   
         BNE   RC25                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         MHI   R1,100              SCALE THE TIME INTERVAL FOR STIMER           
         ST    R1,WAITSECS                                                      
         B     RC20                                                             
*                                                                               
RC25     CLC   =C'MAXDESTS=',CARD  MAXDESTS=N                                   
         BNE   RC30                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         STH   R1,MAXDESTS                                                      
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'USERID=',CARD    USERID=XXX                                   
         BNE   RC40                                                             
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),CARD+7   USER-ID                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ID RECORD                                 
         L     R4,=A(IO)                                                        
         LA    R4,28(R4)           FIND ID ELEMENT (X'02')                      
RC35     CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                IT'S NOT THERE                               
         CLI   0(R4),X'02'                                                      
         BE    *+16                                                             
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RC35                                                             
*                                                                               
         L     RF,=A(USERFILT)                                                  
         LA    RE,USRFMAXQ                                                      
         OC    0(L'USERFILT,RF),0(RF)     ANY MORE EMPTY ENTRY?                 
         BZ    *+14                       YES - SAVE THE FILTER                 
         AHI   RF,L'USERFILT                                                    
         BCT   RE,*-14                    CHECK FOR NEXT EMPTY ENTRY            
         DC    H'0'                EXCESS THE MAX # OF USERID FILTERS           
         MVC   0(L'USERFILT,RF),2(R4)     HEX USERID FILTER                     
         B     RC20                                                             
*                                                                               
RC40     CLC   =C'SUBID=',CARD     SUBID=                                       
         BNE   *+14                                                             
         MVC   SUBFILT,CARD+6                                                   
         B     RC20                                                             
*                                                                               
         CLC   =C'EXSUBID=',CARD   EXSUBID=                                     
         BNE   *+14                                                             
         MVC   EXSUBFT,CARD+8                                                   
         B     RC20                                                             
*                                                                               
         CLC   =C'EXUSERID=',CARD  EXUSERID=XXX                                 
         BNE   RC50                                                             
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),CARD+9   USER-ID                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO ID RECORD                                 
         L     R4,=A(IO)                                                        
         LA    R4,28(R4)           FIND ID ELEMENT (X'02')                      
RC45     CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                IT'S NOT THERE                               
         CLI   0(R4),X'02'                                                      
         BE    *+16                                                             
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RC45                                                             
*                                                                               
         L     RF,=A(EXUFILT)                                                   
         LA    RE,EXUFMAXQ                                                      
         OC    0(L'EXUFILT,RF),0(RF)      ANY MORE EMPTY ENTRY?                 
         BZ    *+14                       YES - SAVE THE FILTER                 
         AHI   RF,L'EXUFILT                                                     
         BCT   RE,*-14                    CHECK FOR NEXT EMPTY ENTRY            
         DC    H'0'                EXCESS THE MAX # OF USERID FILTERS           
         MVC   0(L'EXUFILT,RF),2(R4)      HEX USERID NEGATIVE FILTER            
         B     RC20                                                             
*                                                                               
RC50     CLC   =C'XMITQ2Q=',CARD   XMITQ2Q=, IGNORE THIS                        
         BE    RC20                                                             
         CLC   =C'XMITNFX=',CARD   XMITNFX=                                     
         BNE   RC53                                                             
         CLC   =C'NO',CARD+8                                                    
         BE    RC20                XMITNFX=NO IS THE DEFAULT                    
         CLC   =C'YES',CARD+8                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITNFX,C'Y'        XMITNFX=YES                                  
         B     RC20                                                             
*                                                                               
RC53     CLC   =C'XMITENCODA=',CARD   XMITENCODA=                               
         BNE   RC55                                                             
         CLC   =C'YES',CARD+11                                                  
         BE    RC20                XMITENC=YES IS THE DEFAULT                   
         CLC   =C'NO',CARD+11                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITENC,C'N'        XMITENC=NO                                   
         B     RC20                                                             
*                                                                               
RC55     CLC   =C'XMITBDE=',CARD   XMITBDE=                                     
         BNE   RC58                                                             
         CLC   =C'NO',CARD+8                                                    
         BE    RC20                XMITBDE=NO IS THE DEFAULT                    
         CLC   =C'YES',CARD+8                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITBDE,C'Y'        XMITBDE=YES                                  
         B     RC20                                                             
*                                                                               
RC58     CLC   =C'XMITBDF=',CARD   XMITBDF=                                     
         BNE   RC60                                                             
         CLC   =C'YES',CARD+8                                                   
         BE    RC20                XMITBDF=YES IS THE DEFAULT                   
         CLC   =C'NO',CARD+8                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITBDF,C'N'        XMITBDF=NO                                   
         B     RC20                                                             
*                                                                               
RC60     CLC   =C'XMITMQM=',CARD   XMITMQM=                                     
         BNE   RC65                                                             
         CLC   =C'YES',CARD+8                                                   
         BE    RC20                XMITMQM=YES IS THE DEFAULT                   
         CLC   =C'NO',CARD+8                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITMQM,C'N'        XMITMQM=NO                                   
         B     RC20                                                             
*                                                                               
RC65     CLC   =C'XMITNJE=',CARD   XMITNJE=                                     
         BNE   RC70                                                             
         CLC   =C'NO',CARD+8                                                    
         BE    RC20                XMITNJE=NO IS THE DEFAULT                    
         CLC   =C'YES',CARD+8                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITNJE,C'Y'        XMITNJE=YES                                  
         B     RC20                                                             
*                                                                               
RC70     CLC   =C'XMITFTP=',CARD   XMITFTP=                                     
         BNE   RC75                                                             
         CLC   =C'YES',CARD+8                                                   
         BE    RC20                XMITFTP=YES IS THE DEFAULT                   
         CLC   =C'NO',CARD+8                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITFTP,C'N'        XMITFTP=NO                                   
         B     RC20                                                             
*                                                                               
RC75     CLC   =C'XMITBIAS=',CARD  XMITBIAS=                                    
         BNE   RC78                                                             
         CLC   =C'YES',CARD+9                                                   
         BE    RC20                XMITBIAS=YES IS THE DEFAULT                  
         CLC   =C'NO',CARD+9                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITBIAS,C'N'       XMITBIAS=NO                                  
         B     RC20                                                             
*                                                                               
RC78     CLC   =C'XMITPDF=',CARD   XMITPDF=                                     
         BNE   RC80                                                             
         CLC   =C'YES',CARD+8                                                   
         BE    RC20                XMITPDF=YES IS THE DEFAULT                   
         CLC   =C'NO',CARD+8                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITPDF,C'N'        XMITPDF=NO                                   
         B     RC20                                                             
*                                                                               
RC80     CLC   =C'XMITEASYLINK=',CARD   XMITEASYLINK=                           
         BNE   RC85                                                             
         CLC   =C'NO',CARD+13                                                   
         BE    RC20                XMITEASYLINK=NO IS THE DEFAULT               
         CLC   =C'YES',CARD+13                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITEZ,C'Y'         XMITEASYLINK=YES                             
         B     RC20                                                             
*                                                                               
RC85     CLC   =C'XMITADVANTIS=',CARD   XMITADVANTIS=                           
         BNE   RC88                                                             
         CLC   =C'NO',CARD+13                                                   
         BE    RC20                XMITADVANTIS=NO IS THE DEFAULT               
         CLC   =C'YES',CARD+13                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITADVN,C'Y'       XMITADVANTIS=YES                             
         B     RC20                                                             
*                                                                               
RC88     CLC   =C'XMITFAXGATE=',CARD   XMITFAXGATE=                             
         BNE   RC90                                                             
         CLC   =C'YES',CARD+12                                                  
         BE    RC20                XMITFAXGATE=YES IS THE DEFAULT               
         CLC   =C'NO',CARD+12                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITFAXG,C'N'       XMITFAXGATE=NO                               
         B     RC20                                                             
*                                                                               
RC90     CLC   =C'XMITDARE=',CARD  XMITDARE=                                    
         BNE   RC95                                                             
         CLC   =C'YES',CARD+9                                                   
         BE    RC20                XMITDARE=YES IS THE DEFAULT                  
         CLC   =C'NO',CARD+9                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITDARE,C'N'       XMITDARE=NO                                  
         B     RC20                                                             
*                                                                               
RC95     CLC   =C'XMITCOLUMBINE=',CARD  XMITCOLUMBINE=                          
         BNE   RC100                                                            
         CLC   =C'YES',CARD+14                                                  
         BE    RC20                XMITCOLUMBINE=YES IS THE DEFAULT             
         CLC   =C'NO',CARD+14                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   XMITCOL,C'N'        XMITCOLUMBINE=NO                             
         B     RC20                                                             
*                                                                               
RC100    CLC   =C'EZSTASK=',CARD   EZSTASK=                                     
         BNE   *+14                                                             
         MVC   EZSTASK,CARD+8      AT&T EASYLINK SUBTASK NAME                   
         B     RC20                                                             
*                                                                               
         CLC   =C'NJETASK=',CARD   NJETASK=                                     
         BNE   *+14                                                             
         MVC   NJETASK,CARD+8      NJE SUBTASK NAME                             
         B     RC20                                                             
*                                                                               
         CLC   =C'NFXTASK=',CARD   NFXTASK=                                     
         BNE   *+14                                                             
         MVC   NFXTASK,CARD+8      NFX SUBTASK NAME                             
         B     RC20                                                             
*                                                                               
         CLC   =C'ENCTASK=',CARD   ENCTASK=                                     
         BNE   *+14                                                             
         MVC   ENCTASK,CARD+8      ENCODA SUBTASK NAME                          
         B     RC20                                                             
*                                                                               
         CLC   =C'BDETASK=',CARD   BDETASK=                                     
         BNE   *+14                                                             
         MVC   BDETASK,CARD+8      BDE-EMAIL SUBTASK NAME                       
         B     RC20                                                             
*                                                                               
         CLC   =C'BDFTASK=',CARD   BDFTASK=                                     
         BNE   *+14                                                             
         MVC   BDFTASK,CARD+8      BDE-FTP SUBTASK NAME                         
         B     RC20                                                             
*                                                                               
         CLC   =C'MQMTASK=',CARD   MQMTASK=                                     
         BNE   *+14                                                             
         MVC   MQMTASK,CARD+8      E-MAIL SUBTASK NAME                          
         B     RC20                                                             
*                                                                               
         CLC   =C'FTPTASK=',CARD   FTPTASK=                                     
         BNE   *+14                                                             
         MVC   FTPTASK,CARD+8      FTP SUBTASK NAME                             
         B     RC20                                                             
*                                                                               
         CLC   =C'BIASTASK=',CARD  BIASTASK=                                    
         BNE   *+14                                                             
         MVC   BIASTASK,CARD+9     BIAS SUBTASK NAME                            
         B     RC20                                                             
*                                                                               
         CLC   =C'PDFTASK=',CARD   PDFTASK=                                     
         BNE   *+14                                                             
         MVC   BIASTASK,CARD+9     PDF SUBTASK NAME                             
         B     RC20                                                             
*                                                                               
         CLC   =C'DARETASK=',CARD  DARETASK=                                    
         BNE   *+14                                                             
         MVC   DARETASK,CARD+9     DARE SUBTASK NAME                            
         B     RC20                                                             
*                                                                               
         CLC   =C'COLTASK=',CARD   COLTASK=                                     
         BNE   *+14                                                             
         MVC   COLTASK,CARD+8      COLUMBINE SUBTASK NAME                       
         B     RC20                                                             
*                                                                               
         CLC   =C'ADVNTASK=',CARD  ADVNTASK=                                    
         BNE   *+14                                                             
         MVC   ADVNTASK,CARD+9     ADVANTIS SUBTASK NAME                        
         B     RC20                                                             
*                                                                               
         CLC   =C'FAXGATETASK=',CARD  FAXGATETASK=                              
         BNE   *+14                                                             
         MVC   FAXGTASK,CARD+12    FAXGATE SUBTASK NAME                         
         B     RC20                                                             
*                                                                               
         CLC   =C'EDICTTYPE=',CARD EDICTA VS. EDICTR                            
         BNE   RC110                                                            
         CLI   CARD+10,C'A'                                                     
         BE    RC105                                                            
         CLI   CARD+10,C'R'                                                     
         BE    RC105                                                            
         CLI   CARD+10,C'2'        SPECIAL FOR CBS BDF PROBLEM                  
         BE    RC105                                                            
         DC    H'0'                'A' AND 'R' ARE ONLY VALID VALUES            
RC105    MVC   EDICTTYP,CARD+10                                                 
         B     RC20                                                             
*                                                                               
RC110    CLC   =C'PQSCAN=',CARD    PQSCAN=                                      
         BNE   RC120                                                            
         CLC   =C'YES',CARD+7                                                   
         BE    RC20                PQSCAN=YES IS THE DEFAULT                    
         CLC   =C'NO',CARD+7                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PQSCAN,C'N'         PQSCAN=NO                                    
         B     RC20                                                             
*                                                                               
RC120    CLC   =C'CBSFIX=',CARD    SPECIAL HARD CODE FOR CBS                    
         BNE   RC130                                                            
         CLC   =C'NO',CARD+7                                                    
         BE    RC20                PQSCAN=YES IS THE DEFAULT                    
         CLC   =C'YES',CARD+7                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CBSFIX,C'Y'         CBSFIX=YES                                   
         B     RC20                                                             
*                                                                               
RC130    CLC   =C'BDF2=',CARD      SPECIAL CODE FOR 2ND BDF                     
         BNE   RC160                                                            
         CLC   =C'NO',CARD+5                                                    
         BE    RC20                PQSCAN=YES IS THE DEFAULT                    
         CLC   =C'YES',CARD+5                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BDF2,C'Y'           BDF2=YES                                     
         B     RC20                                                             
*                                                                               
RC160    CLC   =C'TRACE=',CARD     TRACE=                                       
         BNE   RC170                                                            
         CLC   =C'NO',CARD+6                                                    
         BE    RC20                TRACE=NO IS THE DEFAULT                      
         CLC   =C'YES',CARD+6                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC20                                                             
*                                                                               
RC170    CLC   =C'LOGGINGID=',CARD LOGGINGID=                                   
         BNE   *+14                                                             
         MVC   LOGID,CARD+10                                                    
         B     RC20                                                             
*                                                                               
         CLC   =C'DRTEST=',CARD    DISASTER RECOVERY TEST MODE                  
         BNE   RC180                                                            
         CLC   =C'NO',CARD+7                                                    
         BE    RC20                                                             
         CLC   =C'YES',CARD+7                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DRTEST,C'Y'                                                      
         B     RC20                                                             
*                                                                               
RC180    CLC   =C'XMITTBLSIZE=',CARD  XMITTBLSIZE=N                             
         BNE   RC190                                                            
         GOTO1 =V(NUMVAL),DMCB,CARD+12,(2,0)                                    
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         CHI   R1,31               EXCESS THE LIMIT OF EDICT FILE?              
         BNH   *+6                 NO                                           
         DC    H'0'                                                             
         CHI   R1,2                LESS THAN 2 DAY?                             
         BNL   *+6                 NO                                           
         DC    H'0'                                                             
         STH   R1,XTBLSIZE                                                      
         B     RC20                                                             
*                                                                               
RC190    CLC   =C'ALWAYS_ADD_METHOD=',CARD     ALWAYS_ADD_METHOD=?              
         BNE   RC200                                                            
*                                                                               
         LA    RF,METHINCL                                                      
         LA    RE,MTHPMAXQ                                                      
RC191    OC    0(L'METHINCL,RF),0(RF)  ANY MORE EMPTY ENTRY?                    
         BZ    RC192                   YES - SAVE THE FILTER                    
         AHI   RF,L'METHINCL                                                    
         BCT   RE,RC191                CHECK FOR NEXT EMPTY ENTRY               
         DC    H'0'                EXCESS THE MAX # OF USERID FILTERS           
RC192    MVC   0(L'METHINCL,RF),CARD+18 SAVE METHOD TYPE FILTER                 
         B     RC20                                                             
*                                                                               
RC200    CLC   =C'NEVER_ADD_METHOD=',CARD      NEVER_ADD_METHOD=?               
         BNE   RC300                                                            
*                                                                               
         LA    RF,METHXCLD                                                      
         LA    RE,MTHNMAXQ                                                      
RC201    OC    0(L'METHXCLD,RF),0(RF)  ANY MORE EMPTY ENTRY?                    
         BZ    RC202                   YES - SAVE THE FILTER                    
         AHI   RF,L'METHXCLD                                                    
         BCT   RE,RC201                CHECK FOR NEXT EMPTY ENTRY               
         DC    H'0'                EXCESS THE MAX # OF USERID FILTERS           
RC202    MVC   0(L'METHINCL,RF),CARD+17 SAVE METHOD TYPE FILTER                 
         B     RC20                                                             
*                                                                               
RC300    CLC   =C'NOOP=REP',CARD   EXCLUDE THESE REPS                           
         JNE   *+2                                                              
         LARL  RE,NOOPREPS                                                      
RC310    CLI   0(RE),C' '          FIND OPEN SLOT                               
         BE    RC312                                                            
         AHI   RE,2                                                             
         B     RC310               LOOK FOR FREE SLOT                           
*                                                                               
RC312    MVC   0(2,RE),CARD+8      MOVE IN SYSTEM ID                            
         B     RC20                                                             
*                                                                               
RCXIT    GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
***********************************************************************         
***NOTE: COMMENT OUT FROM 6/7/12, REP FILE IS WRITABLE NOW ON SUNDAY.**         
*ON SUNDAY, DON'T ATTACH DARE SUBTASK FOR EDICT'R'                    *         
*BECAUSE CAN'T WRITE TO REP FILE                                      *         
*                                                                     *         
*        CLI   MAJORNAM+5,C'R'     EDICTR?                            *         
*        BNE   RCXX                                                   *         
*                                                                     *         
*        GOTO1 =V(DATCON),DMCB,(5,0),(0,CARD)    TODAY'S DATE         *         
*        GOTO1 =V(GETDAY),DMCB,(0,CARD),(0,FULL) GET DAY OF WEEK      *         
*                                                                     *         
*        CLC   =C'SUN',FULL        IS THIS SUNDAY?                    *         
*        BNE   RCXX                                                   *         
*                                                                     *         
*        MVI   XMITDARE,C'N'       XMITDARE=NO                        *         
***********************************************************************         
*                                                                               
RCXX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WAIT A WHILE BEFORE LOOKING FOR MORE REPORTS.  ALLOW THE WAIT TO              
* BE INTERRUPTED BY THE OPERATOR.                                               
***********************************************************************         
WAITABIT NTR1  BASE=*,LABEL=*                                                   
         OC    LASTTIME,LASTTIME                                                
         BNZ   *+14                                                             
         MVC   WAITSEC2,WAITSECS                                                
         B     WAB10                                                            
*                                                                               
         TIME  BIN                                                              
         S     R0,LASTTIME                                                      
         L     R1,WAITSECS                                                      
         SR    R1,R0                                                            
         BNP   WAITBITX                                                         
         ST    R1,WAITSEC2                                                      
*                                                                               
WAB10    PRNT  WAITAWHILE,PRINT=ALWAYS                                          
*                                                                               
         XC    TIMERECB,TIMERECB                                                
         STIMERM SET,ID=STIMER1,BINTVL=WAITSEC2,EXIT=TIMERXIT                   
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR TIMER POP OR OPERATOR               
*                                                                               
         TM    TIMERECB,X'40'                                                   
         BO    WAITBITX            TIMER POPPED                                 
*                                                                               
         STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF               OPERATOR INTERRUPTED -- CANCEL TIMER         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WAITBITX XIT1                                                                   
         SPACE 5                                                                
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
TIMERECB DS    F                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
BLDPQTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  BUILD_PQTABLE,PRINT=ALWAYS                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GLIST'),=C'PRTQUE',A(IO),0,CXREC              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,=A(IO)                                                        
         L     R4,32(R4)           SAVE A(PRINT QUEUE LIST)                     
*                                                                               
         ZIC   R3,0(R4)            NUMBER OF PRINT QUEUES                       
         ST    R3,NUMPQS                                                        
*                                                                               
         SR    R2,R2               REPORT COUNTER                               
         L     R6,=A(CITABLE)      A(PRINT QUEUE TABLE)                         
*                                                                               
BP10     LA    R4,8(R4)            BUMP TO NEXT PRINT QUEUE                     
         XC    0(CITBLLNQ,R6),0(R6)                                             
         MVC   DUB(4),=C'PRTQ'     CONSTRUCT PRINT QUEUE NAME                   
         MVC   DUB+4(1),1(R4)                                                   
         MVC   CFPQENUM,4(R4)                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'BUFFER'),DUB,0,0,CXREC                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CIDATA,CXREC+12     CIDATA IS HERE                               
         L     RF,=V(DMENQDEQ)     V(DMENQDEQ)                                  
         ST    RF,CIENQDEQ                                                      
******************FOR BACKWARD COMPATIBLE**********************                 
         LHI   RF,24               LENGTH OF OLD PQ KEY                         
         CLI   CIDATA+16,0                                                      
         BNE   *+8                                                              
         LHI   RF,40               LENGTH OF NEW PQ KEY                         
         STH   RF,CINDXLN                                                       
         MVC   CFPQID,DUB                                                       
         MVC   CFPQINUM,0(R4)      PQ FILE INTERNAL NUMBER                      
******************FOR BACKWARD COMPATIBLE**********************                 
         L     RF,CXREC+8          DISPLACEMENT TO PQ SAVE AREA                 
         LA    RF,CXREC(RF)                                                     
         ST    RF,APQSAVE          A(PQ SAVE AREA)                              
         LA    R6,CITBLLNQ(R6)                                                  
         BCT   R3,BP10             LOOK AT ALL PRINT QUEUES                     
*                                                                               
         PRNT  PQTABLE_BUILT,PRINT=ALWAYS                                       
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ALLOCATE THE DESTINATION TABLE OVER 16M LINE                                  
**********************************************************************          
ALODEST  NTR1  BASE=*,LABEL=*                                                   
         L     R3,=A(DESTTBLL*DESTTBLQ)  TABLE SIZE                             
         AHI   R3,10               ROOM FOR LABEL(8), L'ENTRY(2)                
         LR    R4,R3               SAVE THIS LENGTH                             
         AHI   R3,DESTTBLQ         ROOM FOR 1 EOT MARKER ENTRY                  
*                                                                               
         SAM31                                                                  
         STORAGE OBTAIN,LENGTH=(3),LOC=ANY,BNDRY=PAGE                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(8,R1),=C'*DESTTBL'                                             
         MVC   8(2,R1),=AL2(DESTTBLQ)                                           
         AR    R4,R1                                                            
         ST    R4,ADESTTBX         A(END OF DESTTBL)                            
         MVI   0(R4),X'FF'         X'FF' FILL THE EOT MARKER ENTRY              
         MVC   1(DESTTBLQ-1,R4),0(R4)                                           
         AHI   R1,10               BUMP PAST LABEL AND L'ENTRY                  
         ST    R1,ADESTTBL         A(START OF DESTTBL)                          
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUILD A CORE TABLE CONTAINING ALL DATA FROM ALL EDICT RECORDS.                
**********************************************************************          
BLDDEST  NTR1  BASE=*,LABEL=*                                                   
         PRNT  BUILDDESTTABLE,PRINT=ALWAYS                                      
*                                                                               
         LA    R9,=C'DSTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE DESTINATION TABLE                
*                                                                               
         LA    R9,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         MVI   DESTNTRY,X'FF'                                                   
         MVC   DESTNTRY+1(DESTTBLQ-1),DESTNTRY                                  
*                                                                               
         L     R3,ADESTTBL         A(TABLE)                                     
BD05     MVC31 0(DESTTBLQ,R3),DESTNTRY                                          
         AHI   R3,DESTTBLQ                                                      
         C     R3,ADESTTBX         END OF TABLE YET?                            
         BL    BD05                FILL ENTIRE TABLE WITH X'FF'S                
*                                                                               
         USING DESTTABD,R2                                                      
         LA    R2,DESTNTRY                                                      
*                                                                               
         L     R3,ADESTTBL         DESTINATION TABLE                            
         USING EDIKEYD,R5                                                       
         L     R5,=A(IO)                                                        
         XC    EDIKEY,EDIKEY       CLEAR KEY                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',(R5),(R5),0               
         CLI   DMCB+8,0                                                         
         BE    BD20                                                             
         DC    H'0'                                                             
*                                                                               
BD10     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',0,(R5),0                  
         CLI   DMCB+8,0                                                         
         BE    BD20                                                             
         DC    H'0'                                                             
*                                                                               
BD20     LR    R4,R5                                                            
         CLI   0(R4),EDIKSYSQ      EDICT RECORD?                                
         BNE   BD40                                                             
         CLI   1(R4),EDITYPEQ                                                   
         BNE   BD40                NO MORE EDICT RECORDS                        
*                                                                               
         MVI   ELCODE,EDILNKEQ     EDICT ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
*                                                                               
         USING EDILNKD,R4                                                       
         MVC   DESTNAME,EDINAME    USERID                                       
         MVC   DESTMETS,EDIMETHS   METHOD OF SENDING TRANSMISSIONS              
         MVC   DESTMETR,EDIMETHR   METHOD OF RECEIVING TRANSMISSIONS            
         MVC   DESTADVN,EDIADVNO   ADV EASYLINK MAILBOX NUMBER                  
         MVC   DESTREPN,EDIREPNO   REP EASYLINK MAILBOX NUMBER                  
         MVC   DESTNJEC,EDINJEC    NJE CLASS                                    
         MVC   DESTNJEN,EDINJEN    NJE NODE                                     
         MVC   DESTNJEU,EDINJEU    NJE USERID                                   
         MVC   DESTFTPO,EDIFTPO    FTP OPERATING SYSTEM                         
         MVC   DESTFTPL,EDIFTPL    FTP REMOTE LU NAME                           
         MVC   DESTFTPU,EDIFTPU    FTP APPC USERID                              
         MVC   DESTFTPP,EDIFTPP    FTP APPC PASSWORD                            
         MVC   DESTFTPS,EDIFTPS    FTP APPC SERVER CLASS                        
         MVC   DESTFTPF,EDIFFLGS   FTP FLAGS                                    
         MVC   DESTADNA,EDIADNA    ADVANTIS ACCOUNT                             
         MVC   DESTADNU,EDIADNU    ADVANTIS USERID                              
         MVC   DESTADNC,EDIADNC    ADVANTIS CLASS                               
         MVC   DESTCOLL,EDICOLL    COLUMBINE LUID                               
         MVC   DESTCOLU,EDICOLU    COLUMBINE APPC USERID                        
         MVC   DESTCOLP,EDICOLP    COLUMBINE APPC PASSWORD                      
         MVC   DESTBDECN,EDIBDECN  BDE COMMAN NAME                              
         MVC   DESTBDEOP,EDIBDEOP  BDE RECEIVER'S OPERATING SYSTEM              
         MVC   DESTBDEEN,EDIBDEEN  BDE ENCRYPTION (NONE,BLOWFISH,3DES)          
         MVC   DESTBDECM,EDIBDECM  BDE COMPRESS                                 
         MVC   DESTBDESF,EDIBDESF  BDE DELETE SENT FILE                         
         MVC   DESTBDECA,EDIBDECA  BDE CONVERT TO ASCII                         
         MVC   DESTBDECP,EDIBDECP  BDE CODE PAGE                                
         MVC   DESTBDEFN,EDIBDEFN  BDE FAILURE NOTIFICATION                     
         MVC   DESTBDEBI,EDIBDEBI  BDE BINARY DATA TRANSFER                     
*                                                                               
         MVI   DESTPM360,C'N'                                                   
         TM    EDIOPTS,EDIPM360    PM360 OPTION FROM EDICT RECORD               
         BZ    *+8                                                              
         MVI   DESTPM360,C'Y'                                                   
         DROP  R4                                                               
*                                                                               
         MVC31 0(DESTTBLQ,R3),DESTNTRY     MOVE ENTRY TO 31-BIT TABLE           
         AHI   R3,DESTTBLQ         BUMP TO NEXT SLOT IN TABLE                   
         C     R3,ADESTTBX                                                      
         BL    BD10                NEXT RECORD                                  
         DC    H'0'                MUST INCREASE DESTTBLL                       
*                                                                               
BD40     L     R3,ADESTTBL         GO BACK TO BEGINNING OF LIST                 
         LA    R2,DESTNTRY                                                      
*                                                                               
BD50     MVC31 DESTNTRY,0(R3)      GET AN ENTRY FROM DESTTBL                    
         CLI   DESTNAME,X'FF'      ANY MORE ENTRIES?                            
         BE    BD80                NO                                           
*                                                                               
         XC    0(25,R5),0(R5)      CLEAR KEY                                    
         XC    0(25,R5),0(R5)      CLEAR KEY                                    
         MVI   0(R5),C'I'          ID RECORD                                    
         MVC   23(2,R5),=C'  '                                                  
         MVC   15(8,R5),DESTNAME   USER-ID                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R5),(R5),0               
         CLI   DMCB+8,0                                                         
         BNE   BD60                NOT A DDS USER                               
*                                                                               
         LR    R4,R5                                                            
         MVI   ELCODE,2            PASSIVE POINTER ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DESTUIDN,2(R4)                                                   
         MVC31 0(DESTTBLQ,R3),DESTNTRY     MOVE ENTRY TO 31-BIT TABLE           
*                                                                               
BD60     AHI   R3,DESTTBLQ         BUMP TO NEXT SLOT IN TABLE                   
         B     BD50                                                             
         DROP  R2                                                               
*                                                                               
BD80     LA    R9,=C'DSTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE DESTINATION TABLE                
*                                                                               
         LA    R9,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         GETEL R4,28,ELCODE                                                     
         EJECT                                                                  
**********************************************************************          
* BUILD A CORE TABLE CONTAINING ALL COLUMBINE LUNAMES.                          
**********************************************************************          
BLDLUTAB NTR1  BASE=*,LABEL=*                                                   
         PRNT  BUILDCOLLUTAB,PRINT=ALWAYS                                       
*                                                                               
         LA    R9,=C'LUTABLE '     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE COLUMBINE LUTABLE                
*                                                                               
         L     R4,=A(LUTABLE)      FILL LUNAME TABLE WITH X'FF'S                
         LHI   R1,MAXLUS                                                        
BL10     MVI   0(R4),X'FF'                                                      
         MVC   1(LUTABLEQ-1,R4),0(R4)                                           
         LA    R4,LUTABLEQ(,R4)    BUMP TO NEXT TABLE ENTRY                     
         BCT   R1,BL10                                                          
*                                                                               
         LA    R9,=C'DSTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE DESTINATION TABLE                
*                                                                               
         L     R4,=A(LUTABLE)      BUILD TABLE OF LUNAMES                       
         USING LUTABLED,R4                                                      
         SR    R1,R1               COUNT NUMBER OF LUNAMES                      
         L     R3,ADESTTBL         A(DESTINATION TABLE)                         
         LA    R2,DESTNTRY                                                      
         USING DESTTABD,R2                                                      
*                                                                               
BL20     MVC31 DESTNTRY,0(R3)      GET AN ENTRY FROM DESTTBL                    
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    BLX                                                              
         CLI   DESTMETR,EDICOLQ    IS THIS A COLUMBINE STATION?                 
         BNE   BL30                                                             
         XC    0(LUTABLEQ,R4),0(R4)                                             
         MVC   LUEDCKEY,DESTNAME   SAVE EDICT= KEY                              
         MVC   LUNAME,DESTCOLL     SAVE LUNAME                                  
         MVC   LUUSERID,DESTCOLU   SAVE USERID                                  
         MVC   LUPASSWD,DESTCOLP   SAVE PASSWORD                                
         LA    R1,1(R1)                                                         
         CHI   R1,MAXLUS                                                        
         BNH   *+6                                                              
         DC    H'0'                INCREASE MAXLUS                              
         LA    R4,LUTABLEQ(,R4)    BUMP TO NEXT TABLE ENTRY                     
*                                                                               
BL30     AHI   R3,DESTTBLQ                                                      
         B     BL20                                                             
         DROP  R2,R4                                                            
*                                                                               
BLX      LA    R9,=C'DSTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE DESTINATION TABLE                
*                                                                               
         LA    R9,=C'LUTABLE '     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE COLUMBINE LUTABLE                
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* THIS ROUTINE BREAKS UP A PRINT QUEUE REPORT                                   
* INTO LOGICAL REPORTS, AND ADDS THEIR ENTRIES TO THE EDICT FILE.               
**********************************************************************          
XFERREP  NTR1  BASE=*,LABEL=*                                                   
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 DATAMGR,DMCB,(0,=C'RANDOM'),CFPQID,0,R,CXREC                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,R                                                             
         USING PQPLD,R2                                                         
         CLI   QLCLASS,C'G'        CLASS G?                                     
         BNE   XFERXIT                                                          
         TM    QLSTAT,X'80'        STATUS ACTIVE?                               
         BZ    XFERXIT                                                          
         TM    QLATTB,QLATJOBI     DOES REPORT CONTAIN JCL?                     
         BO    XFERXIT             YES -- IGNORE IT FOR NOW                     
         TM    QLATTB,QLATERR      IS REPORT IN ERROR?                          
         BO    XFERXIT             YES -- IGNORE IT                             
*                                                                               
         MVI   SKIPTHIS,C'N'                                                    
*                                                                               
         L     RF,=A(USERFILT)                                                  
         OC    0(L'USERFILT,RF),0(RF)    IS THERE A USERID FILTER?              
         BZ    XFER1X                    NO                                     
         LA    RE,USRFMAXQ                                                      
XFER1    OC    0(L'USERFILT,RF),0(RF)    ANY MORE USERID FILTER?                
         BNZ   *+12                                                             
         MVI   SKIPTHIS,C'Y'       NO - NOT A MATCH, FLAG TO SKIP               
         B     XFER1X                                                           
*                                                                               
         CLC   QLSRCID,0(RF)       YES - DOES THIS REPORT MATCH FILTER?         
         BE    XFER1X              YES - GOT A MATCH                            
         AHI   RF,L'USERFILT                                                    
         BCT   RE,XFER1            TRY NEXT FILTER                              
*                                                                               
XFER1X   CLC   SUBFILT,MYSPACES    IS THERE A SUB-ID FILTER?                    
         BE    XFER1Z              NO                                           
         CLC   QLSUBID,SUBFILT     YES - DOES THIS REPORT MATCH FILTER?         
         BE    XFER1Z              YES                                          
         MVI   SKIPTHIS,C'Y'       NO - NOT A MATCH, FLAG TO SKIP               
*                                                                               
XFER1Z   L     RF,=A(EXUFILT)                                                   
         OC    0(L'EXUFILT,RF),0(RF)    IS THERE A NEG USERID FILTER?           
         BZ    XFER2X                   NO                                      
*                                                                               
         LA    RE,EXUFMAXQ                                                      
XFER2    OC    0(L'EXUFILT,RF),0(RF)    ANY MORE NEG USERID FILTER?             
         BZ    XFER2X              NO - NOT A MATCH - OKAY                      
         CLC   QLSRCID,0(RF)       YES - DOES THIS REPORT MATCH FILTER?         
         BNE   XFER2C              NO: BUMP TO NEXT USERID IN LIST              
         CLI   CBSFIX,C'Y'         DO SPECIAL CBS CODE?                         
         BE    *+12                YES                                          
         MVI   SKIPTHIS,C'Y'       NO - NOT A MATCH, FLAG TO SKIP               
         B     XFER2X                                                           
*                                                                               
         CLC   QLSUBID,=C'ECR'     USERID EXCLUSION ONLY APPLIES TO ECR         
         BNE   XFER2X                                                           
         MVI   SKIPTHIS,C'Y'       NO - NOT A MATCH, FLAG TO SKIP               
         B     XFER2X                                                           
*                                                                               
XFER2C   AHI   RF,L'EXUFILT                                                     
         BCT   RE,XFER2            TRY NEXT FILTER                              
*                                                                               
XFER2X   MVC   RPTCRTIM,QLAGELT    SAVE REPORT CREATION TIME                    
         MVC   RPTCRDAT,QLDATEL    SAVE REPORT CREATION DATE - CMPRSD           
*                                                                               
         MVI   ENDPQCI,C'N'        NOT END OF C/I YET                           
         MVI   EMPTYRPT,C'Y'       ASSUME REPORT HAS NO DATA TO SEND            
         XC    RPTLOGNO,RPTLOGNO   RESET LOGICAL REPORT COUNTER                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,QLSRCID                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(5),DUB                                                      
         MVI   P+35,C','                                                        
         MVC   P+36(3),QLSUBID                                                  
         MVI   P+39,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,QLREPNO                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(5),DUB                                                      
         MVC   OPMS6+22(15),P+30                                                
         PRNT  TRANSFERRPT                                                      
*                                                                               
         XC    RPTUID,RPTUID                                                    
         XC    RPTAGY,RPTAGY                                                    
         XC    RPTAGYOP,RPTAGYOP                                                
*                                                                               
         LA    R9,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),QLSRCID                                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
*                                                                               
         LA    R9,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         CLI   DMCB+8,0            USERID NOT FOUND                             
         BNE   XFER7                                                            
*                                                                               
         L     R4,=A(IO)                                                        
         LA    R4,28(R4)           FIND PASSIVE ELEMENT (X'02')                 
*                                                                               
XFER5    DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    XFER6                                                            
*                                                                               
         CLI   0(R4),CTDSCELQ      (X'02') DESCRIPTION ELEMENT                  
         BNE   *+10                                                             
         MVC   RPTUID,CTDSC-CTDSCD(R4) RETURN USER-ID NAME                      
*                                                                               
         CLI   0(R4),CTAGYELQ      (X'06') AGY ALPHA ID ELEMENT                 
         BNE   *+10                                                             
         MVC   RPTAGY,CTAGYID-CTAGYD(R4)                                        
*                                                                               
         LLC   RF,1(R4)            ELEMENT LENGTH                               
         AR    R4,RF                                                            
         B     XFER5                                                            
*                                                                               
XFER6    DS    0H                                                               
         OC    RPTUID,RPTUID       USER ID FOUND?                               
         BZ    XFER7                                                            
         OC    RPTAGY,RPTAGY       AGY ALPHA FOUND?                             
         BZ    XFER7                                                            
*                                                                               
* READ SYSTEM ACCESS RECORD, AND SAVE AGENCY OPTIONS FLAG, IF PRESENT           
*                                                                               
         LA    R9,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,6)  ENQUEUE THE CONTROL FILE                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5REC,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,RPTAGY                                                  
         DROP  R4                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,A(IO),0               
*                                                                               
         LA    R9,=C'CTFILE'       MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(6),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),6)    DEQUEUE THE CONTROL FILE                     
*                                                                               
         CLI   DMCB+8,0            ACCESS RECORD NOT FOUND                      
         BNE   XFER7                                                            
*                                                                               
         L     R4,=A(IO)                                                        
         LA    R4,28(R4)                                                        
*                                                                               
XFER6A   CLI   0(R4),0                                                          
         BE    XFER8               AGY OPTS ELEMENT NOT FOUND                   
*                                                                               
         CLI   0(R4),CTAGDELQ      AGY GROUP DETAIL ELEM? (X'B4')               
         BNE   *+14                                                             
         MVC   RPTAGYOP,CTAGOPTS-CTAGDD(R4)                                     
         B     XFER6B                                                           
*                                                                               
         LLC   RF,1(R4)            ELEMENT LENGTH                               
         AR    R4,RF                                                            
         B     XFER6A                                                           
*                                                                               
XFER6B   DS    0H                                                               
         TM    RPTAGYOP,CTAGUAT    UAT AGENCY?                                  
         BZ    XFER8                                                            
*                                                                               
         LA    RF,P+30                                                          
         MVC   0(L'RPTUID,RF),RPTUID                                            
         LA    RF,L'RPTUID(RF)                                                  
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         MVC   0(L'QLSUBID,RF),QLSUBID                                          
         LA    RF,L'QLSUBID(RF)                                                 
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         EDIT  (B2,QLREPNO),(5,0(RF)),ALIGN=LEFT                                
         PRNT  SKIPPINGUAT,PRINT=ALWAYS                                         
*                                                                               
*                                  UNKNOWN USER ID, MARK PQ REP PRINTED         
XFER7    XC    WORK,WORK                                                        
         USING UKRECD,RF                                                        
         LA    RF,WORK             BUILD PRINT QUEUE INDEX                      
         MVC   UKSRCID,QLSRCID                                                  
         MVC   UKSUBID,QLSUBID                                                  
         MVC   UKREPNO,QLREPNO                                                  
         DROP  RF                                                               
*                                                                               
         BRAS  RE,MRKPRTND                                                      
*                                                                               
         TM    RPTAGYOP,CTAGUAT    UAT AGENCY?                                  
         BO    XFER7A              YES - DO NOT PRINT "INV UID" ERROR           
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS6,OPMS6)                       
*                                                                               
XFER7A   DS    0H                                                               
         MVC   P+30(15),OPMS6+22                                                
         PRNT  MARKEDPRINTEDOK2,PRINT=ALWAYS                                    
         B     XFERXIT                                                          
*                                                                               
XFER8    DS    0H                                                               
         MVC   RPTUIDNO,QLSRCID    SAVE USERID NUMBER                           
         MVC   RPTSUBID,QLSUBID    SAVE SUB-ID                                  
         MVC   RPTREFNO,QLREPNO    SAVE REFERENCE NUMBER                        
         MVC   RPTPQTYP,QLTYPE     SAVE REPORT TYPE                             
         DROP  R2                                                               
*                                                                               
         USING BCDSTABD,R2                                                      
XFER10   L     R2,ABCDSTAB         BROADCAST DESTINATION TABLE                  
         MVC   RPTLDSTS,=H'1'      INITIALIZE LOGICAL REP DEST SEQ              
*                                                                               
         BRAS  RE,READ1RPT         LOOK FOR A LOGICAL REPORT                    
         BE    XFER14              GOT ONE                                      
         CLI   ENDPQCI,C'Y'        ALREADY AT END OF PQ CI?                     
         BNE   XFERNEXT            NO                                           
         CLI   EMPTYRPT,C'Y'       ANY DATA TO SEND IN REPORT?                  
         BNE   XFERXIT             YES                                          
*                                                                               
         MVC   P+11(39),=C'*** ERROR *** NO DATA IN REPORT TO SEND'             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XC    WORK,WORK                                                        
         USING UKRECD,RF                                                        
         LA    RF,WORK             BUILD PRINT QUEUE INDEX                      
         MVC   UKSRCID,RPTUIDNO                                                 
         MVC   UKSUBID,RPTSUBID                                                 
         MVC   UKREPNO,RPTREFNO                                                 
         DROP  RF                                                               
*                                                                               
         BRAS  RE,MRKPRTND                                                      
*&&DO                                                                           
         LA    R9,=C'PRTQU'        *** UNTIL DMENQDEQ IS RE-ENTRANT ***         
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,5)  ENQUEUE THE PRINT QUEUE                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'INDEX'),CFPQID,WORK,R,CXREC                   
         CLI   DMCB+8,0            REPORT FOUND?                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'PRINTED'),CFPQID,WORK,R,CXREC                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R9,=C'PRTQU'        *** UNTIL DMENQDEQ IS RE-ENTRANT ***         
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),5)    DEQUEUE THE PRINT QUEUE                      
*&&                                                                             
         MVC   P+30(8),RPTUID                                                   
         MVI   P+38,C','                                                        
         MVC   P+39(3),RPTSUBID                                                 
         MVI   P+42,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,RPTREFNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+43(5),DUB                                                      
         PRNT  MARKEDPRINTEDOK3,PRINT=ALWAYS                                    
         B     XFERXIT                                                          
*                                                                               
XFER14   CLI   ISEMAIL,1                                                        
         BNE   XFER15                                                           
         MVI   DESTTYPE,EDFDSTIQ   E-MAIL DESTINATION TYPE                      
         XC    RPTLDSTS,RPTLDSTS   SET LOGICAL REP DEST SEQ = 0                 
         B     XFER59              SKIP CHECK HDR DESTINATION (1ST DST)         
*                                                                               
XFER15   MVI   DESTTYPE,0          NO DESTINATION TYPE YET                      
*                                                                               
         CLC   =C'*DDSTEST*',DESTINAT DDS TEST ONLY?                            
         BE    XFERNEXT            YES -- IGNORE THIS REPORT                    
         CLC   =C'EDICT=',DESTINAT KEY OF EDICT RECORD GIVEN?                   
         BE    XFER30              YES -- FIND RECEIVER IN TABLE                
*                                                                               
         CLI   ISEMAIL,1                                                        
         BNE   *+8                                                              
         MVI   DESTTYPE,EDFDSTIQ   E-MAIL DESTINATION TYPE                      
*                                                                               
         L     R3,ADESTTBL         DESTINATION LIST                             
         LA    R5,DESTNTRY                                                      
         USING DESTTABD,R5                                                      
*                                                                               
XFER20   MVC31 DESTNTRY,0(R3)      GET AN ENTRY FROM DESTTBL                    
         CLC   DESTNAME,RPTUID     FIND SENDER IN TABLE                         
         BNE   XFER22                                                           
         MVC   RPTMETH,DESTMETS    USE SENDER'S METHOD OF TRANSMISSION          
         MVC   RPTPM360,DESTPM360                                               
         B     XFER50                                                           
*                                                                               
XFER22   AHI   R3,DESTTBLQ         BUMP TO NEXT SLOT IN TABLE                   
         CLI   DESTNAME,X'FF'      ANY MORE ENTRIES?                            
         BNE   XFER20              YES                                          
         MVC   P+11(40),=C'*** ERROR *** NO EDICT RECORD FOR SENDER'            
         GOTO1 =V(PRINTER)                                                      
         MVC   BYTE2,0(R2)         X'FF'-LAST DEST., OTHERWISE NOT              
         MVI   BYTE,C'U'           REPORT CANNOT BE SENT, EVER                  
         MVI   RPTERROR,1          ERROR CODE                                   
         BRAS  RE,POSTEDFL         POST THIS IN EDICT FILE                      
         B     XFERNEXT                                                         
*                                                                               
XFER30   MVI   DESTTYPE,EDFDSTEQ   EDICT= DESTINATION TYPE                      
         L     R3,ADESTTBL         DESTINATION LIST                             
         LA    R5,DESTNTRY                                                      
*                                                                               
XFER40   MVC31 DESTNTRY,0(R3)      GET AN ENTRY FROM DESTTBL                    
         CLC   DESTNAME,DESTINAT+6 FIND DESTINATION IN TABLE                    
         BNE   XFER42                                                           
*        MVC   RPTPM360,DESTPM360                                               
         MVC   RPTMETH,DESTMETR    SAVE METHOD OF TRANSMISSION                  
         CLI   ISPDF,1                                                          
         BNE   XFER50                                                           
         MVC   RPTMETH,DESTMETS    SAVE METHOD OF TRANSMISSION                  
         B     XFER50                                                           
*                                                                               
XFER42   AHI   R3,DESTTBLQ         BUMP TO NEXT SLOT IN TABLE                   
         CLI   DESTNAME,X'FF'      ANY MORE ENTRIES?                            
         BNE   XFER40              YES                                          
*                                                                               
         MVC   P+11(42),=C'*** ERROR *** NO EDICT RECORD FOR RECEIVER'          
         GOTO1 =V(PRINTER)                                                      
         MVC   BYTE2,0(R2)         X'FF'-LAST DEST., OTHERWISE NOT              
         MVI   BYTE,C'U'           REPORT CANNOT BE SENT, EVER                  
         MVI   RPTERROR,2          ERROR CODE                                   
*                                                                               
         CLC   =C'*BDE',DESTINAT+6                                              
         BE    XFER49                                                           
         CLC   =C'*BIAS',DESTINAT+6                                             
         BE    XFER49                                                           
         CLC   =C'*DDSDARA',DESTINAT+6                                          
         BE    XFER49                                                           
         CLC   =C'*DDSDARR',DESTINAT+6                                          
         BE    XFER49                                                           
         CLC   =C'*ENCODA',DESTINAT+6                                           
         BE    XFER49                                                           
         CLC   =C'*TVSCAN',DESTINAT+6                                           
         BNE   *+6                                                              
XFER49   DC    H'0'                SPECIAL *EDICT REC CAN'T BE MISSING          
*                                                                               
         BRAS  RE,POSTEDFL         POST THIS IN EDICT FILE                      
         B     XFERNEXT                                                         
*                                                                               
XFER50   CLI   RPTMETH,EDIEASYQ    EASYLINK?                                    
         BNE   XFER53                                                           
         CLI   MAJORNAM+5,C'R'     REP?                                         
         BE    XFER51                                                           
         MVC   DUB(6),DESTADVN+2   USE ADV MAILBOX NO. (WITHOUT '62')           
         B     *+10                                                             
XFER51   MVC   DUB(6),DESTREPN+2   USE REP MAILBOX NO. (WITHOUT '62')           
*                                                                               
         PACK  FULL,DUB(6)                                                      
         L     R0,FULL                                                          
         SRL   R0,4                SHIFT OUT SIGN                               
         STCM  R0,7,EZMAILBX       SAVE MAILBOX NUMBER                          
         DROP  R5                                                               
*                                                                               
         CLC   MQAPP,SPACES        ANY MQ QUEUE INFO GIVEN?                     
         BNH   XFER55              NO                                           
*                                                                               
         L     RE,=A(FACIDTAB)                                                  
XFER52   CLC   MQAPP(4),0(RE)                                                   
         BNE   *+14                                                             
         MVC   MQAPP(4),4(RE)      USE THE 4 CHAR FACPAK ID                     
         B     XFER55                                                           
*                                                                               
         AHI   RE,L'FACIDTAB                                                    
         CLI   0(RE),X'FF'                                                      
         BNE   XFER52                                                           
*                                                                               
         MVC   P+11(33),=C'*** ERROR *** UNKNOWN FACPAK NAME'                   
         GOTO1 =V(PRINTER)                                                      
         MVC   BYTE2,0(R2)         X'FF'-LAST DEST., OTHERWISE NOT              
         MVI   BYTE,C'U'           REPORT CANNOT BE SENT, EVER                  
         MVI   RPTERROR,EDFERIMQ   ERROR CODE                                   
         BRAS  RE,POSTEDFL         POST THIS IN EDICT FILE                      
         B     XFERNEXT                                                         
*                                                                               
XFER53   CLI   RPTMETH,EDINONEQ    METH TO SEND = NONE?                         
         BNE   XFER55                                                           
*                                                                               
*        CLI   RPTPM360,C'Y'                                                    
*        BNE   XFER54                                                           
*        PRNT  REPORTSKIPPED,PRINT=ALWAYS                                       
*        B     XFERNEXT                                                         
*                                                                               
*FER54   DS    0H                                                               
         MVC   P+11(36),=C'*** ERROR *** NO TRANSMISSION METHOD'                
         GOTO1 =V(PRINTER)                                                      
         MVC   BYTE2,0(R2)         X'FF'-LAST DEST., OTHERWISE NOT              
         MVI   BYTE,C'U'           REPORT CANNOT BE SENT, EVER                  
         MVI   RPTERROR,EDFERNMT   ERROR CODE                                   
         BRAS  RE,POSTEDFL         POST THIS IN EDICT FILE                      
         B     XFERNEXT                                                         
*                                                                               
XFER55   MVC   BYTE2,0(R2)         X'FF'-LAST DEST., OTHERWISE NOT              
         MVI   BYTE,C'F'           CREATE REPORT ENTRY IN EDICT FILE            
         BRAS  RE,POSTEDFL                                                      
*                                                                               
XFER59   CLI   0(R2),X'FF'         NOMORE OTHER DEST. TO BE SENT?               
         BE    XFERNEXT                                                         
         MVC   DESTINAT,BCDSTNAT   DESTINATION                                  
         MVC   DESTFMT,BCDSTFMT    DESTINATION FORMAT                           
         LH    RE,RPTLDSTS         INCREMENT THE REP DEST. SEQ                  
         LA    RE,1(RE)                                                         
         STH   RE,RPTLDSTS                                                      
         LA    R2,BCDSTABQ(R2)     BUMP TO THE NEXT BROADCAST DST ENTRY         
         B     XFER15              NEXT DEST                                    
         DROP  R2                                                               
*                                                                               
XFERNEXT CLI   ENDPQCI,C'Y'        ALREADY AT END OF PQ CI?                     
         BNE   XFER10              NO                                           
*                                                                               
XFERXIT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MARK PRINTQ REPORT AS PRINTED. PASS PQ KEY IN WORK                            
***********************************************************************         
MRKPRTND NTR1  BASE=*,LABEL=*                                                   
         LA    R9,=C'PRTQU'        *** UNTIL DMENQDEQ IS RE-ENTRANT ***         
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,5)  ENQUEUE THE PRINT QUEUE                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'INDEX'),CFPQID,WORK,R,CXREC                   
         CLI   DMCB+8,0            REPORT FOUND?                                
         JE    MRKPRT10                                                         
         TM    DMCB+8,X'80'        JUST SKIP IT NOW. PROBABLY MARKED            
         J     MRKPRT90                                                         
*                                                                               
MRKPRT10 GOTO1 DATAMGR,DMCB,(0,=C'PRINTED'),CFPQID,WORK,R,CXREC                 
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
MRKPRT90 LA    R9,=C'PRTQU'        *** UNTIL DMENQDEQ IS RE-ENTRANT ***         
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(5),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),5)    DEQUEUE THE PRINT QUEUE                      
         XIT1                                                                   
         LTORG                                                                  
                                                                                
**********************************************************************          
* READ ONE LOGICAL REPORT FROM A PRINT QUEUE REPORT,                            
* CC SET EQUAL IF A LOGICAL REPORT WAS FOUND, OTHERWISE NOT EQUAL.              
**********************************************************************          
READ1RPT NTR1  BASE=*,LABEL=*                                                   
         XC    XACTDATA,XACTDATA   CLEAR OUT REPORT-SPECIFIC FIELDS             
         XC    FILENAME,FILENAME   CLEAR OUT BDE-FTP FILE NAME                  
         MVI   ISEMAIL,0                                                        
         MVI   ISBDE,0             ASSUME NOT BDE-EMAIL                         
         MVI   ISPDF,0                                                          
         MVI   RPTERROR,0                                                       
         MVI   RPTMETH,0                                                        
         MVI   RPTPM360,0                                                       
         MVI   RPTFLAGS,0          RESET ALL FLAGS                              
         XC    EZMAILBX,EZMAILBX                                                
         XC    MQAPP,MQAPP                                                      
         XC    MQID,MQID                                                        
         XC    PQSCL,PQSCL                                                      
         XC    PQSST,PQSST                                                      
*                                                                               
         MVI   BYTE2,0             NO RECORDS FOUND YET                         
RDRPT10  LH    R1,RPTLOGNO         INCREMENT LOGICAL REPORT SEQ. NUMBER         
         LA    R1,1(R1)                                                         
         STH   R1,RPTLOGNO                                                      
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING XMTTABLD,R2                                                      
         MVI   XMTSOURC,XMTSRCPQ   PRINT QUEUE REPORT                           
         MVC   XMTPRTQ,CFPQINUM    PQ NUMBER                                    
         MVC   XMTUSRID,RPTUIDNO   PQ REPORT SENDING USERID                     
         MVC   XMTSUBID,RPTSUBID   PQ REPORT SUB-ID                             
         MVC   XMTREFNO,RPTREFNO   PQ REPORT REFERENCE NUMBER                   
         MVC   XMTCRDAT,RPTCRDAT   PQ RPT CREATION DATE - CMPRSD                
         MVC   XMTCRTIM,RPTCRTIM   PQ RPT CREATION TIME                         
         MVC   XMTLOGNO,RPTLOGNO   PQ RPT LOGICAL REPORT NUMBER                 
         MVC   XMTDSTNO,RPTLDSTS   PQ RPT LOGICAL REPORT DEST. NUMBER           
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         GOTO1 =V(BINSRCH),DMCB,WORK,AXMTTBL,,XMTTBLQ,XMTKEYQ,0                 
*                                                                               
         ICM   RE,15,DMCB          A(RECORD IN TABLE)                           
         TMH   RE,X'8000'          WAS RECORD FOUND?                            
         BO    *+10                NO                                           
         MVC   WORK(XMTTBLQ),0(RE) SAVE STATUS BYTE                             
         SAM24                                                                  
*                                                                               
         TM    DMCB,X'80'          WAS THIS REPORT ALREADY SEEN?                
         BO    RDRPT30             NO -- TRY TO SEND IT                         
         TM    XMTSTAT,EDFSTSNT+EDFSTRCV+EDFSTCAN+EDFSTJNK+EDFSTPRT+EDF+        
               STWTG+EDFSTLST                                                   
         BZ    RDRPT30             NO, TRY TO SEND IT                           
         DROP  R2                                                               
*                                                                               
         MVI   EMPTYRPT,C'N'       REPORT HAS DATA IN IT TO SEND                
RDRPT20  GOTO1 DATAMGR,DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                       
         CLI   DMCB+8,0            READ A CARD                                  
         BNE   RDRPT80             END OF PQ CONTROL INTERVAL                   
         CLC   =C'*** END OF DDS MESSAGE ***',R+1                               
         BE    RDRPT10             END OF REPORT -- TRY THE NEXT ONE            
         B     RDRPT20                                                          
*                                                                               
RDRPT30  XC    R,R                                                              
DS       USING EDIDESTD,R+1                                                     
         GOTO1 DATAMGR,DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                       
         CLI   DMCB+8,0            READ A CARD                                  
         BNE   RDRPT80             END OF PQ CONTROL INTERVAL                   
         CLC   =C'*HDR*',DS.EDIHDR IS THIS A HEADER RECORD?                     
         BNE   RDRPT30             NO -- FIND BEGINNING OF LOGICAL RPT          
*                                                                               
         CLI   DS.EDITTYPE,EDIEMALQ IS IT A E-MAIL TRANSMISSION?                
         BNE   RDRPT32             NO - CONTINUE                                
         MVI   ISEMAIL,1           YES - IGNORE HDR DST & FORMATTED DST         
         B     RDRPT35                                                          
*                                                                               
RDRPT32  CLI   DS.EDITTYPE,EDITPDFQ IS IT A PDF TRANSMISSION?                   
*        BE    *+10                NO - CONTINUE                                
*        CLC   =C'EDICT=*OPTICA,DS.EDIEDICT  A PDF TRANSACTION?                 
         BNE   RDRPT33             NO - CONTINUE                                
         MVI   ISPDF,1             YES - IGNORE HDR DST & FORMATTED DST         
*                                                                               
RDRPT33  CLC   =C'EDICT=*BDE',DS.EDIEDICT  A BDE TRANSACTION?                   
         BNE   *+8                 NO                                           
         MVI   ISBDE,1             YES - IGNORE HDR DST & FORMATTED DST         
*                                                                               
         MVC   DESTFMT,DS.EDIFDEST  SAVE FORMATTED DESTINATION (IF ANY)         
         MVC   DESTINAT,DS.EDIDESID DESTINATION (UP TO 25 CHARACTERS)           
         CLI   DS.EDIDARE,EDIDARQ   IS IT A DARE REPORT (FOR EASYLINK)?         
         BNE   *+8                                                              
         OI    RPTFLAGS,EDFDAREZ   YES                                          
         DROP  DS                  EDIDESTD                                     
*                                                                               
RDRPT35  L     R3,ABCDSTAB                                                      
         USING BCDSTABD,R3                                                      
         MVI   0(R3),X'FF'         ASSUME THERE IS NO ADDITIONAL DEST           
*                                                                               
RDRPT40  XC    R,R                                                              
         GOTO1 DATAMGR,DMCB,(X'01',=C'READ'),CFPQID,0,R,CXREC,0                 
         CLI   DMCB+8,0            READ A CARD                                  
         BNE   RDRPT80             END OF PQ CONTROL INTERVAL                   
         CLC   =C'*** END OF DDS MESSAGE ***',R+1                               
         BE    RDRPT90                                                          
*                                                                               
         CLC   =C'++DDS',R+1       DDS CONTROL CARD?                            
         BE    RDRPT50             YES -- LEAVE IT ALONE                        
         L     RF,=A(TROUTBND)                                                  
         TR    R+1(L'R-1),0(RF)    TRANSLATE NON-PRINTABLE INTO SPACES          
         CLC   R+1(L'R-1),MYSPACES                                              
         BE    RDRPT40                                                          
*                                                                               
         MVI   BYTE2,X'FF'         WE HAVE A REPORT                             
         MVI   EMPTYRPT,C'N'       REPORT HAS DATA IN IT TO SEND                
         B     RDRPT40                                                          
*                                                                               
RDRPT50  CLC   =C'TRN',R+12        IS THIS THE TRANSACTION DATA CARD?           
         BNE   RDRPT51             NO                                           
         MVC   XACTDATA(1),R+7     SYSTEM                                       
         MVC   XACTDATA+1(3),R+9   RECORD TYPE                                  
         MVC   XACTDATA+4(58),R+16 APPLICATION DATA                             
         B     RDRPT40                                                          
*                                                                               
RDRPT51  CLC   =C'DST',R+12        IS THIS THE BROADCAST DST CARD?              
         BE    RDRPT54             NO                                           
*                                                                               
         CLC   =C'FIL',R+12        IS THIS FILE NAME CARD?                      
         BNE   *+14                                                             
         MVC   FILENAME,R+16       SAVE FILE NAME                               
         B     RDRPT40                                                          
*                                                                               
         CLC   =C'UID',R+12        IS THIS UID NUMBER CARD?                     
         BNE   *+14                                                             
         MVC   UIDNO,R+16       SAVE FILE NAME                                  
         B     RDRPT40                                                          
*                                                                               
         CLI   ISBDE,1                                                          
         BNE   RDRPT52                                                          
         CLC   =C'RCP',R+12        IS THIS THE RECIPIENT CARD?                  
         BNE   RDRPT58             NO                                           
         MVC   DESTFMT,R+16        SAVE FORMATTED DESTINATION (IF ANY)          
         B     RDRPT40                                                          
*                                                                               
RDRPT52  CLI   ISEMAIL,1                                                        
         BNE   RDRPT58                                                          
         CLC   =C'RCP',R+12        IS THIS THE RECIPIENT E-MAIL CARD?           
         BE    RDRPT53             NO                                           
         CLC   =C'CCR',R+12        IS THIS THE CC E-MAIL CARD?                  
         BE    RDRPT53             NO                                           
         CLC   =C'BCC',R+12        IS THIS THE BCC E-MAIL CARD?                 
         BNE   RDRPT58             NO                                           
*                                                                               
RDRPT53  C     R3,EBCDSTAB         EXCESS THE SIZE OF BROADCAST TABLE?          
         BL    *+6                                                              
         DC    H'0'                INCREASE THE MAX # DESTS                     
*                                                                               
         XC    BCDSTNAT,BCDSTNAT   CLEAR THE DEST FIELD                         
         MVC   BCDSTFMT,R+16       SAVE TRUNCATED(16 CHAR) E-MAIL ADR           
         B     RDRPT57             NEXT DEST                                    
*                                                                               
RDRPT54  C     R3,EBCDSTAB         EXCESS THE SIZE OF BROADCAST TABLE?          
         BL    *+6                                                              
         DC    H'0'                INCREASE MAXDESTS                            
*                                                                               
         MVC   BCDSTNAT,R+16       DESTINATION (UP TO 25 CHARACTERS)            
         MVC   BCDSTFMT,R+42       SAVE FORMATTED DESTINATION (IF ANY)          
RDRPT57  LA    R3,BCDSTABQ(R3)     BUMP THE BROADCAST DST TABLE                 
         MVI   0(R3),X'FF'         ASSUME THE END OF BROADCAST FAXTAB           
         B     RDRPT40                                                          
         DROP  R3                                                               
*                                                                               
RDRPT58  CLC   =C'DSN',R+12        DOES DATA RESIDE IN AN MVS DATASET?          
         BNE   RDRPT59             NO                                           
         MVI   BYTE2,X'FF'         YES - WE HAVE A REPORT                       
         MVI   EMPTYRPT,C'N'       ASSUME DATASET ISN'T EMPTY                   
         B     RDRPT40                                                          
*                                                                               
RDRPT59  CLC   =C'MQN',R+12        DOES DATA RESIDE IN AN MVS DATASET?          
         BNE   RDRPT60             NO                                           
         MVC   MQAPP,R+16          MQDEF APPL NAME(FACPAK NAME, CL8)            
         MVC   MQLAB,R+25          MQ MESSAGE LABEL FOR FACPAK, CL6             
*                                                                               
         CLI   R+32,C' '           ANY GIVEN MQ QUEUE #?                        
         BNE   *+14                YES                                          
         MVC   MQID,=XL2'0001'     NO - SET TO 1                                
         B     RDRPT40                                                          
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,R+32,(2,0)                                       
         CLI   DMCB,0                                                           
         BNE   RDRPT40             INVALID, DON'T SAVE MQ QUEUE #               
         L     R1,DMCB+4                                                        
         STCM  R1,3,MQID                                                        
         B     RDRPT40                                                          
*                                                                               
RDRPT60  CLC   =C'PQS',R+12        ANY CHANGE TO PQ CLASS/STATUS?               
         BNE   RDRPT40             NO                                           
         MVC   PQSCL,R+16          PQ CLASS CL1                                 
*                                  PQ STATUS XL1                                
         MVI   PQSST,X'80'         PQ STATUS = ACTIVE AS DEFAULT                
         CLI   R+17,C' '                                                        
         BE    RDRPT40             NO STATUS GIVEN, ASSUME ACTIVE               
         GOTO1 =V(HEXIN),DMCB,R+17,PQSST,2                                      
         B     RDRPT40                                                          
*                                                                               
RDRPT80  MVI   ENDPQCI,C'Y'        SET END OF PQ CI (EODAD)                     
*                                                                               
RDRPT90  CLI   BYTE2,X'FF'         ANY REPORT FOUND?                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* NEW DAY                                                                       
**********************************************************************          
NEWDAY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,=A(NEWDYECB)     A(NEW DAY ECB)                               
         TM    0(RF),X'40'         DID A NEW DAY START?                         
         BZ    NDX                 NO                                           
*                                                                               
         PRNT  NEWDAY_POSTED,PRINT=ALWAYS                                       
*                                                                               
         L     R2,=V(PRNTER)       A(SYSPRINT DCB)                              
         SETPRT (2),DISP=SCHEDULE  SPIN OFF A NEW SYSPRINT                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(3,TODAY3)                                 
         GOTO1 =V(DATCON),DMCB,(5,0),TODAY6                                     
*                                                                               
         LA    R9,=C'EDICTADD'                                                  
         ENQ   (MAJORNAM,(9),E,8)                                               
*                                                                               
         L     RF,=A(PREVEEOF)     A(PREVIOUS EDICT EOF)                        
         XC    0(4,RF),0(RF)       CLEAR IT                                     
*                                                                               
         ZIC   R2,TODAY3+2         DAY NUMBER                                   
         BCTR  R2,0                                                             
         MH    R2,EDCTFTPD                                                      
*                                                                               
         LH    R1,EDCTFTPD                                                      
         AR    R1,R2               LAST TRACK NUMBER FOR TODAY                  
         STCM  R1,3,EDCTFLST                                                    
         L     RF,=A(EDICTLST)                                                  
         STCM  R1,3,0(RF)                                                       
*                                                                               
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER FOR TODAY         
*                                                                               
         STCM  R2,3,FULL           TRACK NUMBER                                 
         MVI   FULL+2,1            BLOCK #1                                     
         MVI   FULL+3,0                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,FULL,0                                                   
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,1                1ST LOGICAL REC                              
         L     R4,=A(EDICTBLK)                                                  
         USING EDFILD,R4                                                        
*                                  SKIP PERMANENT RECORD, EX {00010101}         
ND30     STC   R5,FULL+3                                                        
         CLI   EDFMON,EDFMONPQ     IS THIS A 'PERMANENT' RECORD?                
         BNE   ND50                                                             
*                                                                               
         AH    R4,EDCTFRCL         BUMP TO NEXT RECORD                          
         LA    R5,1(R5)                                                         
         CH    R5,EDCTFRPB         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   ND30                                                             
         DC    H'0'                MUST FIND A EMPTY REC IN 1ST BLOCK!          
         DROP  R4                                                               
*                                                                               
ND50     L     RF,=A(EDICTEOF)                                                  
         MVC   0(4,RF),FULL                                                     
*                                                                               
         DEQ   (MAJORNAM,(9),8)                                                 
*                                                                               
         BRAS  RE,DAYTIMER         RESET THE TIMER FOR NEXT DAY                 
*                                                                               
*REATTACH NJE SUBTASK FOR SOME DATE/TIME VALUE RESET                            
         CLI   XMITNJE,C'Y'        COULD NJE SUBTASK BE RUNNING?                
         BNE   NDX                 NO                                           
*                                                                               
         MVI   BYTE,C'J'                                                        
         BRAS  RE,DETACH           DETACH DARE SUBTASK                          
*                                                                               
         MVC   FULL,=F'3000'       THIRTY SECONDS                               
*                                                                               
         STIMERM SET,ID=STIMER2,BINTVL=FULL,WAIT=YES                            
         LTR   RF,RF                                                            
         BZ    *+6                 WAIT 30 SECONDS                              
         DC    H'0'                                                             
*                                                                               
         MVI   BYTE,C'J'                                                        
         BRAS  RE,ATTACH           REATTACH DARE SUBTASK                        
         MVC   OPMS1(3),=C'NJE'                                                 
         MVC   OPMS1+18(8),=C'RUNNING '                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
NDX      XIT1                                                                   
         LTORG                                                                  
*                                                                               
DAYTIMER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         PRNT  SET_DAYTIMER,PRINT=ALWAYS                                        
         XC    NEWDYECB,NEWDYECB                                                
         TIME  BIN                  R0 = DDS TIME IN 0.01 SEC                   
         L     R1,TIME24HR                                                      
         SR    R1,R0                24HR - TIME NOW                             
         ST    R1,TIMENWDY                                                      
*                                                                               
         STIMERM SET,ID=STIMERND,BINTVL=TIMENWDY,EXIT=DAYXIT                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
DAYXIT   SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING DAYXIT,RB                                                        
         POST  NEWDYECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
NEWDYECB DS    F                                                                
TIMENWDY DS    F                                                                
TIME24HR DC    A(24*60*60*100)     24HR                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SEE IF THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' (P) OR               
* 'MODIFY' (F) COMMAND.  EXAMINE THE COMMAND AND TAKE CORRECT ACTION.           
**********************************************************************          
CHKOPER  NTR1  BASE=*,LABEL=*                                                   
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    CHKOPX              NO                                           
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CHKOPMOD                                                         
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'STOP COMMAND ACCEPTED'             
         B     CHKOPOK                                                          
*                                                                               
CHKOPMOD CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT IS GOING ON?                            
*                                                                               
         CLC   CIBDATLN,=H'6'      LENGTH OF INPUT STRING                       
         BNE   CHKMORE                                                          
*                                                                               
         CLC   =C'STATUS',CIBDATA  SHOW PROGRAM STATUS ON CONSOLE               
         BNE   CHKBAD                                                           
         BRAS  RE,STATUS                                                        
         B     CHKOPOK                                                          
*                                                                               
CHKMORE  CLC   CIBDATLN,=H'3'      LENGTH OF INPUT STRING                       
         BNE   CHKCOLST                                                         
*                                                                               
         CLC   =C'ATT',CIBDATA     TOGGLE EASYLINK TRANSMISSIONS?               
         BNE   CHKNFX                                                           
         MVI   BYTE,C'E'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKNFX   CLC   =C'NFX',CIBDATA     TOGGLE NFX TRANSMISSIONS?                    
         BNE   CHKENC                                                           
         MVI   BYTE,C'Q'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKENC   CLC   =C'ENC',CIBDATA     TOGGLE ENCODA TRANSMISSIONS?                 
         BNE   CHKBDE                                                           
         MVI   BYTE,C'Z'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKBDE   CLC   =C'BDE',CIBDATA     TOGGLE BDE-EMAIL TRANSMISSIONS?              
         BNE   CHKBDF                                                           
         MVI   BYTE,C'T'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKBDF   CLC   =C'BDF',CIBDATA     TOGGLE BDE-FTP TRANSMISSIONS?                
         BNE   CHKMQM                                                           
         MVI   BYTE,C'P'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKMQM   CLC   =C'MQM',CIBDATA     TOGGLE E-MAIL TRANSMISSIONS?                 
         BNE   CHKNJE                                                           
         MVI   BYTE,C'M'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKNJE   CLC   =C'NJE',CIBDATA     TOGGLE NJE TRANSMISSIONS?                    
         BNE   CHKDARE                                                          
         MVI   BYTE,C'J'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKDARE  CLC   =C'DAR',CIBDATA     TOGGLE DARE TRANSMISSIONS?                   
         BNE   CHKFTP                                                           
         MVI   BYTE,C'D'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKFTP   CLC   =C'FTP',CIBDATA     TOGGLE FTP TRANSMISSIONS?                    
         BNE   CHKCOL                                                           
         MVI   BYTE,C'F'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKCOL   CLC   =C'COL',CIBDATA     TOGGLE COLUMBINE TRANSMISSIONS?              
         BNE   CHKFAXG                                                          
         MVI   BYTE,C'C'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKFAXG  CLC   =C'FAX',CIBDATA     TOGGLE FAXGATE TRANSMISSIONS?                
         BNE   CHKPDF                                                           
         MVI   BYTE,C'X'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKPDF   CLC   =C'PDF',CIBDATA     TOGGLE PDF  TRANSMISSIONS?                   
         BNE   CHKBIAS                                                          
         MVI   BYTE,C'O'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKBIAS  CLC   =C'BIA',CIBDATA     TOGGLE BIAS TRANSMISSIONS?                   
         BNE   CHKADVN                                                          
         MVI   BYTE,C'B'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKADVN  CLC   =C'ADN',CIBDATA     TOGGLE ADVANTIS TRANSMISSIONS?               
         BNE   CHKBAD                                                           
         MVI   BYTE,C'A'                                                        
         B     CHKTOGGL                                                         
*                                                                               
CHKCOLST CLC   =C'COL,',CIBDATA    SPECIAL COLUMBINE COMMAND?                   
         BNE   CHKFXGST                                                         
         MVC   WORK,CIBDATA        SAVE THE OPERATOR'S COMMAND STRING           
         BRAS  RE,COLSTAT                                                       
         B     CHKOPOK                                                          
*                                                                               
CHKFXGST CLC   =C'FAX,',CIBDATA    SPECIAL FAXGATE COMMAND?                     
         BNE   CHKBAD                                                           
         MVC   WORK,CIBDATA        SAVE THE OPERATOR'S COMMAND STRING           
         BRAS  RE,FAXGSTAT                                                      
         B     CHKOPOK                                                          
*                                                                               
CHKBAD   GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'*INVALID EDICT COMMAND*'           
         B     CHKOPOK                                                          
*                                                                               
CHKTOGGL BRAS  RE,TOGGLE                                                        
*                                                                               
CHKOPOK  L     RF,ACOMM                                                         
         LA    R3,COMCIBPT-COMLIST(RF) A(A(CIB))                                
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  R2                                                               
*                                                                               
CHKOPX   XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SHOW THE PROGRAM STATUS ON THE CONSOLE.                                       
**********************************************************************          
STATUS   NTR1  BASE=*,LABEL=*                                                   
         USING XMTTABLD,R9                                                      
         LA    R9,XMTENTRY                                                      
*                                                                               
         CLI   XMITEZ,C'N'                                                      
         BE    STAT18                                                           
         MVC   OPMS2(3),=C'ATT'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'     DEFAULT VALUE                       
         TM    EASYECB,X'40'                                                    
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT15                                                           
STAT10   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'E'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT10                                                        
STAT15   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT18   CLI   XMITNJE,C'N'                                                     
         BE    STAT28                                                           
         MVC   OPMS2(3),=C'NJE'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    NJEECB,X'40'                                                     
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT25                                                           
STAT20   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'J'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT20                                                        
STAT25   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT28   CLI   XMITFTP,C'N'                                                     
         BE    STAT38                                                           
         MVC   OPMS2(3),=C'FTP'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    FTPECB,X'40'                                                     
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT35                                                           
STAT30   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'F'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT30                                                        
STAT35   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT38   CLI   XMITNFX,C'N'                                                     
         BE    STAT48                                                           
         MVC   OPMS2(3),=C'NFX'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    NFXECB,X'40'                                                     
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT45                                                           
STAT40   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'Q'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT40                                                        
STAT45   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT48   CLI   XMITCOL,C'N'                                                     
         BE    STAT58                                                           
         MVC   OPMS2(3),=C'COL'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    CFTPECB,X'40'                                                    
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT55                                                           
STAT50   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'C'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT50                                                        
STAT55   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT58   CLI   XMITDARE,C'N'                                                    
         BE    STAT68                                                           
         MVC   OPMS2(3),=C'DAR'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    DAREECB,X'40'                                                    
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT65                                                           
STAT60   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'D'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT60                                                        
STAT65   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT68   CLI   XMITADVN,C'N'                                                    
         BE    STAT78                                                           
         MVC   OPMS2(3),=C'ADN'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    ADVNECB,X'40'                                                    
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT75                                                           
STAT70   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'A'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT70                                                        
STAT75   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT78   CLI   XMITFAXG,C'N'                                                    
         BE    STAT86                                                           
         MVC   OPMS2(3),=C'FAX'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    FAXGECB,X'40'                                                    
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT82                                                           
STAT80   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'X'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT80                                                        
STAT82   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT86   CLI   XMITBIAS,C'N'                                                    
         BE    STAT90                                                           
         MVC   OPMS2(3),=C'BIA'                                                 
         MVC   OPMS2+18(8),=C'RUNNING '                                         
         TM    BIASECB,X'40'                                                    
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'DISABLED'                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT88                                                           
STAT87   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'B'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT87                                                        
STAT88   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT90   CLI   XMITPDF,C'N'                                                     
         BE    STAT100                                                          
         MVC   OPMS2(3),=C'PDF'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    BIASECB,X'40'                                                    
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT94                                                           
STAT92   MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'O'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT92                                                        
STAT94   EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT100  CLI   XMITMQM,C'N'                                                     
         BE    STAT110                                                          
         MVC   OPMS2(3),=C'MQM'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    MQMECB,X'40'                                                     
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT105                                                          
STAT103  MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'M'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT103                                                       
STAT105  EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT110  CLI   XMITBDE,C'N'                                                     
         BE    STAT120                                                          
         MVC   OPMS2(3),=C'BDE'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    BDEECB,X'40'                                                     
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT115                                                          
STAT113  MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'T'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT113                                                       
STAT115  EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT120  CLI   XMITBDF,C'N'                                                     
         BE    STAT130                                                          
         MVC   OPMS2(3),=C'BDF'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    BDFECB,X'40'                                                     
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT125                                                          
STAT123  MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'P'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT123                                                       
STAT125  EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STAT130  CLI   XMITENC,C'N'                                                     
         BE    STATUSX                                                          
         MVC   OPMS2(3),=C'ENC'                                                 
         MVC   OPMS2+18(8),=C'DISABLED'                                         
         TM    ENCECB,X'40'                                                     
         BO    *+10                                                             
         MVC   OPMS2+18(8),=C'RUNNING '                                         
*                                                                               
         SR    R3,R3                                                            
         L     R4,AXMTTBL          A(TABLE)                                     
         ICM   R2,15,XMTTBNUM      R2 = NUMBER OF ENTRIES IN TABLE              
         BZ    STAT135                                                          
STAT133  MVC31 XMTENTRY,0(R4)                                                   
         CLI   XMTMETH,C'Z'                                                     
         BNE   *+16                                                             
         TM    XMTSTAT,EDFSTWTG                                                 
         BZ    *+8                                                              
         LA    R3,1(R3)                                                         
         AHI   R4,XMTTBLQ                                                       
         BCT   R2,STAT133                                                       
STAT135  EDIT  (R3),(6,OPMS2+28),ALIGN=LEFT,ZERO=NOBLANK                        
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
STATUSX  XIT1                                                                   
         DROP  R9                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TURN A METHOD OF TRANSMISSION ON OR OFF, VIA ATTACH / DETACH.                 
* THE FIELD 'BYTE' IDENTIFIES THE METHOD TO TOGGLE.                             
***********************************************************************         
TOGGLE   NTR1  BASE=*,LABEL=*                                                   
         CLI   BYTE,C'E'           TOGGLE EASYLINK TRANSMISSIONS?               
         BNE   TOGGLE10                                                         
         MVC   OPMS1(3),=C'ATT'                                                 
         CLI   XMITEZ,C'Y'         YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE5                                                          
         TM    EASYECB,X'40'       YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
                                                                                
TOGGLE5  MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH EASYLINK SUBTASK                      
         TM    ELOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ELOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE10 CLI   BYTE,C'Q'           TOGGLE NFX TRANSMISSIONS?                    
         BNE   TOGGLE20                                                         
         MVC   OPMS1(3),=C'NFX'                                                 
         CLI   XMITNFX,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE15                                                         
         TM    NFXECB,X'40'        YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE15 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH NFX SUBTASK                           
         TM    QLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  QLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE20 CLI   BYTE,C'J'           TOGGLE NJE TRANSMISSIONS?                    
         BNE   TOGGLE30                                                         
         MVC   OPMS1(3),=C'NJE'                                                 
         CLI   XMITNJE,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE25                                                         
         TM    NJEECB,X'40'        YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE25 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH NJE SUBTASK                           
         TM    JLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  JLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE30 CLI   BYTE,C'F'           TOGGLE FTP TRANSMISSIONS?                    
         BNE   TOGGLE40                                                         
         MVC   OPMS1(3),=C'FTP'                                                 
         CLI   XMITFTP,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE35                                                         
         TM    FTPECB,X'40'        YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE35 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH FTP SUBTASK                           
         TM    FLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  FLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE40 CLI   BYTE,C'D'           TOGGLE DARE TRANSMISSIONS?                   
         BNE   TOGGLE50                                                         
         MVC   OPMS1(3),=C'DAR'                                                 
         CLI   XMITDARE,C'Y'       YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE45                                                         
         TM    DAREECB,X'40'       YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE45 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH DARE SUBTASK                          
         TM    DLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  DLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE50 CLI   BYTE,C'C'           TOGGLE COLUMBINE TRANSMISSIONS?              
         BNE   TOGGLE60                                                         
         MVC   OPMS1(3),=C'COL'                                                 
         CLI   XMITCOL,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE55                                                         
         TM    CFTPECB,X'40'       YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE55 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,BLDLUTAB         BUILD TABLE OF COLUMBINE LUNAMES             
         BRAS  RE,ATTACH           ATTACH COLUMBINE SUBTASK                     
         TM    CLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  CLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE60 CLI   BYTE,C'A'           TOGGLE ADVANTIS TRANSMISSIONS?               
         BNE   TOGGLE70                                                         
         MVC   OPMS1(3),=C'ADN'                                                 
         CLI   XMITADVN,C'Y'       YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE65                                                         
         TM    ADVNECB,X'40'       YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE65 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH ADVANTIS SUBTASK                      
         TM    ALOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ALOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE70 CLI   BYTE,C'X'           TOGGLE FAXGATE TRANSMISSIONS?                
         BNE   TOGGLE80                                                         
         MVC   OPMS1(3),=C'FAX'                                                 
         CLI   XMITFAXG,C'Y'       YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE75                                                         
         TM    FAXGECB,X'40'       YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE75 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH FAXGATE SUBTASK                       
         TM    XLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  XLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE80 CLI   BYTE,C'O'           TOGGLE PDF TRANSMISSIONS?                    
         BNE   TOGGLE86                                                         
         MVC   OPMS1(3),=C'PDF'                                                 
         CLI   XMITPDF,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE82                                                         
         TM    PDFSECB,X'40'       YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE82 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH BIAS SUBTASK                          
         TM    OLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  OLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE86 CLI   BYTE,C'B'           TOGGLE BIAS TRANSMISSIONS?                   
         BNE   TOGGLE90                                                         
         MVC   OPMS1(3),=C'BIA'                                                 
         CLI   XMITBIAS,C'Y'       YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE88                                                         
         TM    BIASECB,X'40'       YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE88 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH BIAS SUBTASK                          
         TM    BLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  BLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLE90 CLI   BYTE,C'M'           TOGGLE E-MAIL TRANSMISSIONS?                 
         BNE   TOGGLEA0                                                         
         MVC   OPMS1(3),=C'MQM'                                                 
         CLI   XMITMQM,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLE95                                                         
         TM    MQMECB,X'40'        YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLE95 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH E-MAIL SUBTASK                        
         TM    MLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  MLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLEA0 CLI   BYTE,C'T'           TOGGLE BDE-EMAIL TRANSMISSIONS?              
         BNE   TOGGLEB0                                                         
         MVC   OPMS1(3),=C'BDE'                                                 
         CLI   XMITBDE,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLEA5                                                         
         TM    BDEECB,X'40'        YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLEA5 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH BDE SUBTASK                           
         TM    TLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  TLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLEB0 CLI   BYTE,C'P'           TOGGLE BDE-FTP TRANSMISSIONS?                
         BNE   TOGGLEC0                                                         
         MVC   OPMS1(3),=C'BDF'                                                 
         CLI   XMITBDF,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLEB5                                                         
         TM    BDFECB,X'40'        YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLEB5 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH BDE SUBTASK                           
         TM    PLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEB7            YES -- DON'T BOTHER TO POST AGAIN            
         POST  PLOOKECB                                                         
*                                                                               
TOGGLEB7 CLI   BDF2,C'Y'                                                        
         BNE   TOGGLEX                                                          
         TM    TLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  TLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLEC0 CLI   BYTE,C'Z'           TOGGLE ENCODA TRANSMISSIONS?                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   OPMS1(3),=C'ENC'                                                 
         CLI   XMITENC,C'Y'        YES -- COULD IT BE RUNNING?                  
         BNE   TOGGLEC5                                                         
         TM    ENCECB,X'40'        YES -- IS THE SUBTASK RUNNING?               
         BZ    TOGGLOFF            NO                                           
*                                                                               
TOGGLEC5 MVC   OPMS1+18(8),=C'RUNNING '                                         
         BRAS  RE,ATTACH           ATTACH ENCODA SUBTASK                        
         TM    ZLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    TOGGLEX             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ZLOOKECB                                                         
         B     TOGGLEX                                                          
*                                                                               
TOGGLOFF MVC   OPMS1+18(8),=C'DISABLED'                                         
         BRAS  RE,DETACH           DETACH DARE SUBTASK                          
*                                                                               
TOGGLEX  GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE STATUS OF THE COLUMBINE PARTNERS, OR TOGGLE THE                   
* STATUS OF A PARTICULAR PARTNER.                                               
***********************************************************************         
COLSTAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R9,=C'LUTABLE '     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE COLUMBINE LUTABLE                
*                                                                               
         CLC   =C'COL,STATUS',WORK                                              
         BNE   COLST20                                                          
*                                                                               
         L     R4,=A(LUTABLE)      TABLE OF LUNAMES                             
         USING LUTABLED,R4                                                      
COLST10  CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    COLSTATX                                                         
         MVC   OPMS3+35(8),LUNAME                                               
         MVC   OPMS3+44(8),=C'BAD     '                                         
         TM    LUSTATUS,LUNOGOOD                                                
         BO    *+10                                                             
         MVC   OPMS3+44(8),=C'OK      '                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS3,OPMS3)                       
         LA    R4,LUTABLEQ(,R4)    BUMP TO NEXT TABLE ENTRY                     
         B     COLST10                                                          
         DROP  R4                                                               
*                                                                               
COLST20  L     R4,=A(LUTABLE)      TABLE OF LUNAMES                             
*                                                                               
         USING LUTABLED,R4                                                      
COLST30  CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   COLST40                                                          
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'INVALID COLUMBINE LUNAME'          
         B     COLSTATX                                                         
*                                                                               
COLST40  CLC   LUNAME,WORK+4       FIND MATCH ON LUNAME                         
         BE    *+12                                                             
         LA    R4,LUTABLEQ(,R4)    BUMP TO NEXT TABLE ENTRY                     
         B     COLST30                                                          
*                                                                               
         MVC   OPMS3+35(8),LUNAME                                               
         TM    LUSTATUS,LUNOGOOD                                                
         BO    *+18                                                             
         OI    LUSTATUS,LUNOGOOD        STATION WAS ON: SHUT IT OFF             
         MVC   OPMS3+44(8),=C'DISABLED'                                         
         B     COLST50                                                          
*                                                                               
         NI    LUSTATUS,X'FF'-LUNOGOOD  STATION WAS OFF: TURN IT ON             
         MVC   OPMS3+44(8),=C'ENABLED '                                         
         TM    CLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    COLST50             YES -- DON'T BOTHER TO POST AGAIN            
         POST  CLOOKECB                                                         
*                                                                               
COLST50  GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS3,OPMS3)                       
         DROP  R4                                                               
*                                                                               
COLSTATX LA    R9,=C'LUTABLE '     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE COLUMBINE LUTABLE                
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE STATUS OF THE FAXGATE PARTNERS, OR TOGGLE THE                     
* STATUS OF A PARTICULAR PARTNER.                                               
***********************************************************************         
FAXGSTAT NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'FAX,STATUS',WORK                                              
         BNE   FAXGST20                                                         
*                                                                               
         L     R4,AFXTABLE         FAXGATE APPC/MVS CONTROL TABLE               
         USING APPCD,R4                                                         
FAXGST10 CLC   =X'FFFFFFFF',0(R4)  END OF TABLE?                                
         BE    FAXGSTAX                                                         
         MVC   OPMS4(13),=C'SENDER   TO  '                                      
         CLI   CONVERSATION_FUNCTION,SENDER                                     
         BE    *+10                                                             
         MVC   OPMS4(13),=C'RECEIVER FROM'                                      
         MVC   OPMS4+30(8),PARTNER_LU_NAME+9 ASSUME LUNAME IS QUALIFIED         
         CLC   PARTNER_LU_NAME+9(8),MYSPACES                                    
         BNE   *+10                                                             
         MVC   OPMS4+30(8),PARTNER_LU_NAME  NOT FULLY-QUALIFIED YET             
         MVC   OPMS4+39(11),=C'DISABLED   '                                     
         TM    INTERNAL_CONVERSATION_STATUS,OPERATOR_ENABLED                    
         BZ    FAXGST15                                                         
         MVC   OPMS4+39(11),=C'BAD        '                                     
         TM    INTERNAL_CONVERSATION_STATUS,COMMUNICATIONS_ERROR                
         BO    FAXGST15                                                         
         MVC   OPMS4+39(11),=C'UNALLOCATED'                                     
         TM    INTERNAL_CONVERSATION_STATUS,ALLOCATED                           
         BZ    FAXGST15                                                         
         MVC   OPMS4+39(11),=C'ENABLED    '                                     
*                                                                               
FAXGST15 GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS4,OPMS4)                       
         LA    R4,APPCDLEN(,R4)    BUMP TO NEXT TABLE ENTRY                     
         B     FAXGST10                                                         
         DROP  R4                                                               
*                                                                               
FAXGST20 L     R4,AFXTABLE         FAXGATE APPC/MVS CONTROL TABLE               
*                                                                               
         USING APPCD,R4                                                         
FAXGST30 CLC   =X'FFFFFFFF',0(R4)  END OF TABLE?                                
         BNE   FAXGST40                                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'INVALID FAXGATE LUNAME'            
         B     FAXGSTAX                                                         
*                                                                               
FAXGST40 MVC   DUB,PARTNER_LU_NAME+9 ASSUME LUNAME IS FULLY-QUALIFIED           
         CLC   PARTNER_LU_NAME+9(8),MYSPACES                                    
         BNE   *+10                                                             
         MVC   DUB,PARTNER_LU_NAME NOT FULLY-QUALIFIED YET                      
*                                                                               
         CLC   DUB,WORK+4          FIND MATCH ON LUNAME                         
         BE    *+12                                                             
         LA    R4,APPCDLEN(,R4)    BUMP TO NEXT TABLE ENTRY                     
         B     FAXGST30                                                         
*                                                                               
         MVC   OPMS4(13),=C'SENDER / RCVR'                                      
         MVC   OPMS4+30(8),DUB                                                  
         TM    INTERNAL_CONVERSATION_STATUS,OPERATOR_ENABLED                    
         BZ    *+18                                                             
         NI    INTERNAL_CONVERSATION_STATUS,X'FF'-OPERATOR_ENABLED              
         MVC   OPMS4+39(11),=C'DISABLED   ' PARTNER WAS ON: TURN IT OFF         
         B     FAXGST50                                                         
*                                                                               
         OI    INTERNAL_CONVERSATION_STATUS,OPERATOR_ENABLED                    
         MVC   OPMS4+39(11),=C'ENABLED    '                                     
         TM    XLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FAXGST50            YES -- DON'T BOTHER TO POST AGAIN            
         POST  XLOOKECB                                                         
*                                                                               
FAXGST50 GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS4,OPMS4)                       
         DROP  R4                                                               
*                                                                               
FAXGSTAX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REATTACH THE DARE SENDING SUBTASK IF IT HAS DIED UNEXPECTEDLY.                
* SAME FOR FAXGATE AND BIAS.                                                    
***********************************************************************         
REATTACH NTR1  BASE=*,LABEL=*                                                   
         CLI   XMITDARE,C'Y'       COULD DARE SUBTASK BE RUNNING?               
         BNE   REATFAXG            NO                                           
         TM    DAREECB,X'40'       IS THE SUBTASK RUNNING?                      
         BZ    REATFAXG            YES                                          
*                                                                               
         MVI   BYTE,C'D'                                                        
         BRAS  RE,DETACH           DETACH DARE SUBTASK                          
*                                                                               
         STIMERM SET,ID=STIMER2,BINTVL=THIRTYSC,WAIT=YES                        
         LTR   RF,RF                                                            
         BZ    *+6                 WAIT 30 SECONDS                              
         DC    H'0'                                                             
*                                                                               
         MVI   BYTE,C'D'                                                        
         BRAS  RE,ATTACH           REATTACH DARE SUBTASK                        
         MVC   OPMS1(3),=C'DAR'                                                 
         MVC   OPMS1+18(8),=C'RUNNING '                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
REATFAXG CLI   XMITFAXG,C'Y'       COULD FAXGATE SUBTASK BE RUNNING?            
         BNE   REATBIAS            NO                                           
         TM    FAXGECB,X'40'       IS THE SUBTASK RUNNING?                      
         BZ    REATBIAS            YES                                          
*                                                                               
         MVI   BYTE,C'X'                                                        
         BRAS  RE,DETACH           DETACH FAXGATE SUBTASK                       
*                                                                               
         STIMERM SET,ID=STIMER2,BINTVL=THIRTYSC,WAIT=YES                        
         LTR   RF,RF                                                            
         BZ    *+6                 WAIT 30 SECONDS                              
         DC    H'0'                                                             
*                                                                               
         MVI   BYTE,C'X'                                                        
         BRAS  RE,ATTACH           REATTACH FAXGATE SUBTASK                     
         MVC   OPMS1(3),=C'FAX'                                                 
         MVC   OPMS1+18(8),=C'RUNNING '                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
REATBIAS CLI   XMITBIAS,C'Y'       COULD BIAS SUBTASK BE RUNNING?               
         BNE   REATX               NO                                           
         TM    BIASECB,X'40'       IS THE SUBTASK RUNNING?                      
         BZ    REATX               YES                                          
*                                                                               
         MVI   BYTE,C'B'                                                        
         BRAS  RE,DETACH           DETACH BIAS SUBTASK                          
*                                                                               
         STIMERM SET,ID=STIMER2,BINTVL=THIRTYSC,WAIT=YES                        
         LTR   RF,RF                                                            
         BZ    *+6                 WAIT 30 SECONDS                              
         DC    H'0'                                                             
*                                                                               
         MVI   BYTE,C'B'                                                        
         BRAS  RE,ATTACH           REATTACH BIAS SUBTASK                        
         MVC   OPMS1(3),=C'BIA'                                                 
         MVC   OPMS1+18(8),=C'RUNNING '                                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
REATX    XIT1                                                                   
*                                                                               
THIRTYSC DC    F'3000'             THIRTY SECONDS                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* OPEN CONTROL FILE FOR READ-ONLY                                               
**********************************************************************          
OPENCTFL NTR1  BASE=*                                                           
*                                                                               
         BC    0,OPENCTX           FIRST TIME THROUGH, DON'T EXIT               
         MVI   *-3,X'F0'           ONLY OPEN CONTROL SYSTEM ONCE                
*                                                                               
         USING UTLD,RF                                                          
         L     RF,=A(UTL)                                                       
         MVI   TSYS,X'0A'                                                       
         DROP  RF                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE NGENDIR NGENFIL X',A(IO),0                            
*                                                                               
*                                                                               
* DUMMY CALL TO CTRL ENQUEUE TO FORCE DDSQDC TO BE OPENED BY MAIN TASK.         
* OTHERWISE A SUBTASK OPENS IT AND THEN MVS CLOSES IT PREMATURELY WHEN          
* THE SUBTASK THAT OPENED IT IS TERMINATED.                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'ENQCTL  '),(C'T',=C'CTRL')                    
*                                                                               
* DO THE SAME DUMMY CALL FOR DDSQDQ FOR PQS                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'ENQDEQ  '),(C'T',=C'PRTQU')                   
*                                                                               
OPENCTX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* OPEN THE EDICT TRANSACTION FILE AND READ THE PREVIOUS BUSINESS DAYS'          
* AND TODAY'S EDICT RECORDS INTO A CORE TABLE, SO WE KNOW WHAT WAS              
* ALREADY SENT.  ALSO, SAVE THE EDICT EOF DISK ADDRESS.                         
**********************************************************************          
OPENEDFL NTR1  BASE=*,LABEL=*                                                   
         PRNT  OPEN_EDICTFILE,PRINT=ALWAYS                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',DAOPEN,A(EDICTBLK),0,EDICTFL,0,0          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,=X'00010100',0                                           
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         MVC   EDCTBKSZ+2(2),DMCB+14  RETURNED PHYSICAL RECORD LENGTH           
         L     RF,=A(EDICTBLK)     THE FIRST LOGICAL RECORD HAS                 
         USING EDFILD,RF            NUMBER OF TRACKS/DAY                        
         CLI   EDFMON,EDFMONPQ                                                  
         BE    *+6                                                              
         DC    H'0'                WE'RE LOOKING AT THE WRONG RECORD            
         MVC   EDCTFRPT+1(1),EDFBKPTK PHYSICAL RECORDS PER TRACK                
         MVC   EDCTFRCL,EDFLRECL   LOGICAL RECORD LENGTH                        
         CLC   EDCTFRCL,=H'256'                                                 
         BNH   *+6                                                              
         DC    H'0'                MAX LRECL IS 256 FOR NOW                     
         SR    R3,R3                                                            
         ICM   R3,3,EDFTKPDY       NUMBER OF TRACKS/DAY                         
         MVC   EDCTFRPB+1(1),EDFRCPBK  NUMBER OF LOGICAL RECS/BLOCK             
         DROP  RF                                                               
         EDIT  (R3),(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  TRACKS_PER_DAY,PRINT=ALWAYS                                      
         EDIT  EDCTBKSZ,(7,P+30),ALIGN=LEFT,ZERO=NOBLANK                        
         PRNT  BLOCK_SIZE,PRINT=ALWAYS                                          
         EDIT  EDCTFRPT,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                        
         PRNT  BLOCKS_PER_TRACK,PRINT=ALWAYS                                    
         EDIT  EDCTFRPB,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                        
         PRNT  RECORDS_PER_BLOCK,PRINT=ALWAYS                                   
         EDIT  EDCTFRCL,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                        
         PRNT  LOGICAL_REC_LEN,PRINT=ALWAYS                                     
*                                                                               
         STH   R3,EDCTFTPD         TRACKS PER DAY. . .                          
         MH    R3,EDCTFRPT         . . . TIMES BLOCKS PER TRACK. . .            
         MH    R3,EDCTFRPB         . . . TIMES RECORDS PER BLOCK. . .           
         ST    R3,MAXREPS          (SAVE MAXIMUM NO. REPORTS PER DAY)           
         MH    R3,XTBLSIZE         . . . TIMES # DAYS OF DATA . . .             
*                                                                               
         C     R3,=A(BINSQ)        EXCESS MAX CAPACITY OF BINSRH31?             
         BNH   *+8                                                              
         L     R3,=A(BINSQ)        YES, USE MAX CAPACITY OF BINSRH31            
*                                                                               
         ST    R3,XMTTBMAX                                                      
         MHI   R3,XMTTBLQ          . . . TIMES RECLEN = TABLE SIZE              
         LR    R4,R3               SAVE LENGTH OF TABLE                         
         AHI   R3,16               ROOM FOR LABEL, COUNTER, MAX ENTRY           
*                                                                               
         SAM31                                                                  
         STORAGE OBTAIN,LENGTH=(3),LOC=ANY,BNDRY=PAGE                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,R1),=C'*XMITTBL'                                             
         XC    8(4,R1),8(R1)       CLEAR COUNTER                                
         MVC   12(4,R1),XMTTBMAX                                                
         LA    R1,16(R1)           BUMP PAST LABEL, COUNTER, MAX#               
         ST    R1,AXMTTBL          A(START OF XMTTBL)                           
         LR    RE,R1               CLEAR TABLE                                  
         LR    RF,R4                                                            
         XCEFL                                                                  
         SAM24                                                                  
*                                                                               
         LH    R7,XTBLSIZE                                                      
         XC    FULL3,FULL3         EDICT FILE DISK ADDRESS                      
         MVC   WORK(6),TODAY6                                                   
         LNR   RE,R7                                                            
         AHI   RE,1                                                             
         ST    RE,DMCB+8                                                        
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6  GOTO THE EARLIEST DAY                
         GOTO1 =V(DATCON),DMCB,WORK+6,(3,FULL)                                  
         MVC   DAY6,WORK+6                                                      
*                                                                               
OPNED30  ZIC   R2,FULL+2           DAY NUMBER                                   
         BCTR  R2,0                                                             
         MH    R2,EDCTFTPD                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER FOR TODAY         
         LH    R1,EDCTFTPD                                                      
         BCTR  R1,0                                                             
         AR    R1,R2               LAST TRACK NUMBER FOR TODAY                  
         STCM  R1,3,EDCTFLST                                                    
*                                                                               
OPNED40  STCM  R2,3,FULL3          TRACK NUMBER                                 
*                                                                               
         LA    R3,1                                                             
OPNED50  STC   R3,FULL3+2          BLOCK NUMBER                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,FULL3,0                                                  
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,1                LOGICAL RECORD NUMBER                        
         L     R4,=A(EDICTBLK)                                                  
         USING EDFILD,R4                                                        
OPNED60  CLI   EDFMON,EDFMONPQ     IS THIS A 'PERMANENT' RECORD?                
         BE    OPNED100            YES - SKIP IT                                
*                                                                               
         CLC   EDFMON,FULL+1       IS THIS RECORD FROM THIS MONTH?              
         BNE   OPNED200            NO -- WE'VE FOUND THE EOF                    
*                                                                               
         CLI   EDFSTAT,EDFNOOP     IS THIS A NO-OP RECORD?                      
         BE    OPNED100            YES - SKIP IT                                
         CLI   EDFSYS,X'81'        IS THIS AN EDICT TRANSACTION RECORD?         
         BL    *+12                                                             
         CLI   EDFSYS,X'A9'                                                     
         BNH   OPNED100            NO - SKIP IT                                 
*                                                                               
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING XMTTABLD,RF                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,EDFPQUID       PQ REPORT SENDING USERID                     
         STCM  R1,3,XMTUSRID                                                    
         D     R0,NUMPQS                                                        
         AHI   R0,1                                                             
         STC   R0,XMTPRTQ          PRINT QUEUE NUMBER                           
         MVI   XMTSOURC,XMTSRCPQ   PRINT QUEUE REPORT                           
         MVC   XMTSUBID,EDFPQSUB   PQ REPORT SUB-ID                             
         MVC   XMTREFNO,EDFPQREF   PQ REPORT REFERENCE NUMBER                   
         MVC   XMTCRDAT,EDFPQDAT   PQ REPORT CREATION DATE - CMPRSD             
         MVC   XMTCRTIM,EDFPQTIM   PQ REPORT CREATION TIME                      
         MVC   XMTLOGNO,EDFPQSEQ   PQ REPORT LOGICAL REPORT NUMBER              
         MVC   XMTDSTNO,EDFPQDSS   PQ REPORT LOGICAL REPORT DEST NUMBER         
         MVC   XMTSTAT,EDFSTAT     REPORT STATUS                                
         MVC   XMTMETH,EDFMETH     METHOD OF TRANSMISSION                       
         MVC   XMTSYS,EDFSYS       SYSTEM                                       
         MVC   XMTTYPE,EDFTYPE     RECORD TYPE                                  
         MVC   XMTDSKAD(3),FULL3   TTTTBB                                       
         STC   R5,XMTDSKAD+3       LOGICAL RECORD NUMBER                        
         OC    EDFCPQCL,EDFCPQCL                                                
         BZ    *+8                                                              
         OI    XMTFLAGS,XMTCPQSQ                                                
         OC    EDFCPQST,EDFCPQST                                                
         BZ    *+8                                                              
         OI    XMTFLAGS,XMTCPQSQ                                                
         DROP  RF                                                               
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         GOTO1 =V(BINSRCH),DMCB,WORK,AXMTTBL,,(1,XMTTBLQ),XMTKEYQ,     +        
               XMTTBMAX                                                         
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               NUMBER OF ENTRIES IN TABLE                   
         MVC   0(4,RF),DMCB+8      UPDATE TABLE SIZE                            
         SAM24                                                                  
*                                                                               
         TM    DMCB,X'80'          WAS THIS REPORT ALREADY IN TABLE?            
         BO    *+16                                                             
         LA    R1,WORK                                                          
         BRAS  RE,DUPWARN          RECORD IS DUPLICATED IN EDICT FILE           
         B     OPNED63                                                          
*                                                                               
         NI    DMCB,X'7F'          TURN OFF HIGH-ORDER BIT, THEN. . .           
         OC    DMCB(4),DMCB        . . . MAKE SURE THE RECORD WENT IN           
         BNZ   *+6                                                              
         DC    H'0'                XMITTBL IS NOT LARGE ENOUGH                  
         MVC   XMTTBNUM,DMCB+8     UPDATE TABLE SIZE                            
*                                                                               
OPNED63  TM    EDFSTAT,EDFSTWTG    IS REPORT WAITING TO BE SENT?                
         BZ    OPNED100            NO                                           
*                                                                               
         CLI   EDFMETH,C'E'        EASYLINK?                                    
         BNE   OPNED65                                                          
         TM    ELOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  ELOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED65  CLI   EDFMETH,C'X'        FAXGATE?                                     
         BNE   OPNED70                                                          
         TM    XLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  XLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED70  CLI   EDFMETH,C'Q'        NFX?                                         
         BNE   OPNED72                                                          
         TM    QLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  QLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED72  CLI   EDFMETH,C'B'        BIAS?                                        
         BNE   OPNED74                                                          
         TM    BLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  BLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED74  CLI   EDFMETH,C'O'        PDF?                                         
         BNE   OPNED75                                                          
         TM    OLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  OLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED75  CLI   EDFMETH,C'A'        ADVANTIS?                                    
         BNE   OPNED80                                                          
         TM    ALOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  ALOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED80  CLI   EDFMETH,C'J'        NJE?                                         
         BNE   OPNED85                                                          
         TM    JLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  JLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED85  CLI   EDFMETH,C'D'        DARE?                                        
         BNE   OPNED87                                                          
         TM    DLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  DLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED87  CLI   EDFMETH,C'F'        FTP?                                         
         BNE   OPNED89                                                          
         TM    FLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  FLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED89  CLI   EDFMETH,C'C'        COLUMBINE?                                   
         BNE   OPNED94                                                          
         TM    CLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  CLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED94  CLI   EDFMETH,C'T'        BDE-EMAIL?                                   
         BNE   OPNED96                                                          
         TM    TLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  TLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED96  CLI   EDFMETH,C'P'        BDE-FTP?                                     
         BNE   OPNED97                                                          
         TM    PLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED96T            YES -- DON'T BOTHER TO POST AGAIN            
         POST  PLOOKECB                                                         
*                                                                               
OPNED96T CLI   BDF2,C'Y'                                                        
         BNE   OPNED100                                                         
         TM    TLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  TLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED97  CLI   EDFMETH,C'Z'        ENCODA?                                      
         BNE   OPNED98                                                          
         TM    ZLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  ZLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED98  CLI   EDFMETH,C'M'        E-MAIL?                                      
         BNE   OPNED100                                                         
         TM    MLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    OPNED100            YES -- DON'T BOTHER TO POST AGAIN            
         POST  MLOOKECB                                                         
         B     OPNED100                                                         
*                                                                               
OPNED100 L     R1,FULL2            INCREMENT NUMBER OF REPORTS TODAY            
         LA    R1,1(R1)                                                         
         ST    R1,FULL2                                                         
*                                                                               
         AH    R4,EDCTFRCL         BUMP TO NEXT RECORD                          
         LA    R5,1(R5)                                                         
         CH    R5,EDCTFRPB         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   OPNED60                                                          
         DROP  R4                                                               
*                                                                               
         LA    R3,1(R3)            NO                                           
         CH    R3,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   OPNED50             YES                                          
*                                                                               
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
         BNH   OPNED40                                                          
         WTO   'YESTURDAY FILE IS FULL'                                         
*        DC    H'0'                THIS DAY'S PARTITION IS FULL                 
*                                                                               
OPNED200 XC    FULL2,FULL2         COUNT TODAY'S REPORTS                        
         MVC   WORK(6),DAY6                                                     
         GOTO1 =V(ADDAY),DMCB,WORK,WORK+6,F'1'  NEXT DAY                        
         GOTO1 =V(DATCON),DMCB,WORK+6,(3,FULL)                                  
         MVC   DAY6,WORK+6                                                      
         BCT   R7,OPNED30                                                       
*                                                                               
OPNEDX   STC   R5,BYTE             SAVE LOGICAL RECORD NUMBER                   
         MVC   DUB(3),FULL3        PRINT TTTTBBRR                               
         MVC   DUB+3(1),BYTE                                                    
         GOTO1 =V(HEXOUT),DMCB,DUB,P+30,4,=C'TOG'                               
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,=A(EDICTEOF)     FOR EDICT ADDING ROUTINE                     
         MVC   0(4,RF),DUB                                                      
         L     RF,=A(EDICTBSZ)                                                  
         MVC   0(4,RF),EDCTBKSZ                                                 
         L     RF,=A(EDICTRPB)                                                  
         MVC   0(2,RF),EDCTFRPB                                                 
         L     RF,=A(EDICTRCL)                                                  
         MVC   0(2,RF),EDCTFRCL                                                 
         L     RF,=A(EDICTLST)                                                  
         MVC   0(2,RF),EDCTFLST                                                 
         L     RF,=A(NUMREPS)                                                   
         MVC   0(4,RF),FULL2                                                    
         PRNT  EDICTFILE_OPENED,PRINT=ALWAYS                                    
*                                                                               
*CHECK IF XMTTBMAX - XMTTBNUM  > MAXREPS, THEN GOOD                             
*OTHERWISE, IT MAY OVERFLOW, SO SHOULD REDUCE # DAYS OF TRANSACTIONS            
         L     RE,XMTTBMAX                                                      
         S     RE,XMTTBNUM                                                      
         C     RE,MAXREPS                                                       
         BNL   *+6                                                              
         DC    H'0'                EXCESS THE ADDRESS SPACE OF BINSRCH          
*                                  REDUCE THE NUMBER OF DAY'S TRANSAC,          
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* NTRY: TODAY3   = TODAY'S DATE IN YMD (3 BYTES)                                
*       EDCTFTPD = # TRACKS PER DAY                                             
*       EDCTFRPT = # BLOCKS PER TRACK                                           
*       EDCTFRPB = # RECORDS PER BLOCK                                          
*       EDCTFLST = LAST TRACK NUMBER FOR TODAY                                  
*       EDICTEOF = DISK ADDRESS OF NEXT AVALIABLE ENTRY                         
*                                                                               
***********************************************************************         
CHKXEDFL NTR1  BASE=*,LABEL=*                                                   
CHXEF10  L     RF,=A(EDICTEOF)     FROM EDICTADD ROUTINE                        
         MVC   CHXWORK,0(RF)       ASSUME THIS IS TRUE EOF                      
         MVC   CHXFULL,CHXWORK                                                  
         MVI   CHXFULL+3,0         CLEAR LOGICAL RECORD NUMBER                  
         SR    R2,R2                                                            
         ICM   R2,3,CHXWORK        TTTT - TRACK NUMBER                          
         SR    R3,R3                                                            
         IC    R3,CHXWORK+2        BB - BLOCK NUMBER                            
         SR    R5,R5                                                            
         IC    R5,CHXWORK+3        RR - LOGICAL RECORD NUMBER                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,CHXFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         MH    RE,EDCTFRCL         BUMP TO NEXT RECORD                          
         L     R4,=A(EDICTBLK)                                                  
         AR    R4,RE                                                            
         B     CHXEF40                                                          
*                                                                               
CHXEF20  STCM  R2,3,CHXFULL        TRACK NUMBER                                 
*                                                                               
         LA    R3,1                                                             
CHXEF30  STC   R3,CHXFULL+2        BLOCK NUMBER                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,CHXFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,1                LOGICAL RECORD NUMBER                        
         L     R4,=A(EDICTBLK)                                                  
         USING EDFILD,R4                                                        
CHXEF40  LH    R1,EDCTFRCL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)       CLEAR THE RECORD                             
         BZ    CHXEF90             ALL THE REST ENTRIES SHOULD BE NULLS         
*                                                                               
         CLI   EDFMON,EDFMONPQ     IS THIS A 'PERMANENT' RECORD?                
         BE    CHXEF90             YES - SKIP IT                                
*                                                                               
         CLI   EDFSTAT,EDFNOOP     IS THIS A NO-OP RECORD?                      
         BE    CHXEF90             YES - SKIP IT                                
*                                                                               
         CLC   EDFMON,TODAY3+1     IS THIS RECORD FROM THIS MONTH?              
         BNE   CHXEF90             NO - OKAY, EDICT FILE WASN'T CLEARED         
*                                                                               
* NOOP (SET EDFSTAT=X'FF') THE CORRUPTED ENTRIES IN THE EDICT FILE              
*                                                                               
         MVC   CHXWORK+4(3),CHXFULL     TTTTBB - TRACK & BLOCK NUMBER           
         STC   R5,CHXWORK+4+3           RR - LOGICAL RECORD NUMBER              
         LA    R1,CHXWORK          0(R1),4(R1)=BEGIN AND END DISK ADDR          
         BRAS  RE,FFEDFIL          NOOP SOME EDICT FILE ENTRIES                 
*                                                                               
* THEN FIND THE NEXT EOF AND THEN CONTINUE CHECKING THE REST OF THE             
* EDICT FILE                                                                    
*                                                                               
         LA    R1,CHXWORK+4                                                     
         BRAS  RE,FNDEDEOF         FIND THE NEXT EOF                            
         B     CHXEF10             CONTINUE CHECKING EDICT FILE                 
*                                                                               
CHXEF90  AH    R4,EDCTFRCL         BUMP TO NEXT RECORD                          
         LA    R5,1(R5)                                                         
         CH    R5,EDCTFRPB         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   CHXEF40                                                          
*                                                                               
         LA    R3,1(R3)            NO                                           
         CH    R3,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   CHXEF30             YES                                          
*                                                                               
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
         BNH   CHXEF20                                                          
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
CHXFULL  DS    F                                                                
CHXWORK  DS    2F                                                               
         EJECT                                                                  
***********************************************************************         
* NOOP (SET EDFSTAT=X'FF') THE CORRUPTED ENTRIES IN THE EDICT FILE              
*                                                                               
* NTRY: 0(R1) = BEGINNING DISK ADDRESS                                          
*       4(R1) = ENDING DISK ADDRESS (ENTRY AT THIS DA WILL BE NOOP'ED)          
*       EDCTFTPD = # TRACKS PER DAY                                             
*       EDCTFRPT = # BLOCKS PER TRACK                                           
*       EDCTFRPB = # RECORDS PER BLOCK                                          
*       EDCTFLST = LAST TRACK NUMBER FOR TODAY                                  
*       TODAY3   = TODAY'S DATE IN YMD (3 BYTES)                                
***********************************************************************         
FFEDFIL  NTR1  BASE=*,LABEL=*                                                   
         MVC   FFEWORK(8),0(R1)                                                 
         MVC   FFEFULL,0(R1)                                                    
         L     R9,4(R1)                                                         
         MVI   FFEFULL+3,0         CLEAR LOGICAL RECORD NUMBER                  
         SR    R2,R2                                                            
         ICM   R2,3,0(R1)          TTTT - TRACK NUMBER                          
         SR    R3,R3                                                            
         IC    R3,2(R1)            BB - BLOCK NUMBER                            
         SR    R5,R5                                                            
         IC    R5,3(R1)            RR - LOGICAL RECORD NUMBER                   
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,FFEFULL,FFEDUB,4,=C'TOG'                         
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ENQ   (MAJORNAM,FFEDUB,E,8)                                            
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,FFEFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         MH    RE,EDCTFRCL         BUMP TO THE TARGET LOGICAL RECORD            
         L     R4,=A(EDICTBLK)                                                  
         AR    R4,RE                                                            
         B     FFEF40                                                           
*                                                                               
FFEF20   STCM  R2,3,FFEFULL        TRACK NUMBER                                 
*                                                                               
         LA    R3,1                                                             
FFEF30   STC   R3,FFEFULL+2        BLOCK NUMBER                                 
*                                                                               
         CL    R9,FFEFULL          REACH THE ENDING DISK ADDR?                  
         BNH   FFEFX               YES - EXIT W/O READ/WRITE THIS BLOCK         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,FFEFULL,FFEDUB,4,=C'TOG'                         
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ENQ   (MAJORNAM,FFEDUB,E,8)                                            
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,FFEFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,1                LOGICAL RECORD NUMBER                        
         L     R4,=A(EDICTBLK)                                                  
*                                                                               
FFEF40   LR    RE,R5                                                            
         ICM   RE,14,FFEFULL                                                    
         CLR   RE,R9               REACH THE ENDING DISK ADDR?                  
         BNL   FFEFX               YES - EXIT                                   
*                                                                               
         USING EDFILD,R4                                                        
         CLI   EDFMON,EDFMONPQ     IS THIS A 'PERMANENT' RECORD?                
         BE    FFEF50              SKIP THIS ONE                                
*                                  PRINT THE BAD ENTRY BEFORE NOOP IT           
         STCM  RE,15,WORK                                                       
         GOTO1 =V(HEXOUT),DMCB,WORK,P+30,4,=C'TOG'                              
         PRNT  BAD_EDICT_ENTRY,PRINT=ALWAYS                                     
         XC    DMCB+12(2),DMCB+12                                               
         MVC   DMCB+14(2),EDCTFRCL                                              
         GOTO1 =V(PRNTBL),DMCB,0,(R4),C'DUMP',,=C'1D'                           
*                                                                               
         MVI   EDFSTAT,EDFNOOP     MARK THIS ENTRY NOOP                         
         MVC   EDFMON,TODAY3+1     MOVE IN MONTH # ALSO                         
         DROP  R4                                                               
*                                                                               
FFEF50   AH    R4,EDCTFRCL         BUMP TO NEXT RECORD                          
         LA    R5,1(R5)                                                         
         CH    R5,EDCTFRPB         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   FFEF40                                                           
*                                                                               
* WRITE BACK THIS BLOCK AND DEQ THIS BLOCK                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',WTID,A(EDICTBLK),EDCTBKSZ,       +        
               EDICTFL,FFEFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                UNABLE TO WRITE EDICT FILE BLOCK             
         DEQ   (MAJORNAM,FFEDUB,8)                                              
*                                                                               
         LA    R3,1(R3)            NO                                           
         CH    R3,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   FFEF30              YES                                          
*                                                                               
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
         BNH   FFEF20                                                           
         DC    H'0'                THIS DAY'S PARTITION IS FULL                 
*                                                                               
* WRITE BACK THIS LAST BLOCK AND DEQ THIS BLOCK                                 
*                                                                               
FFEFX    GOTO1 DATAMGR,DMCB,=C'DADDS',WTID,A(EDICTBLK),EDCTBKSZ,       +        
               EDICTFL,FFEFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                UNABLE TO WRITE EDICT FILE BLOCK             
         DEQ   (MAJORNAM,FFEDUB,8)                                              
*                                                                               
* EMAIL WITH THE DISK ADDRESS RANGE OF FILE CORRUPTION                          
*                                                                               
FFEFX20  GOTO1 =V(HEXOUT),DMCB,FFEWORK,FFEMSG1A,4,=C'TOG'                       
         GOTO1 =V(HEXOUT),DMCB,FFEWORK+4,FFEMSG1B,4,=C'TOG'                     
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('FFEMSG1Q',FFEMSG1)                      
         XIT1                                                                   
                                                                                
         LTORG                                                                  
FFEFULL  DS    F                                                                
FFEDUB   DS    D                                                                
FFEWORK  DS    2F                                                               
FFEMSG1  DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:EDICT FILE CORRUPTED--'              
         DC    C'CHECK ENTRIES FROM '                                           
FFEMSG1A DS    CL8                                                              
         DC    C' UNTIL '                                                       
FFEMSG1B DS    CL8                                                              
FFEMSG1Q EQU   *-FFEMSG1                                                        
         EJECT                                                                  
***********************************************************************         
* NTRY: 0(R1) = BEGINNING DISK ADDRESS                                          
*       TODAY3   = TODAY'S DATE IN YMD (3 BYTES)                                
*       EDCTFTPD = # TRACKS PER DAY                                             
*       EDCTFRPT = # BLOCKS PER TRACK                                           
*       EDCTFRPB = # RECORDS PER BLOCK                                          
*       EDCTFLST = LAST TRACK NUMBER FOR TODAY                                  
*                                                                               
***********************************************************************         
FNDEDEOF NTR1  BASE=*,LABEL=*                                                   
         MVC   FEOFFULL,0(R1)                                                   
         MVI   FEOFFULL+3,0        CLEAR LOGICAL RECORD NUMBER                  
         SR    R2,R2                                                            
         ICM   R2,3,0(R1)          TTTT - TRACK NUMBER                          
         SR    R3,R3                                                            
         IC    R3,2(R1)            BB - BLOCK NUMBER                            
         SR    R5,R5                                                            
         IC    R5,3(R1)            RR - LOGICAL RECORD NUMBER                   
*                                                                               
         L     RE,=A(NUMREPS)      LOAD # OF REPORT FOR TODAY                   
         MVC   FEOFFUL2,0(RE)                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,FEOFFULL,0                                               
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         MH    RE,EDCTFRCL         BUMP TO THE TARGET LOGICAL RECORD            
         L     R4,=A(EDICTBLK)                                                  
         AR    R4,RE                                                            
         B     FEOF40                                                           
*                                                                               
FEOF20   STCM  R2,3,FEOFFULL       TRACK NUMBER                                 
*                                                                               
         LA    R3,1                                                             
FEOF30   STC   R3,FEOFFULL+2       BLOCK NUMBER                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,FEOFFULL,0                                               
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         LA    R5,1                LOGICAL RECORD NUMBER                        
         L     R4,=A(EDICTBLK)                                                  
*                                                                               
FEOF40   LH    R1,EDCTFRCL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)       EMPTY ENTRY?                                 
         BZ    FEOF190             YES - FIND THE NEW EOF                       
*                                                                               
         USING EDFILD,R4                                                        
         CLI   EDFMON,EDFMONPQ     IS THIS A 'PERMANENT' RECORD?                
         BE    FEOF150             SKIP THIS ONE                                
         CLI   EDFSTAT,EDFNOOP     IS THIS NOOP ENTRY?                          
         BE    FEOF150             SKIP THIS ONE                                
         CLC   EDFMON,TODAY3+1     IS THIS RECORD FROM THIS MONTH?              
         BNE   FEOF190             NO - OKAY, EDICT FILE WASN'T CLEARED         
         CLI   EDFSYS,X'81'        IS THIS AN EDICT TRANSACTION RECORD?         
         BL    *+12                                                             
         CLI   EDFSYS,X'A9'                                                     
         BNH   FEOF150             NO - SKIP IT                                 
*                                                                               
*WE SHOULD ADD THIS ENTRY TO THE XMTTABLE                                       
*                                                                               
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING XMTTABLD,RF                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,EDFPQUID       PQ REPORT SENDING USERID                     
         STCM  R1,3,XMTUSRID                                                    
         D     R0,NUMPQS                                                        
         AHI   R0,1                                                             
         STC   R0,XMTPRTQ          PRINT QUEUE NUMBER                           
         MVI   XMTSOURC,XMTSRCPQ   PRINT QUEUE REPORT                           
         MVC   XMTSUBID,EDFPQSUB   PQ REPORT SUB-ID                             
         MVC   XMTREFNO,EDFPQREF   PQ REPORT REFERENCE NUMBER                   
         MVC   XMTCRDAT,EDFPQDAT   PQ REPORT CREATION DATE - CMPRSD             
         MVC   XMTCRTIM,EDFPQTIM   PQ REPORT CREATION TIME                      
         MVC   XMTLOGNO,EDFPQSEQ   PQ REPORT LOGICAL REPORT NUMBER              
         MVC   XMTDSTNO,EDFPQDSS   PQ REPORT LOGICAL REPORT DEST NUMBER         
         MVC   XMTSTAT,EDFSTAT     REPORT STATUS                                
         MVC   XMTMETH,EDFMETH     METHOD OF TRANSMISSION                       
         MVC   XMTSYS,EDFSYS       SYSTEM                                       
         MVC   XMTTYPE,EDFTYPE     RECORD TYPE                                  
         MVC   XMTDSKAD(3),FEOFFULL       TTTTBB                                
         STC   R5,XMTDSKAD+3       LOGICAL RECORD NUMBER                        
         OC    EDFCPQCL,EDFCPQCL                                                
         BZ    *+8                                                              
         OI    XMTFLAGS,XMTCPQSQ                                                
         OC    EDFCPQST,EDFCPQST                                                
         BZ    *+8                                                              
         OI    XMTFLAGS,XMTCPQSQ                                                
         DROP  RF                                                               
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         GOTO1 =V(BINSRCH),DMCB,WORK,AXMTTBL,,(1,XMTTBLQ),XMTKEYQ,     +        
               XMTTBMAX                                                         
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               NUMBER OF ENTRIES IN TABLE                   
         MVC   0(4,RF),DMCB+8      UPDATE TABLE SIZE                            
         SAM24                                                                  
*                                                                               
         TM    DMCB,X'80'          WAS THIS REPORT ALREADY IN TABLE?            
         BO    *+6                                                              
         DC    H'0'                RECORD IS DUPLICATED IN EDICT FILE           
         NI    DMCB,X'7F'          TURN OFF HIGH-ORDER BIT, THEN. . .           
         OC    DMCB(4),DMCB        . . . MAKE SURE THE RECORD WENT IN           
         BNZ   *+6                                                              
         DC    H'0'                XMITTBL IS NOT LARGE ENOUGH                  
         MVC   XMTTBNUM,DMCB+8     UPDATE TABLE SIZE                            
*                                                                               
         TM    EDFSTAT,EDFSTWTG    IS REPORT WAITING TO BE SENT?                
         BZ    FEOF100             NO                                           
*                                                                               
         CLI   EDFMETH,C'E'        EASYLINK?                                    
         BNE   FEOF65                                                           
         TM    ELOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ELOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF65   CLI   EDFMETH,C'X'        FAXGATE?                                     
         BNE   FEOF70                                                           
         TM    XLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  XLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF70   CLI   EDFMETH,C'Q'        NFX?                                         
         BNE   FEOF72                                                           
         TM    QLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  QLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF72   CLI   EDFMETH,C'B'        BIAS?                                        
         BNE   FEOF74                                                           
         TM    BLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  BLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF74   CLI   EDFMETH,C'O'        PDF?                                         
         BNE   FEOF75                                                           
         TM    OLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  OLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF75   CLI   EDFMETH,C'A'        ADVANTIS?                                    
         BNE   FEOF80                                                           
         TM    ALOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ALOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF80   CLI   EDFMETH,C'J'        NJE?                                         
         BNE   FEOF85                                                           
         TM    JLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  JLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF85   CLI   EDFMETH,C'D'        DARE?                                        
         BNE   FEOF87                                                           
         TM    DLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  DLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF87   CLI   EDFMETH,C'F'        FTP?                                         
         BNE   FEOF89                                                           
         TM    FLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  FLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF89   CLI   EDFMETH,C'C'        COLUMBINE?                                   
         BNE   FEOF94                                                           
         TM    CLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  CLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF94   CLI   EDFMETH,C'T'        BDE-EMAIL?                                   
         BNE   FEOF96                                                           
         TM    TLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  TLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF96   CLI   EDFMETH,C'P'        BDE-FTP?                                     
         BNE   FEOF97                                                           
         TM    PLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF96T             YES -- DON'T BOTHER TO POST AGAIN            
         POST  PLOOKECB                                                         
*                                                                               
FEOF96T  CLI   BDF2,C'Y'                                                        
         BNE   FEOF100                                                          
         TM    TLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  TLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF97   CLI   EDFMETH,C'Z'        ENCODA?                                      
         BNE   FEOF98                                                           
         TM    ZLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ZLOOKECB                                                         
         B     FEOF100                                                          
*                                                                               
FEOF98   CLI   EDFMETH,C'M'        E-MAIL?                                      
         BNE   FEOF100                                                          
         TM    MLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    FEOF100             YES -- DON'T BOTHER TO POST AGAIN            
         POST  MLOOKECB                                                         
         B     FEOF100                                                          
         DROP  R4                                                               
*                                                                               
FEOF100  L     R1,FEOFFUL2         INCREMENT NUMBER OF REPORTS TODAY            
         LA    R1,1(R1)                                                         
         ST    R1,FEOFFUL2                                                      
*                                                                               
*                                                                               
FEOF150  AH    R4,EDCTFRCL         BUMP TO NEXT RECORD                          
         LA    R5,1(R5)                                                         
         CH    R5,EDCTFRPB         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   FEOF40                                                           
*                                                                               
         LA    R3,1(R3)            NO                                           
         CH    R3,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   FEOF30              YES                                          
*                                                                               
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
         BNH   FEOF20                                                           
         DC    H'0'                THIS DAY'S PARTITION IS FULL                 
*                                                                               
* FOR EDICTADD ROUTINE - SAVE THE NEW EOF DISK ADDRESS                          
FEOF190  L     RF,=A(EDICTEOF)                                                  
         STC   R5,FEOFFULL+3                                                    
         MVC   0(4,RF),FEOFFULL                                                 
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,FEOFFULL,P+30,4,=C'TOG'                          
         PRNT  NEW_EDICT_EOF,PRINT=ALWAYS                                       
*                                                                               
         L     R1,=A(NUMREPS)      UPDATE # OF REPORT FOR TODAY                 
         MVC   0(4,R1),FEOFFUL2                                                 
*                                                                               
*CHECK IF XMTTBMAX - XMTTBNUM  > MAXREPS, THEN GOOD                             
*OTHERWISE, IT MAY OVERFLOW, SO SHOULD REDUCE # DAYS OF TRANSACTIONS            
         L     RE,XMTTBMAX                                                      
         S     RE,XMTTBNUM                                                      
         C     RE,MAXREPS                                                       
         BNL   *+6                                                              
         DC    H'0'                EXCESS THE ADDRESS SPACE OF BINSRCH          
*                                  REDUCE THE NUMBER OF DAY'S TRANSAC,          
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
FEOFFULL DS    F                                                                
FEOFFUL2 DS    F                                                                
         EJECT                                                                  
***********************************************************************         
* NTRY: TODAY3   = TODAY'S DATE IN YMD (3 BYTES)                                
*       EDCTFTPD = # TRACKS PER DAY                                             
*       EDCTFRPT = # BLOCKS PER TRACK                                           
*       EDCTFRPB = # RECORDS PER BLOCK                                          
*       EDCTFLST = LAST TRACK NUMBER FOR TODAY                                  
*       EDICTEOF = DISK ADDRESS OF NEXT AVALIABLE ENTRY                         
*                                                                               
***********************************************************************         
SWEEPEDF NTR1  BASE=*,LABEL=*                                                   
         OC    SWECLK,SWECLK       SWEEP THIS ROUND?                            
         BZ    SWE10               YES                                          
         LH    RE,SWECLK                                                        
         BCTR  RE,0                                                             
         STH   RE,SWECLK           COUNT DOWN SWEEP CLOCK                       
         B     SWEXX                                                            
*                                                                               
SWE10    PRNT  SWEEP_EDICT_FILE,PRINT=ALWAYS                                    
         MVC   SWECLK,=Y(SWECLKQ)  RESET SWEEP CLOCK                            
*                                                                               
*ENQ/DEQ WITH 'EDICTADD' TO PREVENT ROUNTINE EDICTADD FROM CHANGING             
*THE FIELD EDICTEOF WHILE THIS ROUNTINE IS READING IT.                          
*                                                                               
         LA    R9,=C'EDICTADD'                                                  
         ENQ   (MAJORNAM,(9),E,8)                                               
         L     RF,=A(EDICTEOF)                                                  
         MVC   SWEEOF,0(RF)        GET THE CURRENT EDICT EOF                    
         DEQ   (MAJORNAM,(9),8)                                                 
*                                                                               
         SR    R9,R9               NUMBER OF CORRUPTED ENTRIES                  
*                                                                               
         ZIC   R2,TODAY3+2          DAY NUMBER                                  
         BCTR  R2,0                                                             
         MH    R2,EDCTFTPD                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER FOR TODAY         
*                                                                               
SWE20    STCM  R2,3,SWEFULL        TRACK NUMBER                                 
*                                                                               
         LA    R3,1                                                             
SWE30    STC   R3,SWEFULL+2        BLOCK NUMBER                                 
*                                                                               
         CLC   SWEFULL,SWEEOF      REACH THE ENDING DISK ADDR?                  
         BNL   SWEX20              YES - EXIT W/O READ/WRITE THIS BLOCK         
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,SWEFULL,SWEDUB,4,=C'TOG'                         
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ENQ   (MAJORNAM,SWEDUB,E,8)                                            
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,SWEFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         MVI   SWEWIT,0            ASSUME THIS BLOCK IS FINE                    
*                                                                               
         LA    R5,1                LOGICAL RECORD NUMBER                        
         L     R4,=A(EDICTBLK)                                                  
*                                                                               
SWE40    LR    RE,R5                                                            
         ICM   RE,14,SWEFULL                                                    
         CL    RE,SWEEOF           REACH THE ENDING DISK ADDR?                  
         BNL   SWEX                YES - EXIT AND WRITE BACK THIS BLOCK         
*                                                                               
         USING EDFILD,R4                                                        
         CLI   EDFMON,EDFMONPQ     IS THIS A 'PERMANENT' RECORD?                
         BE    SWE50               YES - SKIP IT                                
         CLI   EDFSTAT,EDFNOOP     IS THIS A NO-OP RECORD?                      
         BE    SWE50               YES - SKIP IT                                
         CLC   EDFMON,TODAY3+1     IS THIS RECORD FROM THIS MONTH?              
         BE    SWE50               YES - SKIP IT                                
*                                                                               
*                                  PRINT THE BAD ENTRY BEFORE NOOP IT           
         STCM  RE,15,WORK                                                       
         GOTO1 =V(HEXOUT),DMCB,WORK,P+30,4,=C'TOG'                              
         PRNT  BAD_EDICT_ENTRY,PRINT=ALWAYS                                     
         XC    DMCB+12(2),DMCB+12                                               
         MVC   DMCB+14(2),EDCTFRCL                                              
         GOTO1 =V(PRNTBL),DMCB,0,(R4),C'DUMP',,=C'1D'                           
*                                                                               
         MVI   EDFSTAT,EDFNOOP     MARK THIS ENTRY NOOP                         
         MVC   EDFMON,TODAY3+1     MOVE IN MONTH # ALSO                         
         AHI   R9,1                UPDATE NUMBER OF CORRUPTED ENTRIES           
         MVI   SWEWIT,1            THIS BLOCK HAS BAD EDICT RECORD(S)           
         DROP  R4                                                               
*                                                                               
SWE50    AH    R4,EDCTFRCL         BUMP TO NEXT RECORD                          
         LA    R5,1(R5)                                                         
         CH    R5,EDCTFRPB         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   SWE40                                                            
*                                                                               
* WRITE BACK THIS BLOCK AND DEQ THIS BLOCK                                      
*                                                                               
         CLI   SWEWIT,1            ANY BAD EDICT RECORD IN THIS BLOCK?          
         BNE   SWE60               NO - SKIP THE WRITE                          
         GOTO1 DATAMGR,DMCB,=C'DADDS',WTID,A(EDICTBLK),EDCTBKSZ,       +        
               EDICTFL,SWEFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                UNABLE TO WRITE EDICT FILE BLOCK             
SWE60    DEQ   (MAJORNAM,SWEDUB,8)                                              
*                                                                               
         LA    R3,1(R3)            NO                                           
         CH    R3,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   SWE30               YES                                          
*                                                                               
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
         BNH   SWE20                                                            
         DC    H'0'                THIS DAY'S PARTITION IS FULL                 
*                                                                               
**********************************************************************          
* WRITE BACK THIS LAST BLOCK AND DEQ THIS BLOCK                                 
**********************************************************************          
SWEX     CLI   SWEWIT,1            ANY BAD EDICT RECORD IN THIS BLOCK?          
         BNE   SWEX10              NO - SKIP THE WRITE                          
         GOTO1 DATAMGR,DMCB,=C'DADDS',WTID,A(EDICTBLK),EDCTBKSZ,       +        
               EDICTFL,SWEFULL,0                                                
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                UNABLE TO WRITE EDICT FILE BLOCK             
SWEX10   DEQ   (MAJORNAM,SWEDUB,8)                                              
                                                                                
**********************************************************************          
* EMAIL WITH THE DISK ADDRESS RANGE OF FILE CORRUPTION                          
**********************************************************************          
SWEX20   LTR   R9,R9               ANY BAD EDICT RECORD?                        
         BZ    SWEXX               NO - EXIT                                    
         GOTO1 =V(HEXOUT),DMCB,(R9),SWEMSG1A,4,=C'TOG'                          
         MVC   SWEMSG1H,PRNTTIME                                                
         MVC   SWEMSG1M,PRNTTIME+2                                              
         MVC   SWEMSG1S,PRNTTIME+4                                              
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('SWEMSG1Q',SWEMSG1)                      
*                                                                               
SWEXX    XIT1                                                                   
                                                                                
         LTORG                                                                  
SWEEOF   DS    F                                                                
SWEFULL  DS    F                                                                
SWEDUB   DS    D                                                                
SWECLK   DC    H'0'                SWEEP CLOCK, SWEEP ONLY WHEN 0               
SWECLKQ  EQU   10                  SWEEP EVERY 10TH TIME                        
SWEWIT   DC    X'00'               WRITE BACK THIS BLOCK FLAG                   
SWEMSG1  DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
SWEMSG1A DS    CL8                                                              
         DC    C' EDICT FILE ENTRIES ARE FOUND CORRUPTED AT '                   
SWEMSG1H DS    CL2                                                              
         DC    C':'                                                             
SWEMSG1M DS    CL2                                                              
         DC    C':'                                                             
SWEMSG1S DS    CL2                                                              
SWEMSG1Q EQU   *-SWEMSG1                                                        
         EJECT                                                                  
**********************************************************************          
* NTRY: R1 = XMTTABLE ENTRY                                                     
* EMAIL A WARNING MESSAGE WHEN A DUPLICATED ENTRY IS FOUND                      
**********************************************************************          
         USING XMTTABLD,R1                                                      
DUPWARN  NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         ICM   R0,3,XMTUSRID                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUPMGUID,DUB                                                     
*                                                                               
         MVC   DUPMGSUB,XMTSUBID                                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,XMTREFNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUPMGRPN,DUB                                                     
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,XMTLOGNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUPMGLGR,DUB                                                     
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,XMTDSTNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUPMGDST,DUB                                                     
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',('DUPMSGQ',DUPMSG)                        
         XIT1                                                                   
                                                                                
         LTORG                                                                  
DUPMSG   DS    0C                                                               
         DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
         DC    C'DUPLICATE ENTRY: '                                             
DUPMGUID DS    CL5                 USER ID #                                    
         DC    C','                                                             
DUPMGSUB DS    CL3                 REPORT SUB ID                                
         DC    C','                                                             
DUPMGRPN DS    CL5                 REPORT #                                     
         DC    C','                                                             
DUPMGLGR DS    CL5                 LOGICAL REPORT #                             
         DC    C','                                                             
DUPMGDST DS    CL5                 DESTINATION #                                
DUPMSGQ  EQU   *-DUPMSG                                                         
         EJECT                                                                  
**********************************************************************          
* PRINT USUAGE                                                                  
**********************************************************************          
PRTUSAGE NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2               PREPARE FOR DIVIDE                           
         L     R3,XMTTBNUM         CURRENT # ENTRY IN XMITTBL                   
         MHI   R3,100              * 100%                                       
         D     R2,XMTTBMAX         / MAX # ENTRY IN XMITTBL                     
         EDIT  (R3),(3,P+30),ZERO=NOBLANK                                       
         PRNT  XMITTBL_%_USAGE,PRINT=ALWAYS                                     
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* IF BYTE = 'F', REPORT WAS FOUND IN THE PRINT QUEUE:                           
*                 SET WAITING BIT, STAMP TIME, BUMP TO NEXT RECORD.             
*                BYTE2='FF' MEANS LAST DEST. IN THIS LOGICAL REPORT.            
*    BYTE = 'U', REPORT IS UNSENDABLE:                                          
*                 SET CAN'T SEND BIT, BUMP TO NEXT RECORD.                      
*                                                                               
*    XACTDATA+0:  SYSTEM                                                        
*    XACTDATA+1:  TYPE/SUBTYPE                                                  
*    XACTDATA+4:  58-BYTE APPLICATION AREA                                      
*                                                                               
*    BYTE = 'P', WE'VE JUST MARKED THE REPORT PRINTED IN PRINT QUEUE:           
*                 REMEMBER THAT TRIVAL BUT CRITICAL FACT.                       
*                 XMTENTRY HAS THE TABLE ENTRY TO BE MARKED PRINTED.            
*                                                                               
*    BYTE = 'G', GET THE EDICT FILE ENTRY:                                      
*                 XMTENTRY HAS THE TABLE ENTRY TO BE MARKED.                    
*                 RETURN EDICT FILE ENTRY IN EDFENTRY.                          
*                                                                               
**********************************************************************          
POSTEDFL NTR1  BASE=*,LABEL=*                                                   
         CLI   BYTE,C'P'           JUST MARKED REPORT PRINTED?                  
         BE    POSTE100                                                         
         CLI   BYTE,C'G'           JUST GET EDICT FILE ENTRY?                   
         BE    POSTE100                                                         
*                                                                               
         CLI   BYTE,C'U'           UNSENDABLE?                                  
         BE    POSTE05                                                          
         CLI   BYTE,C'F'           FOUND IN PRINT QUEUE?                        
         JNE   *+2                                                              
*                                                                               
         CLI   ISPDF,1                                                          
         BNE   POSTE00                                                          
*                                                                               
         CLI   RPTMETH,EDIPDFQ     PDF                                          
         BNE   POSTE00                                                          
         CLI   XMITPDF,C'N'                                                     
         BE    POSTE03             SKIP REPORT                                  
*                                                                               
POSTE00  OC    METHINCL,METHINCL   ANY + METHOD FILTER?                         
         BZ    POSTE01X            NO                                           
         LA    RF,METHINCL                                                      
         LA    RE,MTHPMAXQ                                                      
POSTE01  OC    0(L'METHINCL,RF),0(RF) ANY MORE?                                 
         BZ    POSTE01X            NO                                           
         CLC   RPTMETH,0(RF)       DOES THIS REPORT MATCH FILTER?               
         BE    POSTE05             YES - ALWAYS ADD THIS METHOD                 
         AHI   RF,L'METHINCL                                                    
         BCT   RE,POSTE01          TRY NEXT FILTER                              
*                                                                               
POSTE01X OC    METHXCLD,METHXCLD   ANY - METHOD FILTER?                         
         BZ    POSTE02X            NO                                           
         LA    RF,METHXCLD                                                      
         LA    RE,MTHNMAXQ                                                      
POSTE02  OC    0(L'METHXCLD,RF),0(RF) ANY MORE?                                 
         BZ    POSTE02X            NO                                           
         CLC   RPTMETH,0(RF)       DOES THIS REPORT MATCH FILTER?               
         BE    POSTE03             YES - NEVER ADD THIS METHOD, EXIT            
         AHI   RF,L'METHXCLD                                                    
         BCT   RE,POSTE02          TRY NEXT FILTER                              
*                                                                               
POSTE02X CLI   SKIPTHIS,C'Y'       SKIP THIS REPORT?                            
         BNE   POSTE05             NO - ADD IT                                  
*                                                                               
POSTE03  MVC   P+30(8),RPTUID                                                   
         MVI   P+38,C','                                                        
         MVC   P+39(3),RPTSUBID                                                 
         MVI   P+42,C','                                                        
         SR    R0,R0                                                            
         ICM   R0,3,RPTREFNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+43(5),DUB                                                      
         PRNT  REPORTSKIPPED,PRINT=ALWAYS                                       
         CLI   RPTMETH,EDIBXFOQ    BXF                                          
         BNE   POSTEX                                                           
*                                                                               
         CLI   RPTPM360,C'N'       IF SET TO N, THEN MARK PRINTED               
         BNE   POSTEX                                                           
         XC    WORK,WORK                                                        
         USING UKRECD,RF                                                        
         LA    RF,WORK             BUILD PRINT QUEUE INDEX                      
         MVC   UKSRCID,RPTUIDNO                                                 
         MVC   UKSUBID,RPTSUBID                                                 
         MVC   UKREPNO,RPTREFNO                                                 
*        BRAS  RE,MRKPRTND                                                      
         B     POSTEX                                                           
         DROP  RF                                                               
*                                                                               
POSTE05  EQU   *                                                                
         LA    R4,WORK             BUILD AN EDICT RECORD                        
         USING EDFILD,R4                                                        
         LH    R1,EDCTFRCL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)       CLEAR THE RECORD                             
         MVC   EDFSYS,XACTDATA     SYSTEM                                       
         MVC   EDFTYPE,XACTDATA+1  RECORD TYPE                                  
         MVC   EDFMETH,RPTMETH     LOGICAL REPORT TRANSMISSION METHOD           
         MVC   EDFMON,TODAY3+1     MONTH                                        
         MVC   EDFDSTTY,DESTTYPE   DESTINATION TYPE                             
         MVC   EDFPQTYP,RPTPQTYP   PQ REPORT TYPE                               
         MVC   EDFPQUID,RPTUIDNO   USERID NUMBER                                
         MVC   EDFPQSUB,RPTSUBID   REPORT SUB-ID                                
         MVC   EDFPQREF,RPTREFNO   REFERENCE NUMBER                             
         MVC   EDFPQDAT,RPTCRDAT   REPORT CREATION DATE - CMPRSD                
         MVC   EDFPQTIM,RPTCRTIM   REPORT CREATION TIME                         
         MVC   EDFPQSEQ,RPTLOGNO   LOGICAL REPORT SEQUENCE NUMBER               
         MVC   EDFPQDSS,RPTLDSTS   LOGICAL REPORT DESTINATION SEQUENCE          
         MVC   EDFEZBOX,EZMAILBX   EASYLINK MAILBOX NUMBER                      
         MVC   EDFFLAGS,RPTFLAGS   VARIOUS FLAGS                                
         MVC   EDFAPPL,XACTDATA+4  APPLICATION AREA                             
         MVC   EDFMQAPP,MQAPP      MQDEF APPLICATION CODE                       
         MVC   EDFMQID,MQID        MQDEF QUEUE #                                
         MVC   EDFMQLAB,MQLAB      MQ MESSAGE LABEL FOR FACPAK                  
         MVC   EDFCPQCL,PQSCL      PQ CLASS AFTER SENT                          
         MVC   EDFCPQST,PQSST      PQ STATUS AFTER SENT                         
*                                                                               
         CLI   EDFMETH,C'P'        BDE-FTP?                                     
         BNE   *+10                                                             
         MVC   EDFBDFIL,FILENAME   FILE NAME                                    
*                                                                               
         CLI   EDFMETH,C'O'        PDF?                                         
         BNE   *+10                                                             
         MVC   EDFAPPL,UIDNO       UNIQUE ID NUMBER                             
*                                                                               
         MVC   EDFDEST,DESTINAT    START WITH UNFORMATTED DESTINATION           
         CLI   DESTFMT,C' '                                                     
         BNH   *+10                                                             
         MVC   EDFDEST,DESTFMT     WE WERE GIVEN THE FORMATTED DEST.            
*                                                                               
         CLI   BYTE,C'U'           UNSENDABLE?                                  
         BNE   POSTE20                                                          
         TM    EDFSTAT,EDFSTSNT+EDFSTRCV+EDFSTCAN+EDFSTJNK+EDFSTPRT+EDF+        
               STWTG+EDFSTLST                                                   
         BZ    POSTE10             DIDN'T TOUCH THIS ONE YET                    
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(18),=C'MARKING UNSENDABLE'                                  
         GOTO1 =V(PRINTER)                                                      
POSTE10  OI    EDFSTAT,EDFSTJNK    SET UNSENDABLE FLAG                          
         MVC   EDFERROR,RPTERROR   ERROR REASON CODE                            
         B     POSTE40                                                          
*                                                                               
POSTE20  TM    EDFSTAT,EDFSTSNT+EDFSTRCV+EDFSTCAN+EDFSTJNK+EDFSTPRT+EDF+        
               STWTG+EDFSTLST                                                   
         BZ    POSTE30             DIDN'T TOUCH THIS ONE YET                    
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(13),=C'MARKING FOUND'                                       
         GOTO1 =V(PRINTER)                                                      
POSTE30  OI    EDFSTAT,EDFSTWTG    SET WAITING FLAG                             
         CLI   BYTE2,X'FF'         LAST DEST. IN A LOGICAL REPORT?              
         BNE   *+8                 NO                                           
         OI    EDFSTAT,EDFSTLST    MARK LAST DEST. IN A LOGICAL REPORT          
*                                                                               
POSTE40  PRNT  ADDEDICTRECORD                                                   
         CLI   TRACEFLG,C'Y'                                                    
         BNE   POSTE45                                                          
         XC    DMCB+12(2),DMCB+12                                               
         MVC   DMCB+14(2),EDCTFRCL                                              
         GOTO1 =V(PRNTBL),DMCB,0,WORK,C'DUMP',,=C'1D'                           
POSTE45  GOTO1 =A(EDICTADD),DMCB,MAJORNAM,WORK,0                                
         MVC   FULL,DMCB+8                                                      
         GOTO1 =V(HEXOUT),DMCB,FULL,P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PRNT  EDICTRECORDADDED,PRINT=ALWAYS                                    
*                                                                               
         XC    CARD,CARD                                                        
         LA    R2,CARD                                                          
         USING XMTTABLD,R2                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,EDFPQUID       PQ REPORT SENDING USERID                     
         STCM  R1,3,XMTUSRID                                                    
         D     R0,NUMPQS                                                        
         AHI   R0,1                                                             
         STC   R0,XMTPRTQ          PRINT QUEUE NUMBER                           
         MVI   XMTSOURC,XMTSRCPQ   PRINT QUEUE REPORT                           
         MVC   XMTSUBID,EDFPQSUB   PQ REPORT SUB-ID                             
         MVC   XMTREFNO,EDFPQREF   PQ REPORT REFERENCE NUMBER                   
         MVC   XMTCRDAT,EDFPQDAT   PQ REPORT CREATION DATE - CMPRSD             
         MVC   XMTCRTIM,EDFPQTIM   PQ REPORT CREATION TIME                      
         MVC   XMTLOGNO,EDFPQSEQ   PQ REPORT LOGICAL REPORT NUMBER              
         MVC   XMTDSTNO,EDFPQDSS   PQ REPORT LOGICAL REPORT DEST. NUM.          
         MVC   XMTSTAT,EDFSTAT     REPORT STATUS                                
         MVC   XMTMETH,EDFMETH     METHOD OF TRANSMISSION                       
         MVC   XMTSYS,EDFSYS       SYSTEM                                       
         MVC   XMTTYPE,EDFTYPE     RECORD TYPE                                  
         MVC   XMTDSKAD(3),FULL    TTTTBB                                       
         MVC   XMTDSKAD+3(1),FULL+3 LOGICAL RECORD NUMBER                       
         OC    EDFCPQCL,EDFCPQCL                                                
         BZ    *+8                                                              
         OI    XMTFLAGS,XMTCPQSQ                                                
         OC    EDFCPQST,EDFCPQST                                                
         BZ    *+8                                                              
         OI    XMTFLAGS,XMTCPQSQ                                                
         DROP  R2                                                               
*                                                                               
         LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,(9),E,8)  ENQUEUE THE TRANSMIT TABLE                   
*                                                                               
         SAM31                                                                  
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               RF = A(ENTRIES#)                             
         MVC   DMCB+8(4),0(RF)     NUMBER OF ENTRIES IN TABLE                   
         GOTO1 =V(BINSRCH),DMCB,CARD,AXMTTBL,,(1,XMTTBLQ),XMTKEYQ,     +        
               XMTTBMAX                                                         
         L     RF,AXMTTBL                                                       
         AHI   RF,-8               NUMBER OF ENTRIES IN TABLE                   
         MVC   0(4,RF),DMCB+8      UPDATE TABLE SIZE                            
         SAM24                                                                  
*                                                                               
         TM    DMCB,X'80'          WAS THIS REPORT ALREADY IN TABLE?            
         BO    *+16                                                             
         LA    R1,CARD                                                          
         BRAS  RE,DUPWARN          RECORD IS DUPLICATED IN EDICT FILE           
         B     POSTE48                                                          
*                                                                               
         NI    DMCB,X'7F'          TURN OFF HIGH-ORDER BIT, THEN. . .           
         OC    DMCB(4),DMCB        . . . MAKE SURE THE RECORD WENT IN           
         BNZ   *+6                                                              
         DC    H'0'                XMITTBL IS NOT LARGE ENOUGH                  
         MVC   XMTTBNUM,DMCB+8     UPDATE TABLE SIZE                            
*                                                                               
POSTE48  LA    R9,=C'XMTTABLE'     MINOR RESOURCE NAME                          
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,(9),8)    DEQUEUE THE TRANSMIT TABLE                   
*                                                                               
         CLI   BYTE,C'F'           REPORT WAS MARKED FOUND?                     
         BNE   POSTE90             NO                                           
*                                                                               
         CLI   EDFMETH,C'E'        EASYLINK?                                    
         BNE   POSTE50                                                          
         TM    ELOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ELOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE50  CLI   EDFMETH,C'Q'        NFX?                                         
         BNE   POSTE55                                                          
         TM    QLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  QLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE55  CLI   EDFMETH,C'A'        ADVANTIS?                                    
         BNE   POSTE60                                                          
         TM    ALOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ALOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE60  CLI   EDFMETH,C'J'        NJE?                                         
         BNE   POSTE65                                                          
         TM    JLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  JLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE65  CLI   EDFMETH,C'D'        DARE?                                        
         BNE   POSTE70                                                          
         TM    DLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  DLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE70  CLI   EDFMETH,C'F'        FTP?                                         
         BNE   POSTE75                                                          
         TM    FLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  FLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE75  CLI   EDFMETH,C'X'        FAXGATE?                                     
         BNE   POSTE77                                                          
         TM    XLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  XLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE77  CLI   EDFMETH,C'C'        COLUMBINE?                                   
         BNE   POSTE79                                                          
         TM    CLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  CLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE79  CLI   EDFMETH,C'B'        BIAS?                                        
         BNE   POSTE80                                                          
         TM    BLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  BLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE80  CLI   EDFMETH,C'O'        PDF?                                         
         BNE   POSTE81                                                          
         TM    OLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  OLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE81  CLI   EDFMETH,C'T'        BDE-EMAIL?                                   
         BNE   POSTE83                                                          
         TM    TLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  TLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE83  CLI   EDFMETH,C'P'        BDE-FTP?                                     
         BNE   POSTE87                                                          
         TM    PLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE83T            YES -- DON'T BOTHER TO POST AGAIN            
         POST  PLOOKECB                                                         
*                                                                               
POSTE83T CLI   BDF2,C'Y'                                                        
         BNE   POSTE90                                                          
         TM    TLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  TLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE87  CLI   EDFMETH,C'Z'        ENCODA?                                      
         BNE   POSTE88                                                          
         TM    ZLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  ZLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE88  CLI   EDFMETH,C'M'        E-MAIL?                                      
         BNE   POSTE90                                                          
         TM    MLOOKECB,X'40'      ECB ALREADY POSTED?                          
         BO    POSTE90             YES -- DON'T BOTHER TO POST AGAIN            
         POST  MLOOKECB                                                         
         B     POSTE90                                                          
*                                                                               
POSTE90  B     POSTEX                                                           
                                                                                
POSTE100 LA    R3,XMTENTRY                                                      
         USING XMTTABLD,R3                                                      
         MVC   FULL,XMTDSKAD       DISK ADDRESS OF ENTRY                        
         MVC   FULL2(3),FULL       TTTTBB (NO LOGICAL RECORD NUMBER)            
         MVI   FULL2+3,0                                                        
         GOTO1 =V(HEXOUT),DMCB,FULL2,MINORNAM,4,=C'TOG'                         
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),MINORNAM                                                 
         PRNT  ENQUEUE                                                          
*                                                                               
         ENQ   (MAJORNAM,MINORNAM,E,8)                                          
*                                                                               
* EDICT FILE BLOCK IS NOW ENQUEUED EXCLUSIVELY                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',RDID,A(EDICTBLK),0,              +        
               EDICTFL,FULL2,0                                                  
         OC    12(2,R1),12(R1)                                                  
         BZ    POSTE110                                                         
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(30),=C'EDICT FILE DISK ADDRESS IS BAD'                      
         GOTO1 =V(PRINTER)                                                      
         B     POSTE120            IGNORE THIS ONE                              
*                                                                               
POSTE110 ZIC   R1,FULL+3           LOGICAL RECORD NUMBER                        
         BCTR  R1,0                                                             
         MH    R1,EDCTFRCL         OFFSET INTO BLOCK OF THIS RECORD             
         L     R4,=A(EDICTBLK)                                                  
         AR    R4,R1               A(RECORD WITHIN BLOCK)                       
         USING EDFILD,R4                                                        
*                                                                               
         CLC   EDFPQUID,XMTUSRID   ARE WE AT THE CORRECT ENTRY?                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   EDFPQSUB,XMTSUBID                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   EDFPQREF,XMTREFNO                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   EDFPQTIM,XMTCRTIM                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   EDFPQSEQ,XMTLOGNO                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   EDFPQDSS,XMTDSTNO                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,=A(EDFENTRY)                                                  
         MVC   0(L'EDFENTRY,RE),0(R4)   SAVE THIS ENTRY                         
         CLI   BYTE,C'G'           JUST GET EDICT FILE ENTRY?                   
         BE    POSTE120            YES - EXIT                                   
*                                                                               
         TM    EDFSTAT,EDFSTSNT+EDFSTJNK   YES -- LOOK AT REPORT STATUS         
         BNZ   *+6                                                              
         DC    H'0'                REPORT MUST HAVE BEEN SENT OR TOSSED         
         OI    EDFSTAT,EDFSTPRT    REPORT HAS BEEN MARKED PRINTED               
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DADDS',WTID,A(EDICTBLK),EDCTBKSZ,       +        
               EDICTFL,FULL2,0                                                  
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
POSTE120 MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(8),MINORNAM                                                 
         PRNT  DEQUEUE                                                          
*                                                                               
         DEQ   (MAJORNAM,MINORNAM,8)                                            
*                                                                               
* EDICT FILE BLOCK IS NOW DEQUEUED                                              
*                                                                               
POSTEX   XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS SINGLE-THREADED ROUTINE ADDS RECORDS TO THE END OF THE EDICT             
* TRANSACTION FILE.  IT ENQUEUES ITSELF, ENQUEUES A BLOCK, WRITES A             
* BLOCK, DEQUEUES THE BLOCK, THEN DEQUEUES ITSELF.                              
***********************************************************************         
EDICTADD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R2,R1               A(PARAMETERS)                                
         USING EADDPRMD,R2                                                      
*                                                                               
         L     R8,EADDAMAJ         MAJOR RESOURCE NAME                          
         LA    R9,=C'EDICTADD'     MINOR RESOURCE NAME                          
         ENQ   ((8),(9),E,8)       ENQUEUE MYSELF                               
*                                                                               
* THIS SUBROUTINE IS NOW ENQUEUED EXCLUSIVELY                                   
*                                                                               
         L     R3,=A(EDICTFL)      A(EDICT FILE DTF)                            
*                                                                               
         OC    PREVEEOF,PREVEEOF                                                
         BZ    EDCTAD10            NO PREV EOF, SKIP THE CHECKING               
*                                                                               
         XC    FULL1,FULL1                                                      
         MVC   FULL1(3),PREVEEOF   TTTTBB00                                     
*                                                                               
         GOTO1 =V(HEXOUT),DMCB1,FULL1,DUB1,4,=C'TOG'                            
         CLC   =F'8',DMCB1+16                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ENQ/DEQ EDICT FILE BLOCK FOR READING PREV EOF ENTRY                           
*                                                                               
         ENQ   ((8),DUB1,E,8)                                                   
         GOTO1 =V(DATAMGR),DMCB1,=C'DADDS',RDID,EDCTBLK,0,(R3),FULL1,0          
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                UNABLE TO READ EDICT FILE BLOCK              
         DEQ   ((8),DUB1,8)                                                     
*                                                                               
         ZIC   R1,PREVEEOF+3       LOGICAL RECORD NUMBER                        
         BCTR  R1,0                                                             
         MH    R1,EDICTRCL         OFFSET INTO BLOCK OF THIS RECORD             
         LA    R4,EDCTBLK(R1)      A(RECORD WITHIN BLOCK)                       
         USING EDFILD,R4                                                        
*                                                                               
         L     RF,EADDAREC                                                      
         CLC   EDFMON,EDFMON-EDFILD(RF)                                         
         BE    EDCTAD10                                                         
         L     R0,PREVEEOF         R0 = DISK ADDRESS OF THIS ENTRY              
         DC    H'0'                MONTH # MUST MATCH                           
         DROP  R4                                                               
*                                                                               
EDCTAD10 XC    FULL1,FULL1                                                      
         MVC   FULL1(3),EDICTEOF   TTTTBB00                                     
*                                                                               
         GOTO1 =V(HEXOUT),DMCB1,FULL1,DUB1,4,=C'TOG'                            
         CLC   =F'8',DMCB1+16                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TIME  DEC                                                              
         L     RF,EADDAREC                                                      
         ST    R1,EDFADATE-EDFILD(RF)           DATE ADDED                      
         ST    R0,EDFATIME-EDFILD(RF)           TIME ADDED                      
*                                                                               
         ENQ   ((8),DUB1,E,8)                                                   
*                                                                               
* EDICT FILE BLOCK IS NOW ENQUEUED EXCLUSIVELY                                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB1,=C'DADDS',RDID,EDCTBLK,0,(R3),FULL1,0          
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                UNABLE TO READ EDICT FILE BLOCK              
*                                                                               
         ZIC   R1,EDICTEOF+3       LOGICAL RECORD NUMBER                        
         BCTR  R1,0                                                             
         MH    R1,EDICTRCL         OFFSET INTO BLOCK OF THIS RECORD             
         LA    R4,EDCTBLK(R1)      A(RECORD WITHIN BLOCK)                       
*                                                                               
         L     RF,EADDAREC                                                      
         LH    R1,EDICTRCL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RF)       MOVE IN THE CALLER'S RECORD                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB1,=C'DADDS',WTID,EDCTBLK,EDICTBSZ,(R3), +        
               FULL1,0                                                          
         OC    12(2,R1),12(R1)                                                  
         BZ    *+6                                                              
         DC    H'0'                UNABLE TO WRITE EDICT FILE BLOCK             
*                                                                               
         DEQ   ((8),DUB1,8)                                                     
*                                                                               
* EDICT FILE BLOCK IS NOW DEQUEUED                                              
*                                                                               
         MVC   EDSKADDR,EDICTEOF   RETURN DISK ADDRESS TO CALLER                
         MVC   PREVEEOF,EDICTEOF   SAVE THIS DISK ADDRESS                       
*                                                                               
         ZIC   R1,EDICTEOF+3       CURRENT LOGICAL RECORD NUMBER                
         CH    R1,EDICTRPB         ANY MORE ROOM ON THIS BLOCK?                 
         BNL   *+16                                                             
         LA    R1,1(R1)            YES -- INCREMENT RECORD NUMBER               
         STC   R1,EDICTEOF+3                                                    
         B     EDCTAD20                                                         
*                                                                               
         MVI   EDICTEOF+3,1        START FROM LOGICAL RECORD 1                  
         ZIC   R1,EDICTEOF+2       CURRENT BLOCK NUMBER                         
         L     RE,=A(EDCTFRPT)                                                  
         LH    RE,0(RE)            RE = NUMBER OF RECORDS/TRACK                 
         CR    R1,RE               ANY MORE ROOM ON THIS TRACK?                 
         BNL   *+16                                                             
         LA    R1,1(R1)            YES -- INCREMENT BLOCK NUMBER                
         STC   R1,EDICTEOF+2                                                    
         B     EDCTAD20                                                         
*                                                                               
         MVI   EDICTEOF+2,1        START WITH BLOCK NUMBER 1                    
         SR    R1,R1                                                            
         ICM   R1,3,EDICTEOF       CURRENT TRACK NUMBER                         
         CLM   R1,3,EDICTLST       ANY MORE TRACKS AVAILABLE FOR TODAY?         
         BL    *+6                                                              
         DC    H'0'                NO -- EDICTFIL IS NOT BIG ENOUGH             
         LA    R1,1(R1)                                                         
         STCM  R1,3,EDICTEOF       YES -- INCREMENT TRACK NUMBER                
*                                                                               
EDCTAD20 L     R1,NUMREPS          INCREMENT NUMBER OF REPORTS TODAY            
         LA    R1,1(R1)                                                         
         ST    R1,NUMREPS                                                       
*                                                                               
         LA    R9,=C'EDICTADD'     MINOR RESOURCE NAME                          
         DEQ   ((8),(9),8)         DEQUEUE MYSELF                               
*                                                                               
* THIS SUBROUTINE IS NOW DEQUEUED                                               
*                                                                               
         XIT1                                                                   
         DROP  R2,RB                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
DUB1     DS    D                                                                
FULL1    DS    F                                                                
DMCB1    DS    10F                                                              
NUMREPS  DS    F                   NUMBER OF LOGICAL REPORTS FOUND              
EDICTBSZ DS    A                   EDICT FILE PHYSICAL RECORD LENGTH            
PREVEEOF DC    F'0'                EDICT FILE DISK ADDRESS(BEFORE EOF)          
EDICTEOF DS    F                   EDICT FILE DISK ADDRESS                      
EDICTRPB DS    H                   EDICT FILE RECORDS/BLOCK                     
EDICTRCL DS    H                   EDICT FILE LOGICAL RECORD LENGTH             
EDICTLST DS    H                   EDICT FILE LAST TRACK NUM FOR TODAY          
EDCTBLK  DS    18432X              EDICT FILE BLOCK                             
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
         USING *,RF                                                             
EXTRTN   C     R0,=F'12'           TEST SDWA ACQUIRED                           
         BNE   *+8                 NO, JUST LET MVS PERC                        
         SR    RF,RF                                                            
         BR    RE                                                               
*                                                                               
         STM   RE,RC,12(RD)        SAVE CALLING REGS                            
         ST    RD,ESTAERD          AND POINTER                                  
         ST    R1,ESTAER1          AND POINTER TO SDWA                          
*                                                                               
         USING SDWA,R1                                                          
         MVC   GPS(64),SDWAGRSV    R0-RF AT ABEND                               
         DROP  R1                                                               
*                                                                               
         L     RF,=A(EXT00)                                                     
         BR    RF                                                               
         DROP  RF                                                               
         LTORG                                                                  
*                                                                               
         USING *,RB                                                             
EXT00    LR    RB,RF                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         L     RA,=V(CPRINT)                                                    
         L     RD,=A(REGSAVE)      TEMP REGISTER SAVE AREA (100D)               
*                                                                               
         PRNT  ESTAE_ACTIVATED                                                  
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS5,OPMS5)                       
*                                                                               
         L     R1,ESTAER1          POINTER TO SDWA                              
         USING SDWA,R1                                                          
         CLC   OPERCANC,SDWACMPC   TEST OPERATOR CANCEL                         
         BE    NORETRY                                                          
         DROP  R1                                                               
*                                                                               
         LA    R1,RETTAB                                                        
EXT10    CLC   GPS+44(4),0(R1)     COMPARE RB AT ABEND TO ROUNTINE ADR          
         BNE   *+10                                                             
         L     RE,4(R1)            ADR(RETRY/CLEANUP/ABEND)                     
         BR    RE                                                               
         AHI   R1,RETTABLQ                                                      
         OC    0(4,R1),0(R1)       END OF TABLE                                 
         BNZ   EXT10               NO, CONTINUE                                 
*                                                                               
NORETRY  L     RD,ESTAERD                                                       
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
CLEANUP  L     R3,=A(CLEAN)                                                     
         SETRP DUMP=YES,RC=4,RETADDR=(3),FRESDWA=YES                            
*                                                                               
         USING *,RB                                                             
CLEAN    LR    RB,RF                                                            
*                                  DEQ ALL RESOURCES                            
         SR    R3,R3                                                            
         LA    R2,ENQTABLE                                                      
CLEAN10  ICM   R8,15,0(R2)         MAJOR NAME                                   
         BZ    CLEAN30                                                          
         ICM   R9,15,4(R2)         MINOR NAME                                   
         IC    R3,8(R2)            MINOR NAME LENGTH                            
*                                                                               
         DEQ   ((8),(9),(3)),RET=HAVE                                           
         LTR   RF,RF                                                            
         BNZ   CLEAN20                                                          
         MVC   P+30(8),0(R8)                                                    
         MVC   P+40(8),0(R9)                                                    
         PRNT  DEQUEUE                                                          
*                                                                               
CLEAN20  AHI   R2,ENQTABLQ                                                      
         B     CLEAN10                                                          
*                                                                               
CLEAN30  BRAS  RE,SHUTDOWN         DETACH ALL SUBTASKS                          
         L     RD,SAVERD           EDICT REGISTER SAVE AREA                     
         XBASE                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
ESTAERD  DC    F'0'                                                             
ESTAER1  DC    F'0'                                                             
GPS      DS    16F                                                              
OPERCANC DC    X'222000'                                                        
*                                                                               
ENQTABLQ EQU   ENQTAB2-ENQTABLE                                                 
ENQTABLE DS    0C                                                               
         DC    AL4(MAJORNAM),AL4(*+5),AL1(8),CL8'XMTTABLE'                      
ENQTAB2  DC    AL4(MAJORNAM),AL4(*+5),AL1(5),CL8'PRTQU'                         
         DC    AL4(MAJORNAM),AL4(*+5),AL1(8),CL8'DSTTABLE'                      
         DC    AL4(MAJORNAM),AL4(*+5),AL1(6),CL8'CTFILE'                        
         DC    AL4(MAJORNAM),AL4(*+5),AL1(7),CL8'LUTABLE'                       
         DC    AL4(MAJORNAM),AL4(MINORNAM),AL1(8),CL8' '                        
         DC    AL4(MAJORNAM),AL4(*+5),AL1(8),CL8'EDICTADD'                      
         DC    AL4(MAJORNAM),AL4(DUB1),AL1(8),CL8' '                            
         DC    AL4(0)                                                           
*                                                                               
RETTABLQ EQU   RETTAB2-RETTAB                                                   
RETTAB   DS    0F                                                               
         DC    A(EDICT),A(NORETRY)                                              
RETTAB2  DC    A(SHUTDOWN),A(NORETRY)                                           
         DC    A(BLDPNTAB),A(NORETRY)                                           
         DC    A(INITIAL),A(NORETRY)                                            
         DC    A(FINDREPS),A(NORETRY)                                           
         DC    A(ATTACH),A(CLEANUP)                                             
         DC    A(DETACH),A(NORETRY)                                             
         DC    A(PURGEOLD),A(CLEANUP)                                           
         DC    A(READCRDS),A(NORETRY)                                           
         DC    A(WAITABIT),A(CLEANUP)                                           
         DC    A(BLDPQTAB),A(NORETRY)                                           
         DC    A(BLDDEST),A(NORETRY)                                            
         DC    A(BLDLUTAB),A(NORETRY)                                           
         DC    A(XFERREP),A(CLEANUP)                                            
         DC    A(READ1RPT),A(CLEANUP)                                           
         DC    A(CHKOPER),A(CLEANUP)                                            
         DC    A(STATUS),A(CLEANUP)                                             
         DC    A(TOGGLE),A(CLEANUP)                                             
         DC    A(COLSTAT),A(CLEANUP)                                            
         DC    A(FAXGSTAT),A(CLEANUP)                                           
         DC    A(REATTACH),A(CLEANUP)                                           
         DC    A(OPENEDFL),A(NORETRY)                                           
         DC    A(PRTUSAGE),A(NORETRY)                                           
         DC    A(POSTEDFL),A(CLEANUP)                                           
         DC    A(EDICTADD),A(CLEANUP)                                           
         DC    F'0'                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*REGSAVE'                                                      
REGSAVE  DS    100D                                                             
*                                                                               
*                                                                               
COMMWORK DS    0D                  COMMON STORAGE AREA                          
*                                                                               
EDICTFL  DMDA                                                                   
*                                                                               
         DS    0F                  USED FOR LARL INSTRUCTION                    
NOOPREPS DC    CL40' '                                                          
REPLIST  DC    16X'FF'                                                          
*                                                                               
MYSPACES DC    CL256' '                                                         
OPMS1    DC    C'XXX TRANSMISSIONS XXXXXXXX'                                    
OPMS2    DC    C'XXX TRANSMISSIONS XXXXXXXX, XXXXXX WAITING'                    
OPMS3    DC    C'TRANSMISSIONS TO COLUMBINE STATION XXXXXXXX XXXXXXXX'          
OPMS4    DC    C'XXXXXXXXXXXXX FAXGATE MACHINE XXXXXXXX XXXXXXXXXXX'            
OPMS5    DC    C'ESTAE ACTIVATED'                                               
OPMS6    DC    C'UNKNOWN USERID ERROR:                  '                       
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
PARM     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
FULL2    DS    F                                                                
FULL3    DS    F                                                                
SAVERD   DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
ISEMAIL  DS    X                                                                
ISBDE    DS    X                                                                
ISPDF    DS    X                                                                
*                                                                               
WORK     DS    CL256                                                            
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
BINSQ    EQU   (X'FFFFFF'/XMTTBLQ) MAX CAPACITY OF BINSRCH (24-BIT ADR)         
*                                                                               
DATAMGR  DC    V(DATAMGR)                                                       
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ACOMM    DS    A                   A(COMMUNICATIONS PARAMETER LIST)             
AXMTTBL  DS    A                   A(TABLE OF REPORTS)                          
ADESTTBL DS    A                   A(TABLE OF DESTINATIONS, EDICT REC)          
ADESTTBX DS    A                   A(END OF DESTINATION TABLE)                  
*INCREASE THE TABLE SIZE FROM 3000 TO 3500                                      
DESTTBLL EQU   3500                MAXIMUM NAMES SUPPORTED                      
APQSAVE  DS    A                   A(PRINT QUEUE SAVE AREA)                     
AFXTABLE DS    A                   A(FAXGATE APPC/MVS CONTROL TABLE)            
*                                                                               
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    X'00',AL3(NEWDYECB) NEW DAY START (DDS TIME)                     
         DC    X'80',AL3(TIMERECB) TIMER POP                                    
*                                                                               
         DS    0D                                                               
EASYPARM DS    XL(SUBPARML)        EASYLINK SUBTASK PARAMETER LIST              
NFXPARMS DS    XL(SUBPARML)        NFX SUBTASK PARAMETER LIST                   
MQMPARMS DS    XL(SUBPARML)        MQM SUBTASK PARAMETER LIST                   
BDEPARMS DS    XL(SUBPARML)        BDE-EMAIL SUBTASK PARAMETER LIST             
BDFPARMS DS    XL(SUBPARML)        BDE-FTP SUBTASK PARAMETER LIST               
ENCPARMS DS    XL(SUBPARML)        ENCODA SUBTASK PARAMETER LIST                
NJEPARMS DS    XL(SUBPARML)        NJE SUBTASK PARAMETER LIST                   
FTPPARMS DS    XL(SUBPARML)        FTP SUBTASK PARAMETER LIST                   
DAREPARM DS    XL(SUBPARML)        DARE SUBTASK PARAMETER LIST                  
COLPARMS DS    XL(SUBPARML)        COLUMBINE SUBTASK PARAMETER LIST             
ADVNPARM DS    XL(SUBPARML)        ADVANTIS SUBTASK PARAMETER LIST              
FAXGPARM DS    XL(SUBPARML)        FAXGATE SUBTASK PARAMETER LIST               
BIASPARM DS    XL(SUBPARML)        BIAS SUBTASK PARAMETER LIST                  
PDFSPARM DS    XL(SUBPARML)        PDF SUBTASK PARAMETER LIST                   
EASYTCB  DS    F                   TCB FOR EASYLINK SUBTASK                     
EASYECB  DS    F                   ECB FOR EASYLINK SUBTASK                     
ESTPECB  DS    F                   POST TO STOP EASYLINK TRANSMISSIONS          
ELOOKECB DC    F'0'                POST TO WAKE UP EASYLINK SUBTASK             
NFXTCB   DS    F                   TCB FOR NFX SUBTASK                          
NFXECB   DS    F                   ECB FOR NFX SUBTASK                          
QSTPECB  DS    F                   POST TO STOP NFX TRANSMISSIONS               
QLOOKECB DC    F'0'                POST TO WAKE UP NFX SUBTASK                  
ENCTCB   DS    F                   TCB FOR ENCODA SUBTASK                       
ENCECB   DS    F                   ECB FOR ENCODA SUBTASK                       
ZSTPECB  DS    F                   POST TO STOP ENCODA TRANSMISSIONS            
ZLOOKECB DC    F'0'                POST TO WAKE UP ENCODA SUBTASK               
BDETCB   DS    F                   TCB FOR BDE-EMAIL SUBTASK                    
BDEECB   DS    F                   ECB FOR BDE-EMAIL SUBTASK                    
TSTPECB  DS    F                   POST TO STOP BDE-EMAIL TRANSMISSIONS         
TLOOKECB DC    F'0'                POST TO WAKE UP BDE-EMAIL SUBTASK            
BDFTCB   DS    F                   TCB FOR BDE-FTP SUBTASK                      
BDFECB   DS    F                   ECB FOR BDE-FTP SUBTASK                      
PSTPECB  DS    F                   POST TO STOP BDE-FTP TRANSMISSIONS           
PLOOKECB DC    F'0'                POST TO WAKE UP BDE-FTP SUBTASK              
MQMTCB   DS    F                   TCB FOR E-MAIL SUBTASK                       
MQMECB   DS    F                   ECB FOR E-MAIL SUBTASK                       
MSTPECB  DS    F                   POST TO STOP E-MAIL TRANSMISSIONS            
MLOOKECB DC    F'0'                POST TO WAKE UP E-MAIL SUBTASK               
NJETCB   DS    F                   TCB FOR NJE SUBTASK                          
NJEECB   DS    F                   ECB FOR NJE SUBTASK                          
JSTPECB  DS    F                   POST TO STOP NJE TRANSMISSIONS               
JLOOKECB DC    F'0'                POST TO WAKE UP NJE SUBTASK                  
FTPTCB   DS    F                   TCB FOR FTP SUBTASK                          
FTPECB   DS    F                   ECB FOR FTP SUBTASK                          
FSTPECB  DS    F                   POST TO STOP FTP TRANSMISSIONS               
FLOOKECB DC    F'0'                POST TO WAKE UP FTP SUBTASK                  
DARETCB  DS    F                   TCB FOR DARE SUBTASK                         
DAREECB  DS    F                   ECB FOR DARE SUBTASK                         
DSTPECB  DS    F                   POST TO STOP DARE TRANSMISSIONS              
DLOOKECB DC    F'0'                POST TO WAKE UP DARE SUBTASK                 
CFTPTCB  DS    F                   TCB FOR COLUMBINE SUBTASK                    
CFTPECB  DS    F                   ECB FOR COLUMBINE SUBTASK                    
CSTPECB  DS    F                   POST TO STOP COLUMBINE XMISSIONS             
CLOOKECB DC    F'0'                POST TO WAKE UP COLUMBINE SUBTASK            
ADVNTCB  DS    F                   TCB FOR ADVANTIS SUBTASK                     
ADVNECB  DS    F                   ECB FOR ADVANTIS SUBTASK                     
ASTPECB  DS    F                   POST TO STOP ADVANTIS XMISSIONS              
ALOOKECB DC    F'0'                POST TO WAKE UP ADVANTIS SUBTASK             
FAXGTCB  DS    F                   TCB FOR FAXGATE SUBTASK                      
FAXGECB  DS    F                   ECB FOR FAXGATE SUBTASK                      
XSTPECB  DS    F                   POST TO STOP FAXGATE XMISSIONS               
XLOOKECB DC    F'0'                POST TO WAKE UP FAXGATE SUBTASK              
BIASTCB  DS    F                   TCB FOR BIAS SUBTASK                         
BIASECB  DS    F                   ECB FOR BIAS SUBTASK                         
BSTPECB  DS    F                   POST TO STOP BIAS XMISSIONS                  
BLOOKECB DC    F'0'                POST TO WAKE UP BIAS SUBTASK                 
PDFSTCB  DS    F                   TCB FOR PDF SUBTASK                          
PDFSECB  DS    F                   ECB FOR PDF SUBTASK                          
PDFPECB  DS    F                   POST TO STOP PDF  XMISSIONS                  
OLOOKECB DC    F'0'                POST TO WAKE UP BIAS SUBTASK                 
*                                                                               
DSPACE   DC    C' '                DATASPACE IDENTIFIER                         
XMITEZ   DC    C'N'                ATTACH EASYLINK SUBTASK                      
XMITNFX  DC    C'N'                ATTACH NFX SUBTASK                           
XMITENC  DC    C'Y'                ATTACH ENCODA SUBTASK                        
XMITBDE  DC    C'N'                ATTACH BDE-EMAIL SUBTASK                     
XMITBDF  DC    C'Y'                ATTACH BDE-FTP SUBTASK                       
XMITMQM  DC    C'Y'                ATTACH MQ E-MAIL SUBTASK                     
XMITNJE  DC    C'N'                ATTACH NJE SUBTASK                           
XMITFTP  DC    C'Y'                ATTACH FTP SUBTASK                           
XMITDARE DC    C'Y'                ATTACH DARE SUBTASK                          
XMITADVN DC    C'N'                ATTACH ADVANTIS SUBTASK                      
XMITCOL  DC    C'Y'                ATTACH COLUMBINE SUBTASK                     
XMITFAXG DC    C'Y'                ATTACH FAXGATE SUBTASK                       
XMITBIAS DC    C'Y'                ATTACH BIAS SUBTASK                          
XMITPDF  DC    C'N'                ATTACH PDFS SUBTASK                          
PQSCAN   DC    C'Y'                SCAN PRINT QUEUES FOR REPORTS                
TRACEFLG DC    C'N'                DON'T PRINT DETAILED TRACE                   
CBSFIX   DC    C'N'                DON'T DO SPECIAL CODE FOR CBS                
BDF2     DC    C'N'                DON'T DO SPECIAL CODE FOR 2ND BDF            
*                                                                               
EZSTASK  DC    C'EDIEZS  '         EASYLINK SENDER SUBTASK NAME                 
NJETASK  DC    C'EDINJE  '         NJE SUBTASK NAME                             
NFXTASK  DC    C'EDINFX  '         NFX SENDER SUBTASK NAME                      
MQMTASK  DC    C'EDIMQMS '         E-MAIL SENDER SUBTASK NAME                   
ENCTASK  DC    C'EDIENCS '         ENCODA SENDER SUBTASK NAME                   
BDETASK  DC    C'EDIBDES '         BDE-EMAIL SENDER SUBTASK NAME                
BDFTASK  DC    C'EDIBDFS '         BDE-FTP SENDER SUBTASK NAME                  
FTPTASK  DC    C'EDIFTP  '         FTP SUBTASK NAME                             
DARETASK DC    C'EDIDARS '         DARE SENDER SUBTASK NAME                     
COLTASK  DC    C'EDICOL  '         COLUMBINE SUBTASK NAME                       
ADVNTASK DC    C'EDIADVN '         ADVANTIS SUBTASK NAME                        
FAXGTASK DC    C'EDIFAX  '         FAXGATE SENDER SUBTASK NAME                  
BIASTASK DC    C'EDIBIAS '         BIAS SENDER SUBTASK NAME                     
PDFTASK  DC    C'EDIPDFS '         BIAS SENDER SUBTASK NAME                     
*                                                                               
VATBAMR1 DS    V                   A(APPC/MVS ASYNCHRONOUS MANAGER RTN)         
FUNCTION DC    F'2'                ASYNC_MANAGER CLEANUP                        
ASYNCHRONOUS_NUMBER DS F                                                        
APPC_RETURN_CODE    DS F                                                        
*                                                                               
WAITSECS DC    F'18000'            DEFAULT WAIT TIME = 3 MINUTES                
WAITSEC2 DC    F'0'                                                             
LASTTIME DC    F'0'                BIN TIME FOR LAST PQ SCAN                    
DESTNTRY DS    XL(DESTTBLQ)        TEMP STORAGE FOR 1 DESTTBL  ENTRY            
XMTTBNUM DS    F                   XMITTBL CURRENT NUMBER OF ENTRIES            
XMTENTRY DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
XMTENTR2 DS    XL(XMTTBLQ)         TEMP STORAGE FOR 1 XIMTABLE ENTRY            
XMTTBMAX DC    A(0)                XMITTBL MAX NO. OF ENTRIES (RPT/DAY)         
NUMPQS   DS    F                   NUMBER OF PRINT QUEUES                       
MAXREPS  DC    F'0'                MAXIMUM LOGICAL REPORTS PER DAY              
EDCTBKSZ DC    A(0)                EDICT FILE PHYSICAL RECORD LENGTH            
EDCTFRPT DC    H'0'                EDICT FILE PHYSICAL RECORDS/TRACK            
EDCTFRPB DC    H'0'                EDICT FILE LOGICAL RECORDS/BLOCK             
EDCTFRCL DC    H'0'                EDICT FILE LOGICAL RECORD LENGTH             
EDCTFTPD DS    H                   EDICT FILE TRACKS PER DAY                    
EDCTFLST DS    H                   EDICT FILE LAST TRACK FOR TODAY              
*                                                                               
SKIPTHIS DC    C'N'                Y = SKIP THIS REPORT                         
*                                                                               
METHINCL DC    (MTHPMAXQ)H'0'      ALWAYS ADD THESE METHODS' REPORT             
MTHPMAXQ EQU   11                  MAX NUMBER OF METHOD FILTERS                 
METHXCLD DC    (MTHNMAXQ)H'0'      NEVER ADD THESE METHODS' REPORT              
MTHNMAXQ EQU   11                  MAX NUMBER OF NEG METHOD FILTERS             
*                                                                               
SUBFILT  DC    CL3'   '            OPTIONAL SUB-ID FILTER                       
EXSUBFT  DC    CL3'   '            OPTIONAL EXSUB-ID FILTER                     
ELCODE   DS    X                                                                
MAXDESTS DC    H'100'              DEFAULT MAX DESTS = 100 DESTS                
XTBLSIZE DC    H'4'                DEFAULT XMTTBLE SIZE = 4 DAYS                
ABCDSTAB DS    A                   A(BROADCAST DEST TABLE)                      
EBCDSTAB DS    A                   A(END OF BROADCAST DEST TABLE)               
LOGID    DS    C                   FOR LOGGING (SPECIFY EDICT FILE)             
DRTEST   DC    C'N'                'Y' = DISASTER RECOVERY TEST MODE            
EDICTTYP DS    C                   'A' OR 'R' (EDICTA VS. EDICTR)               
MAJORNAM DC    C'EDICT   '         MAJOR RESOURCE NAME FOR EDICTFIL ENQ         
MINORNAM DC    C'        '         MINOR RESOURCE NAME (EDICTFIL D/A)           
TODAY3   DS    XL3                 TODAY -- BINARY YMD                          
TODAY6   DS    CL6                 TODAY -- EBCDIC YYMMDD                       
DAY6     DS    CL6                 EBCDIC YYMMDD                                
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
ENDPQCI  DS    C                   'Y' = END OF THIS PQ C/I                     
EMPTYRPT DS    C                   'Y' = PQ REPORT HAS NO DATA TO SEND          
DESTINAT DS    CL25                EASYLINK DESTINATION                         
DESTFMT  DS    CL16                FORMATTED DESTINATION                        
DESTTYPE DS    C                   DESTINATION TYPE                             
STIMER1  DS    XL4                 FOR TIMER POPS                               
STIMER2  DS    XL4                                                              
STIMERND DS    XL4                                                              
PQINDEX  DS    XL40                PRINT QUEUE INDEX ENTRY                      
RPTPQTYP DS    XL1                 PQ REPORT TYPE                               
RPTAGY   DS    CL2                 REPORT AGENCY ALPHA                          
RPTAGYOP DS    X                   SAVED AGY OPTS FROM X'B4'(CTAGDELQ)          
RPTUID   DS    CL8                 REPORT USERID (ALPHA)                        
RPTUIDNO DS    XL2                 REPORT USERID                                
RPTSUBID DS    CL3                 REPORT SUB-ID                                
RPTREFNO DS    XL2                 REPORT REFERENCE NUMBER                      
RPTCRTIM DS    XL2                 REPORT CREATION TIME                         
RPTCRDAT DS    XL2                 REPORT CREATION DATE - CMPRSD                
RPTLOGNO DS    H                   LOGICAL REPORT SEQ. WITHIN PHYSICAL          
RPTLDSTS DS    H                   LOG. REP. DEST. SEQ. WITHIN PHYSICAL         
RPTMETH  DS    C                   LOGICAL REPORT TRANSMISSION METHOD           
RPTPM360 DS    C                   PM360 FLAG                                   
RPTERROR DS    X                   ERROR REASON CODE                            
RPTFLAGS DS    X                   VARIOUS FLAGS                                
EZMAILBX DS    XL3                 MAILBOX NUMBER (PWOS, WITHOUT '62')          
MQAPP    DS    CL(L'EDFMQAPP)      MQDEF APPLICATION CODE                       
MQID     DS    XL(L'EDFMQID)       MQDEF QUEUE #                                
MQLAB    DS    XL(L'EDFMQLAB)      MQ MESSAGE LABEL FOR FACPAK                  
PQSCL    DS    CL(L'EDFCPQCL)      PQ CLASS AFTER SENT                          
PQSST    DS    XL(L'EDFCPQST)      PQ STATUS AFTER SENT                         
CARD     DS    CL80                FOR CONTROL CARDS                            
XACTDATA DS    CL62                DATA FROM ++DDS TRN CARD                     
FILENAME DS    CL(L'EDFBDFIL)      BDE-FTP FILE NAME                            
UIDNO    DS    CL36                UNIQUE ID NUMBER (PDF)                       
KEY      DS    XL25                FOR CTFILE READS                             
KEY2     DS    XL48                FOR GENDIR/FILE READS                        
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL256               PQ RECORD DATA                               
ATSARBUF DS    A                   TSAR BUFFER ADDRESSS                         
ATSAROFF DS    A                   TSAROFF ADDRESSS                             
TSARBLK  DS    XL(TSARDL)          TSAR CONTROL BLOCK                           
         EJECT                                                                  
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60) NUMBER OF TABLE ENTRIES                
         DC    H'60'                     MUST REMAIN AS 60                      
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBAMR1  DC    CL8'ATBAMR1'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
*                                                                               
         DS    0D                                                               
         DC    C'EDICTBLK'                                                      
EDICTBLK DC    18432X'00'          EDICT FILE BLOCK                             
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DC    4000X'00'           CTFILE/GENFIL I/O AREA                       
*                                                                               
         DS    0D                                                               
         DC    C'*RPTNUM*'                                                      
RPTNUMS  DS    (NUMRPTSQ)XL2       REPORT NUMBERS TO EXAMINE                    
RPTNUMSX DC    X'0000'                                                          
NUMRPTSQ EQU   2500                MAX NUMBER OF CLASS G RPTS PER PQ            
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'CITABLE*'                                                      
CITABLE  DC    (MAXPQS*CITBLLNQ)X'FF'                                           
         DC    X'FF'               END OF TABLE MARKER                          
MAXPQS   EQU   16                  MAXIMUM OF 16 PRINT QUEUES                   
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*LUTABLE'                                                      
LUTABLE  DS    (MAXLUS)XL(LUTABLEQ)                                             
         DC    X'FF'                                                            
MAXLUS   EQU   100                                                              
         DS    0D                                                               
EDFENTRY DS    CL256                                                            
         EJECT                                                                  
***********************************************************************         
* SSB AND UTL                                                                   
***********************************************************************         
         DS    0D                                                               
         DC    CL16'*SSB**SSB**SSB**'                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
         DC    CL16'*UTL**UTL**UTL*'                                            
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'0A'               SYSTEM 10 (CONTROL)                          
         ORG                                                                    
UTLL     EQU   *-UTL                                                            
*                                                                               
         EJECT                                                                  
TROUTBND DS    0XL256                                                           
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'40404040404040404040404040404040' 00-0F                        
         DC    X'40404040404040404040404040404040' 10-1F                        
         DC    X'40404040404040404040404040404040' 20-2F                        
         DC    X'40404040404040404040404040404040' 30-3F                        
         DC    X'404142434445464748494A4B4C4D4E4F' 40-4F                        
         DC    X'505152535455565758595A5B5C5D5E5F' 50-5F                        
         DC    X'606162636465666768696A6B6C6D6E6F' 60-6F                        
         DC    X'707172737475767778797A7B7C7D7E7F' 70-7F                        
         DC    X'808182838485868788898A8B8C8D8E8F' 80-8F                        
         DC    X'909192939495969798999A9B9C9D9E9F' 90-9F                        
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF' A0-AF                        
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF' B0-BF                        
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF' C0-CF                        
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF' D0-DF                        
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF' E0-EF                        
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF' F0-FF                        
         EJECT                                                                  
*                                                                               
FACIDTAB DS    0CL8                                                             
         DC    C'ADV1',C'ADV1'                                                  
         DC    C'ADV2',C'ADV2'                                                  
         DC    C'ADV3',C'ADV3'                                                  
         DC    C'ADV4',C'ADV4'                                                  
         DC    C'ADV5',C'ADV5'                                                  
         DC    C'ADV6',C'ADV6'                                                  
         DC    C'ADV7',C'ADV7'                                                  
         DC    C'ADV8',C'ADV8'                                                  
         DC    C'AD1 ',C'ADV1'                                                  
         DC    C'AD2 ',C'ADV2'                                                  
         DC    C'AD3 ',C'ADV3'                                                  
         DC    C'AD4 ',C'ADV4'                                                  
         DC    C'AD5 ',C'ADV5'                                                  
         DC    C'AD6 ',C'ADV6'                                                  
         DC    C'AD7 ',C'ADV7'                                                  
         DC    C'AD8 ',C'ADV8'                                                  
         DC    C'REPA',C'REPA'                                                  
         DC    C'REPB',C'REPB'                                                  
         DC    C'REPC',C'REPC'                                                  
         DC    C'TST ',C'TST '                                                  
         DC    C'MEL ',C'MEL '                                                  
         DC    C'FQA ',C'FQA '                                                  
         DC    C'CSC ',C'CSC '                                                  
         DC    4X'FF'                                                           
*                                                                               
USERFILT DC    (USRFMAXQ)H'0'      OPTIONAL HEX USERID FILTERS                  
USRFMAXQ EQU   201                 MAX NUMBER OF USERID FILTERS                 
EXUFILT  DC    (EXUFMAXQ)H'0'      OPTIONAL HEX NEGATIVE USERID FILTERS         
EXUFMAXQ EQU   401                 MAX NUMBER OF NEG USERID FILTERS             
*                                                                               
         EJECT                                                                  
BCDSTABD DSECT                     BROADCAST FAXING DESTINATION DSECT           
BCDSTNAT DS    CL(L'DESTINAT)      BROADCAST DESTINATION                        
BCDSTFMT DS    CL(L'DESTFMT)       BROADCAST DESTINATION FORMAT                 
BCDSTABQ EQU   *-BCDSTABD                                                       
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
       ++INCLUDE CTGENEDICT                                                     
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
       ++INCLUDE CTGENMQDEF                                                     
         EJECT                                                                  
       ++INCLUDE FATABSDEQU                                                     
         EJECT                                                                  
         DSECT                                                                  
         IEZCIB                                                                 
*                                                                               
         IEZCOM                                                                 
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* DMPRTQK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
* DMSYSTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMSYSTABD                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* EDIDESTD                                                                      
         PRINT OFF                                                              
EDIDESTD DSECT                                                                  
       ++INCLUDE EDIDESTD                                                       
         PRINT ON                                                               
* DDEDIFGWRK                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDEDIFGWRK                                                     
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASSB / FASSBOFF                                                              
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
         PRINT ON                                                               
**FASELIST                                                                      
         PRINT OFF                                                              
***    ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* IHASDWA                                                                       
         PRINT OFF                                                              
         IHASDWA GR32=YES                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012DDEDICT   03/04/21'                                      
         END                                                                    
