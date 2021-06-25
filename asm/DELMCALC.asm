*          DATA SET DELMCALC   AT LEVEL 044 AS OF 02/20/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMCALA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'LOCAL MONTHLIES CALCULATION DRIVER'                             
***********************************************************************         
*                                                                               
* THIS IS THE LOCAL MONTHLIES CALCULATION DRIVER MODULE. PRIOR TO THIS          
* PROGRAM, AN ICETOOL APPLICATION HAS RUN WHICH PRODUCES A LIST OF              
* DDNAMES AND PATHNAMES FOR DYNAMIC ALLOCATION. THE ICETOOL MODULE IS:          
*                                                                               
*  DELMINDNR (MODE=NORMAL)                                                      
*  DELMINDDP (MODE=DPCUME)                                                      
*  DELMINDPU (MODE=PGMAMUP)                                                     
*                                                                               
* THIS PROGRAM IS ESSENTIALLY A PRE-PROCESSOR FOR THE LOCAL MONTHLIES.          
* THE BASIC IDEA IS THAT WE CALL ICETOOL ITERATIVELY, ONCE FOR EACH             
* MARKET/STREAM COMBINATION, AND APPEND THE OUTPUT DATASETS FOR EACH            
* CALL TO THE FINAL OUTPUT DATASETS WHICH ARE FED INTO THE CONVERSION.          
*                                                                               
* 1. RELINK THIS PROGRAM IF THE WORK RECORD LENGTH CHANGES (IT IS               
*     DEFINED IN DELMDSECT). THE TWO EQUATES TO WATCH ARE:                      
*      W_LMRECLQ  (FOR MODE=NORMAL AND MODE=PGNAMUP)                            
*      WD_LMRECLQ (FOR MODE=DPCUME)                                             
*     EACH OUTPUT DATASET IS DYNAMICALLY ALLOCATED WITH AN APPROPRIATE          
*     DEFAULT LRECL, PRIMARILY BASED UPON THE MODE= CARD.                       
*     THE NIELSEN LOCAL MONTHLIES *INPUT* DATASETS ARE ALSO DYNAMICALLY         
*     ALLOCATED USING THE SAME DEFAULT LRECL, SO IT IS CRITICAL THAT            
*     NO INPUT RECORD EXCEEDS THE WORK RECORD LRECL IN LENGTH.                  
*                                                                               
* 2. TABLE TMPDDTAB DRIVES THE DYNAMIC ALLOCATIONS OF THE *OUTPUT*              
*     DATASETS. FEEL FREE TO ADD, DELETE, OR CHANGE ANY ENTRIES. ANY            
*     OR ALL OF THESE ALLOCATIONS MAY BE OVERRIDDEN IN JCL. IF THE              
*     DATASET IS ALLOCATED VIA JCL, IT SIMPLY WON'T BE ALLOCATED BY             
*     THIS PROGRAM.                                                             
*                                                                               
* 3. THIS PROGRAM CHECKS FOR AN OPERATOR "P" (STOP) COMMAND AFTER               
*     PROCESSING EACH MARKET. WE DON'T EXPECT THAT THIS WILL TYPICALLY          
*     BE NECESSARY. IF A "P" COMMAND IS SEEN, THIS PROGRAM WILL FINISH          
*     CLEANLY, AND WHICHEVER MARKETS HAVE ALREADY BEEN PROCESSED WILL           
*     FLOW THROUGH TO THE CONVERSION. ANY REMAINING MARKETS WOULD THEN          
*     NEED TO BE PROCESSED MANUALLY.                                            
*                                                                               
***********************************************************************         
DELMCALC CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,DELMCALC,=V(REGSAVE),R9,R8                                     
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(,RF),=C'DRVTRACE'  DDNAME=DRVTRACE               
*                                                                               
         USING PNA_RECD,R5                                                      
         LA    R5,RDW              PATHNAME WORK RECORD                         
*                                                                               
         BAS   RE,INIT             INITIALIZATION LOGIC                         
*                                                                               
         CP    NUMMKTS,=P'0'       TOTAL NUMBER OF MARKETS/STREAMS              
         JE    *+2                 NO MARKETS TO PROCESS ?!?                    
*                                                                               
* PRINT A LIST OF THE MARKETS/STREAMS WE'LL BE PROCESSING. (NOTE THAT           
* WE'LL NEVER HIT THE EOF ON NFSFILES IN THIS LOOP, SO IT DOESN'T               
* MATTER WHAT THE EODAD IS AT THIS POINT.)                                      
*                                                                               
         MVC   P(40),=C'THE FOLLOWING MARKETS WILL BE PROCESSED:'               
         GOTO1 =V(PRINTER)                                                      
         OPEN  NFSFILES            CONTAINS COMPLETE PATHNAME INDEX             
MAIN20   DS    0H                                                               
         GET   NFSFILES,RDW                                                     
         CLC   =C'BEGIN MARKET: ',PNA_HEADER_EYEC                               
         BNE   MAIN20              ONLY LOOK AT 'BEGIN MARKET' RECORDS          
         BAS   RE,FMTMKT           FILL IN REMAINDER OF PRINT LINE              
         GOTO1 =V(PRINTER)                                                      
         AP    MKT#CNTR,=P'1'      INCREMENT MARKET COUNTER                     
         CP    MKT#CNTR,NUMMKTS    ANY MORE MARKETS?                            
         BNH   MAIN20              YES                                          
         CLOSE NFSFILES                                                         
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(23),=C'INITIALIZATION COMPLETE'                                
         GOTO1 =V(PRINTER)                                                      
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* CALCULATE THE "VIP" FOR EACH MARKET/STREAM, ONE AT A TIME.                    
*                                                                               
         OPEN  NFSFILES            RE-OPEN TO READ FROM THE BEGINNING           
         ZAP   MKT#CNTR,=P'1'      INITIALIZE MARKET COUNTER                    
*                                                                               
NXTMKT   DS    0H                                                               
         GET   NFSFILES,RDW                                                     
         CLC   =C'BEGIN MARKET: ',PNA_HEADER_EYEC                               
         BE    NXTMKT20            THIS MUST BE 1ST RECORD OF EACH SET          
         ABEND 7,DUMP              NFSFILES HAS GONE HORRIBLY WRONG!            
*                                  FAILURE HERE INDICATES THAT THERE            
*                                   MAY BE A LOGIC ERROR IN WHICHEVER           
*                                   PRE-PROCESSOR MODULE PRECEDED THIS          
*                                   (DELMINDNR, DELMINDPU, DELMINDDP).          
*                                                                               
NXTMKT20 DS    0H                                                               
         MVC   P(21),=C'** PROCESSING MARKET '                                  
         BAS   RE,FMTMKT           FILL IN REMAINDER OF PRINT LINE              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   RE,ALLOCINP         ALLOCATE NIELSEN INPUT FILES                 
         BNE   NXTMKT30            SKIP IF WE SEE FILE INTEGRITY ERRORS         
*                                                                               
         BAS   RE,ALOCTMPM         ALLOCATE TEMPORARY DATASETS                  
         BAS   RE,CALLICE          ** CALL ICETOOL FOR CALCULATIONS **          
         BAS   RE,DEALTMPM         DEALLOCATE TEMPORARY DATASETS                
*                                                                               
NXTMKT30 DS    0H                                                               
         BAS   RE,DEALCINP         DEALLOCATE L.M. INPUT FILES                  
*                                                                               
         AP    MKT#CNTR,=P'1'      INCREMENT MARKET COUNTER                     
         MVI   P,0                 SKIP A LINE BETWEEN MARKETS                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   RE,CHKOPER          CHECK FOR OPERATOR INTERVENTION              
         CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BNE   NXTMKT              NO: PROCESS NEXT MARKET                      
*                                                                               
         MVC   RETCODE,=F'4'       SET WARNING RETURN CODE                      
         MVC   P(46),=C'*** PROCESSING STOPPED BY OPERATOR COMMAND ***'         
         GOTO1 =V(PRINTER)                                                      
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ENDMKTS  DS    0H                                                               
         CLOSE NFSFILES                                                         
*                                                                               
         BAS   RE,CHKFINAL         CHECK OUTPUT FILE RECORD COUNTS              
*                                                                               
         MVC   P(35),=C'PROCESSING COMPLETED WITHOUT ERRORS'                    
         CLI   ANYWARNS,C'Y'                                                    
         BNE   *+10                                                             
         MVC   P(54),=C'PROCESSING COMPLETED ** WITH WARNINGS AND/OR ER+        
               RORS **'                                                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE RC=RETCODE                                                       
         EJECT                                                                  
*                                                                               
* READ PARAMETER CARDS FROM SYSIN. THE SYSIN DD IS GENERATED BY                 
* THE PREPROCESSOR MODULE WHICH RAN PRIOR TO THIS (DELMINDNR,                   
* DELMINDPU, OR DELMINDDP).                                                     
*                                                                               
READCRDS NTR1                                                                   
*                                                                               
READCARD DS    0H                                                               
         MVC   TITLE(34),=C'LOCAL MONTHLIES CALCULATION DRIVER'                 
         MVC   P(16),=C'PARAMETER CARDS:'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NEXTCARD DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    RDCARD90            ALL PARAMETER CARDS VALID                    
*                                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT?                                     
         BE    NEXTCARD            YES                                          
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   *+18                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      DDSIO= OVERRIDE                              
         B     NEXTCARD                                                         
*                                                                               
         CLC   =C'DSPACE=',CARD                                                 
         BNE   *+18                                                             
         LA    RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE= OVERRIDE                   
         B     NEXTCARD                                                         
*                                                                               
         IF CLC,=C'ICETOOL=',EQ,CARD                                            
           CLC   =C'YES ',CARD+8   DEFAULT: CALL ICETOOL                        
           BE    NEXTCARD                                                       
           CLC   =C'NO ',CARD+8    FOR TESTING ONLY                             
           JNE   *+2               INVALID ICETOOL= OPTION!                     
           MVI   CALICESW,C'N'     DON'T CALL ICETOOL                           
           B     NEXTCARD                                                       
         ENDIF                                                                  
*                                                                               
         IF CLC,=C'DEBUG=',EQ,CARD                                              
           CLC   =C'NO ',CARD+6    DEFAULT: NOT IN DEBUG MODE                   
           BE    NEXTCARD                                                       
           CLC   =C'YES ',CARD+6   FOR TESTING ONLY                             
           JNE   *+2               INVALID DEBUG= OPTION!                       
           MVI   DEBUGSW,C'Y'      SET DEBUG MODE ON                            
           B     NEXTCARD                                                       
         ENDIF                                                                  
*                                                                               
         CLC   =C'WARNMAIL=',CARD                                               
         BNE   RDCARD10                                                         
         CLC   =C'YES ',CARD+9     DEFAULT: SEND WARNING E-MAILS                
         BE    NEXTCARD                                                         
         CLC   =C'NO ',CARD+9      FOR TESTING ONLY                             
         JNE   *+2                 INVALID WARNMAIL= PARAMETER OPTION!          
         MVI   WARNEFLG,C'N'       DON'T SEND WARNING E-MAILS                   
         B     NEXTCARD                                                         
*                                                                               
RDCARD10 DS    0H                                                               
         CLC   =C'CHECKTOTS=',CARD                                              
         BNE   RDCARD15                                                         
         CLC   =C'YES ',CARD+10    DEFAULT: CHECK FINAL TAPE TOTALS             
         BE    NEXTCARD                                                         
         CLC   =C'NO ',CARD+10     FOR TESTING ONLY                             
         JNE   *+2                 INVALID CHECKTOTS= PARAMETER OPTION!         
         MVI   CHKTOTS,C'N'        DON'T CHECK MARKET TOTALS                    
         B     NEXTCARD                                                         
*                                                                               
RDCARD15 DS    0H                                                               
         IF CLC,=C'DYNALOCMSG=',EQ,CARD                                         
           CLC   =C'SEVERE ',CARD+11 DEFAULT: ONLY PUT SEVERE MESSAGES          
           BE    NEXTCARD                                                       
           CLC   =C'ALL ',CARD+11    PUT INFORMATIONAL MESSAGES ALSO            
           JNE   *+2                 INVALID DYNALOCMSG= PARAM. OPTION!         
           MVI   RBLKRBX+(S99EMGSV-S99RBX),S99XINFO                             
           B     NEXTCARD                                                       
         ENDIF                                                                  
*                                                                               
         CLC   =C'WRKDSNQUAL=',CARD FOR TESTING ONLY                            
         BNE   RDCARD30                                                         
         LA    RF,CARD+11          WRKDSNQUAL= VALUE                            
         LA    RE,DSNQUALS         SAVE DSN QUALIFIERS                          
         SR    R1,R1               COUNT SIGNIFICANT CHARACTERS                 
         LHI   R0,L'PNA_PARM_VALUE                                              
RDCARD20 CLI   0(RF),C' '          BUILD ONE CHARACTER AT A TIME                
         BE    NEXTCARD                                                         
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         AHI   R1,1                                                             
         STC   R1,DSNQUALL         SAVE NO. OF SIGNIFICANT CHARACTERS           
         BCT   R0,RDCARD20                                                      
         B     NEXTCARD                                                         
*                                                                               
RDCARD30 DS    0H                                                               
* MOST CARDS CAN BE IGNORED, BECAUSE WE HANDLED THEM IN THE PREVIOUS            
* ICETOOL APPLICATION.                                                          
*                                                                               
         CLC   =C'DATE=',CARD      DATE= OVERRIDE CARD?                         
         BE    NEXTCARD                                                         
*                                                                               
         CLC   =C'SERVICE=',CARD   SERVICE= CARD?                               
         BNE   *+14                                                             
         MVC   SERVICE,CARD+8      SAVE THE SERVICE VALUE                       
         B     NEXTCARD                                                         
*                                                                               
         CLC   =C'STREAM=',CARD    STREAM= CARD?                                
         BE    NEXTCARD                                                         
         CLC   =C'BOOK=',CARD      BOOK= CARD?                                  
         BE    NEXTCARD                                                         
         CLC   =C'MARKET=',CARD    MARKET= CARD?                                
         BE    NEXTCARD                                                         
         CLC   =C'EXMARKET=',CARD  EXMARKET= CARD?                              
         BE    NEXTCARD                                                         
         CLC   =C'DATA',CARD       DATA= CARD?                                  
         BE    NEXTCARD                                                         
*                                                                               
         CLC   =C'MODE=',CARD      MODE= CARD?                                  
         BE    RDCARD50                                                         
         MVC   P(37),=C'*** ERROR: INVALID PARAMETER CARD ***'                  
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                INVALID PARAMETER CARD                       
*                                                                               
RDCARD50 DS    0H                                                               
         CLC   =C'NORMAL  ',CARD+5 NORMAL (QH AND PAV) MODE?                    
         BNE   *+12                                                             
         MVI   MODE,NORMAL                                                      
         B     NEXTCARD                                                         
         CLC   =C'DPCUME  ',CARD+5 DAYPART CUME MODE?                           
         BNE   *+12                                                             
         MVI   MODE,DPCUME                                                      
         B     NEXTCARD                                                         
         CLC   =C'PGNAMUP ',CARD+5 PROGRAM NAME UPDATE MODE?                    
         JNE   *+2                 INVALID MODE= PARAMETER                      
         MVI   MODE,PGNAMUP                                                     
         B     NEXTCARD                                                         
*                                                                               
RDCARD90 DS    0H                                                               
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     YES                 ALL CARDS ARE VALID                          
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
*                                                                               
* INITIALIZATION LOGIC.                                                         
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     R2,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,R2                                                       
*                                  ASSUME WE'RE *NOT* A STARTED TASK            
         QEDIT ORIGIN=COMCIBPT,CIBCTR=0  DISALLOW MODIFY COMMANDS               
         DROP  R2                                                               
*                                                                               
         L     RF,PSAAOLD-PSA(,0)       GET CURRENT/HOME ASCB                   
         L     RF,(ASCBASXB-ASCB)(,RF)  GET ASXB ADDRESS                        
         L     RF,(ASXBSENV-ASXB)(,RF)  GET ACEE ADDRESS                        
         CLC   =CL4'ACEE',0(RF)         VALID ACEE?                             
         JNE   *+2                      NO: IMPOSSIBLE                          
         MVC   RACFUSER,(ACEEUSRI-ACEE)(RF)  YES: EXTRACT RACF USERID           
*                                                                               
         LA    R2,FULL                                                          
         EXTRACT (R2),'S',FIELDS=ASID                                           
         LA    R1,FULL                                                          
         L     R2,0(R1)                                                         
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R2 = A(ASSB)                                
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     R1,ASSBJSAB-ASSB(R2) R1 = A(JSAB)                                
         MVC   JOBID,JSABJBID-JSAB(R1)  JOBID (E.G., JOB12345)                  
         SAM24 ,                   SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         GOTO1 =V(LOADER),DMCB,=CL8'T00AD2',0                                   
         ICM   RF,15,DMCB+4        DEMTABOF LOADED OKAY?                        
         JZ    *+2                 NO                                           
*                                                                               
         GOTO1 (RF),DMCB,MRKTNAMT  GET A(MARKET NAME TABLE)                     
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JZ    *+2                                                              
         MVC   MKTNAMLN,6(R1)      L'TABLE ENTRY                                
*                                                                               
         CLC   =C'NT',0(RE)        FIND USA NIELSEN TV TABLE                    
         BE    *+20                GOT IT                                       
         ICM   RE,7,2(RE)          TRY NEXT ONE                                 
         OC    0(2,RE),0(RE)       EOT?                                         
         BNZ   *-20                NO                                           
         DC    H'0'                YES: WHERE IS THE TABLE?                     
         LA    RE,5(RE)            BYPASS HEADER                                
         ST    RE,AMKTNAMS         SAVE A(NT MARKET NAME TABLE)                 
*                                                                               
         LA    RF,LMIN             "FAKE" DCB FOR DDDYNALLOC                    
         LHI   R0,W_LMRECLQ        L'LOCAL MONTHLIES WORK RECORD                
         CLI   MODE,DPCUME                                                      
         BNE   *+8                                                              
         LHI   R0,WD_LMRECLQ       L'LOCAL MONTHLIES DPCUME WORK RECORD         
         USING IHADCB,RF                                                        
         STH   R0,DCBLRECL         LRECL                                        
         MHI   R0,10                                                            
         STH   R0,DCBBLKSI         BLKSIZE (ARBITRARY, AS PER ED LEW)           
         DROP  RF                                                               
*                                                                               
         MVC   CNTSA1US,=C'USING(WRK1)' DFSORT PARMS ARE IN DD WRK1CNTL         
         MVC   CNTSA1DF,=C'NFSFILES'   GET NUMBER OF INPUT FILES                
*                                                                               
         LA    R1,ICEPAR1          COUNT OPERATOR VIA PARAMETER LIST            
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL ICETOOL CALL?                     
         JNZ   *+2                 NO: HOW CAN A COUNT OPERATOR FAIL?!?         
*                                                                               
         CP    CNT#RECS,=P'0'      ANY MARKETS TO PROCESS?                      
         JE    *+2                 NO: HOW DID WE GET HERE ?!?                  
*                                                                               
         MVC   NUMMKTS,CNT#RECS    SAVE TOTAL NUMBER OF MARKETS                 
*                                                                               
         CLC   DSNQUALS,SPACES     WILL WE CATALOG THE TEMP DATASETS?           
         BE    INITX               NO                                           
         CP    NUMMKTS,=P'1'       YES: ONLY 1 MARKET ALLOWED PER JOB!          
         BE    INITX                                                            
         ABEND 11                                                               
*                                                                               
INITX    DS    0H                                                               
         GOTO1 =V(DYNALLOC),DMCB,(X'FD',=CL8'STATIONS'),(X'80',SPACES)          
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* FORMAT AN INFORMATIONAL PRINT LINE FOR A MARKET/STREAM.                       
*                                                                               
FMTMKT   NTR1                                                                   
*                                                                               
         MVC   P+21(11),=C'XXX      : '                                         
         MVC   P+21(L'PNA_HEADER_MARKET),PNA_HEADER_MARKET  MARKET #            
         MVI   P+24,C'/'                                                        
         MVC   P+25(L'PNA_HEADER_STREAM),PNA_HEADER_STREAM                      
         MVC   P+32(15),=C'*** UNKNOWN ***'    JUST IN CASE                     
*                                                                               
         PACK  DUB,PNA_HEADER_MARKET                                            
         CVB   R0,DUB              R0 = BINARY MARKET NUMBER                    
         CHI   R0,124              IS THIS NIELSEN'S NEW ATLANTA DMA#?          
         BNE   *+8                                                              
         LHI   R0,168              YES: USE NIELSEN'S OLD ATLANTA DMA#          
*                                                                               
         L     RE,AMKTNAMS         A(NIELSEN TV MARKET NAME TABLE)              
FMTMKT10 OC    0(2,RE),0(RE)       EOT?                                         
         BZ    FMTMKT20            YES: MARKET NOT FOUND (???)                  
         USING MKNTABD,RE                                                       
         CH    R0,MKNNUM           MATCH ON MARKET NUMBER?                      
         BE    *+12                                                             
         AH    RE,MKTNAMLN         NO: BUMP TO NEXT                             
         B     FMTMKT10                                                         
         MVC   MKTNAME,MKNNAME     SAVE MARKET NAME                             
         MVC   P+32(30),MKNNAME    PRINT MARKET NAME                            
         DROP  RE                                                               
*                                                                               
FMTMKT20 DS    0H                                                               
         LA    RF,P+63             POINT BEYOND MARKET NAME                     
         MVI   0(RF),C'('                                                       
         LA    RF,1(RF)                                                         
         EDIT  MKT#CNTR,(3,(RF)),ALIGN=LEFT  THIS RELATIVE MARKET NO.           
         AR    RF,R0               BUMP BY # DIGITS                             
         MVC   0(4,RF),=C' OF '                                                 
         LA    RF,4(RF)                                                         
         EDIT  NUMMKTS,(3,(RF)),ALIGN=LEFT   TOTAL NUMBER OF MARKETS            
         AR    RF,R0               BUMP BY # DIGITS                             
         MVI   0(RF),C')'                                                       
*                                                                               
         MVC   WRNMSG1M,PNA_HEADER_MARKET  MARKET NUMBER                        
         MVI   WRNMSG1M+3,C'/'                                                  
         MVC   WRNMSG1S,PNA_HEADER_STREAM STREAM                                
         MVC   WRNMSG1N,MKTNAME                MARKET NAME                      
         LA    RF,WRNMSG1N+L'WRNMSG1N          PUT PARENS AROUND NAME           
         BCTR  RF,0                                                             
         CLI   0(RF),C' '          FIND LAST CHARACTER                          
         BNH   *-6                                                              
         MVI   1(RF),C')'          CLOSE PARENTHESIS                            
         MVC   WRNMSG1#,JOBID                                                   
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* SEE IF THE OPERATOR HAS INTERRUPTED WITH A 'STOP' (P) COMMAND.                
* NOTE: WE DO *NOT* WAIT ON THE OPERATOR ECB BEFORE CALLING ICETOOL             
* TO DO THE "VIP" CALCULATIONS. SO WHEN THE OPERATOR ISSUES A COMMAND,          
* IT MAY BE A FEW MINUTES BEFORE THAT COMMAND IS ACKNOWLEDGED BY THIS           
* PROGRAM.                                                                      
*                                                                               
CHKOPER  NTR1                                                                   
*                                                                               
         L     R2,ACOMM                                                         
         USING COMLIST,R2                                                       
         L     RF,COMECBPT         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    CHKOPX              NO                                           
*                                                                               
         L     R4,COMCIBPT         A(CIB)                                       
         USING CIBNEXT,R4                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         JNE   *+2                 IMPOSSIBLE: "MODIFY" IS INHIBITED!           
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'STOP COMMAND ACCEPTED'             
*                                                                               
         QEDIT ORIGIN=COMCIBPT,BLOCK=(R4)  FREE THE CIB                         
         DROP  R2                                                               
         DROP  R4                                                               
*                                                                               
CHKOPX   DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* DYNAMICALLY ALLOCATE LOCAL MONTHLIES INPUT FILES, DRIVEN BY THE               
* NFSFILES DD, WHICH IS GENERATED BY THE PRIOR ICETOOL APPLICATION.             
*                                                                               
ALLOCINP NTR1                                                                   
*                                                                               
         MVI   BADMRKT,C'N'        ASSUME NO PROBLEMS WITH MARKET               
*                                                                               
ALLOCI10 DS    0H                                                               
         GET   NFSFILES,RDW                                                     
         CLC   =C'END MARKET:   ',PNA_HEADER_EYEC                               
         BE    ALLOCIX             NO MORE DDNAMES FOR THIS MARKET              
*                                                                               
         CLC   =C'BEGIN DDNAME: ',PNA_HEADER_EYEC                               
         BE    ALLOCI20            THIS MUST BE 1ST RECORD OF EACH SET          
         ABEND 3,DUMP              NFSFILES HAS GONE HORRIBLY WRONG             
*                                  FAILURE HERE INDICATES THAT THERE            
*                                   MAY BE A LOGIC ERROR IN WHICHEVER           
*                                   PRE-PROCESSOR MODULE PRECEDED THIS          
*                                   (DELMINDNR, DELMINDPU, DELMINDDP).          
*                                                                               
ALLOCI20 DS    0H                                                               
         MVC   DDNAME,PNA_HEADER_DDNAME                                         
         MVC   P(43),=C'DYNAMICALLY ALLOCATING TO DDNAME XXXXXXXX: '            
         MVC   P+33(8),DDNAME                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   RECORD,SPACES                                                    
         GET   NFSFILES,RDW        GET THE FIRST PATHNAME                       
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,RDW            RECLEN (FROM RDW)                            
         SHI   R3,(PNA_PATHNAME-PNA_RDW)   R3 NOW = L'PATHNAME                  
*                                                                               
         LR    RF,R3               L'PATHNAME                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),PNA_PATHNAME   PRINT THE FIRST PATHNAME FOR THIS DD         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'H',DDNAME),                        +        
               ((R3),PNA_PATHNAME),(X'54',LMIN)                                 
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    ALLOCI30            NO                                           
*                                                                               
* WE ARE UNABLE TO DYNAMICALLY ALLOCATE AN UNZIPPED FILE WITH THE               
* EXPECTED PATHNAME.                                                            
* WE'VE SEEN THIS HAPPEN WHEN:                                                  
*  1. A ZIP FILE IS CORRUPT (NOTE THAT SERIOUS ZIP FILE CORRUPTION              
*     ERRORS ARE TYPICALLY CAUGHT DURING THE UNZIP STEP PRIOR TO THIS           
*     PROGRAM'S EXECUTION).                                                     
*  2. THE UNZIPPED FILE HAS AN UNEXPECTED PATHNAME (I.E., THAT DOES NOT         
*     CONFORM TO THE SPEC).                                                     
*  3. THE MOUNTPOINT IS BLOWN AWAY. E.G., IF ANOTHER JOB IS SUBMITTED           
*     USING THE SAME USERID AS THIS JOB WHILE THIS JOB IS RUNNING, AND          
*     IF THAT OTHER JOB DOES AN UNMOUNT OF THE CURRENT MOUNTPOINT, THEN         
*     THIS PROGRAM WILL NO LONGER BE LOOKING AT THE CORRECT FOLDER, AND         
*     OF COURSE IT WON'T FIND THE UNZIPPED FILE.                                
*                                                                               
         ABEND 4,DUMP              UNZIPPED FILE DYNALLOC ERROR!                
*                                                                               
ALLOCI30 DS    0H                                                               
         MVC   CNTSA1US,=C'USING(WRK2)' DFSORT PARMS ARE IN DD WRK2CNTL         
         MVC   CNTSA1SP,SPACES                                                  
         MVC   CNTSA1DF,DDNAME     NIELSEN INPUT FILE DDNAME                    
         LA    R1,CNTSA1DF+7       POINT TO LAST BYTE                           
         CLI   0(R1),C' '          LOOK FOR END OF DDNAME                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         MVI   1(R1),C')'          CLOSE DDNAME WITH RIGHT PARENTHESIS          
*                                                                               
         LA    R1,ICEPAR1          COUNT OPERATOR VIA PARAMETER LIST            
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL ICETOOL CALL?                     
         JNZ   *+2                 NO: HOW CAN A COUNT OPERATOR FAIL?!?         
         CP    CNT#RECS,=P'2'      IS THERE 1 HEADER AND 1 FOOTER REC?          
         BE    ALLOCI40            YES                                          
         CLI   MODE,PGNAMUP        (PGNAM AND PGNAMUP ARE CONCATENATED)         
         BNE   *+14                                                             
         CP    CNT#RECS,=P'4'      2 HEADERS AND 2 FOOTERS?                     
         BE    ALLOCI40            YES                                          
*                                                                               
         MVI   BADMRKT,C'Y'        MARKET FAILED PRE-PROCESSING                 
         MVI   ANYWARNS,C'Y'       REMEMBER: WE ISSUED A WARNING                
         CLI   WARNEFLG,C'N'       SEND WARNING E-MAIL...                       
         BE    ALLOCI35            ...UNLESS SUPPRESSED VIA WARNMAIL=NO         
         MVC   WRNMSG1D,=C'BAD INPUT FILE'                                      
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',('WRNMSLQ1',WRNMSG1)                  
ALLOCI35 DS    0H                                                               
         MVC   P(47),=C'*** ERROR *** MISSING HEADER AND/OR FOOTER REC.+        
               '                                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(53),=C'INPUT FILES FOR THIS MARKET/STREAM ARE BEING IG+        
               NORED.'                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ALLOCI40 DS    0H                                                               
         GET   NFSFILES,RDW        GET THE NEXT PATHNAME FOR THIS DD            
         CLC   =C'END DDNAME:   ',PNA_HEADER_EYEC  ARE THERE ANY MORE?          
         BE    ALLOCI10            NO: SEE IF THERE ARE MORE DDNAMES            
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,RDW            RECLEN (FROM RDW)                            
         SHI   R3,(PNA_PATHNAME-PNA_RDW)   R3 NOW = L'PATHNAME                  
*                                                                               
         LR    RF,R3               L'PATHNAME                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),PNA_PATHNAME   PRINT THE CONCATENATED PATHNAME              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'H',DUB),                           +        
               ((R3),PNA_PATHNAME),(X'5C',LMIN)                                 
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    ALLOCI60            NO                                           
         ABEND 4,DUMP              SHOULD NEVER FAIL!                           
*                                   UNZIPPED FILE DYNALLOC ERROR.               
*                                   CHECK OUTPUT FOR PREVIOUS JOBSTEPS          
*                                   (CORRUPT ZIP FILE? OTHER ERROR?)            
*                                                                               
ALLOCI60 DS    0H                                                               
         MVC   CNTSA1SP,SPACES                                                  
         MVC   CNTSA1DF,DUB        NIELSEN INPUT FILE DDNAME                    
         LA    R1,CNTSA1DF+7       POINT TO LAST BYTE                           
         CLI   0(R1),C' '          LOOK FOR END OF DDNAME                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         MVI   1(R1),C')'          CLOSE DDNAME WITH RIGHT PARENTHESIS          
*                                                                               
         LA    R1,ICEPAR1          COUNT OPERATOR VIA PARAMETER LIST            
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL ICETOOL CALL?                     
         JNZ   *+2                 NO: HOW CAN A COUNT OPERATOR FAIL?!?         
         CP    CNT#RECS,=P'2'      IS THERE 1 HEADER AND 1 FOOTER REC?          
         BE    ALLOCI90            YES                                          
*                                                                               
         MVI   BADMRKT,C'Y'        MARKET FAILED PRE-PROCESSING                 
         MVI   ANYWARNS,C'Y'       REMEMBER: WE ISSUED A WARNING                
         CLI   WARNEFLG,C'N'       SEND WARNING E-MAIL...                       
         BE    ALLOCI70            ...UNLESS SUPPRESSED VIA WARNMAIL=NO         
         MVC   WRNMSG1D,=C'BAD INPUT FILE'                                      
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',('WRNMSLQ1',WRNMSG1)                  
ALLOCI70 DS    0H                                                               
         MVC   P(47),=C'*** ERROR *** MISSING HEADER AND/OR FOOTER REC.+        
               '                                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(53),=C'INPUT FILES FOR THIS MARKET/STREAM ARE BEING IG+        
               NORED.'                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ALLOCI90 DS    0H                                                               
         MVC   DUB1,SPACES                                                      
         LLC   R1,DMCB+8                                                        
         N     R1,=X'00000007'     L'RETURNED DDNAME (MINUS ONE !!!)            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB1(0),DUB         RETURNED DDNAME FROM DYNALLOC                
         GOTO1 =V(DYNALLOC),DMCB,(C'C',DDNAME),(X'40',DUB1) CONCATENATE         
         B     ALLOCI30            LOOK FOR MORE FILES TO CONCATENATE           
*                                                                               
ALLOCIX  DS    0H                                                               
         MVC   CNTSA1US,SPACES     SUBSEQUENT COUNTS DON'T NEED "USING"         
         MVC   CNTSA1SP,SPACES                                                  
*                                                                               
         CLI   BADMRKT,C'Y'        MARKET FAILED PRE-PROCESSING?                
         JE    NO                  YES: RETURN WITH BAD CC                      
         J     YES                 NO: RETURN WITH GOOD CC                      
         EJECT                                                                  
*                                                                               
* DYNAMICALLY ALLOCATE ALL TEMPORARY OUTPUT DATASETS NEEDED BY THE              
* ICETOOL APPLICATION. ALSO ALLOCATE ANY REQUIRED TEMPORARY INPUT               
* DATASETS (E.G., DPTOIN).                                                      
*                                                                               
ALOCTMPM NTR1                                                                   
*                                                                               
         MVC   P(34),=C'ALLOCATING TEMPORARY WORK DATASETS'                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R4,TMPDDTAB         DDNAME TABLE                                 
         USING TMPDDTBD,R4                                                      
*                                                                               
ALCTMPM3 DS    0H                                                               
         CLI   0(R4),X'FF'         EOT?                                         
         BE    ALCTMPMX                                                         
*                                                                               
         LLC   R1,MODE             FOR THE CURRENT MODE...                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TMPMODES,0          ...MUST WE ALLOCATE THIS DATASET?            
         BZ    ALCTMPM7            NO                                           
*                                                                               
         CLI   DEBUGSW,C'Y'        DEBUG=YES CARD WAS READ?                     
         BE    *+12                YES: NO NEED TO CHECK DEBUG ATTRIB.          
         TM    TMPMODES,DEBUG      NO: IS THIS A DEBUG DATASET?                 
         BO    ALCTMPM7            YES: DON'T ALLOCATE IT                       
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'A',TMPDDNAM)                                
         CLI   4(R1),0             IS DATASET ALREADY ALLOCATED?                
         BE    ALCTMPM5            NO                                           
*                                                                               
         MVC   P(L'TMPDDNAM),TMPDDNAM                                           
         MVC   P+9(20),=C'PREVIOUSLY ALLOCATED'                                 
         GOTO1 =V(PRINTER)                                                      
         B     ALCTMPM7            CHECK NEXT DATASET                           
*                                                                               
ALCTMPM5 DS    0H                                                               
*                                                                               
* NOTE: WE ALLOCATE THE TEMPORARY DATASETS BY INVOKING THE IBM DYNALLOC         
* MACRO, RATHER THAN BY CALLING DDDYNALLOC. THAT'S BECAUSE WE HAVE              
* MANY UNIQUE (AND ATYPICAL) ALLOCATION REQUIREMENTS IN THIS PROGRAM,           
* WHICH DON'T FIT EASILY WITHIN DDDYNALLOC'S RATHER CHALLENGING (AND            
* INEXTENSIBLE) PARAMETER LIST STRUCTURE.                                       
*                                                                               
         MVC   TXTDDNAME,TMPDDNAM  DDNAME                                       
         MVC   TXTPRIME,TMPPRIME   PRIMARY SPACE ALLOCATION                     
         MVC   TXTSECND,TMPSECND   SECONDARY SPACE ALLOCATION                   
         MVC   TXTSTATS,TMPSTATS   STATUS SPECIFICATION                         
*                                                                               
         MVC   TXTLRECL,=AL2(W_LMRECLQ)   COMMON "WORK RECORD" LRECL            
         CLI   MODE,DPCUME                                                      
         BNE   *+10                                                             
         MVC   TXTLRECL,=AL2(WD_LMRECLQ)  DPCUME WORK RECORD LRECL              
         OC    TMPLRECL,TMPLRECL   ANY OVERRIDE LRECL PROVIDED?                 
         BZ    *+10                                                             
         MVC   TXTLRECL,TMPLRECL   YES: USE IT                                  
*                                                                               
* ALL INPUT DATASETS REQUIRE THAT RECFM BE SET. IF THE DATASET IS ALSO          
* AN OUTPUT DATASET (I.E., IF DFSORT OPENS AND CLOSES IT BEFORE IT IS           
* READ), THEN WE DON'T NEED TO EXPLICITLY SET RECFM, BECAUSE DFSORT             
* TAKES CARE OF THAT FOR US. BUT IF WE'RE GOING TO READ IT (OR USE A            
* COUNT OPERATOR ON IT) AND IT'S NEVER BEEN OPENED, THEN IT MUST BE             
* ALLOCATED WITH AN EXPLICIT RECFM, OR ELSE THE READ WILL ABEND.                
*                                                                               
         XC    TEXT_UNIT_RECFM,TEXT_UNIT_RECFM   ASSUME NO RECFM=               
         MVC   TXTRECFM,TMPRECFM                 (BUT JUST IN CASE)             
         CLI   TXTRECFM,0                        RECFM= VALUE PRESENT?          
         BE    *+10                              NO                             
         MVC   TEXT_UNIT_RECFM,=AL2(DALRECFM)    YES: ADD TEXT UNIT             
*                                                                               
         XC    ATXTUDSN,ATXTUDSN   DEFAULT IS NO DSN (TEMP DATASET)             
         XC    ATXTNDSP,ATXTNDSP                                                
         XC    ATXTCDSP,ATXTCDSP                                                
         XC    ATXTRLSE,ATXTRLSE                                                
         CLC   DSNQUALS,SPACES     SHOULD WE CATALOG THE TEMP DATASETS?         
         BE    ALCTMPM6            NO                                           
*                                                                               
* DSN QUALIFIERS ARE:                                                           
*  1. HLQ = SUBMITTER'S USERID                                                  
*  2. NEXT QUALIFIER(S) ARE FROM WRKDSNQUAL PARAMETER CARD                      
*  3. FINAL QUALIFIER IS DDNAME                                                 
*                                                                               
         MVC   ATXTUDSN,=A(TEXT_UNIT_DSN)   YES: BUILD DSN                      
         MVC   TXTDSNAME,SPACES                                                 
         MVC   TXTDSNAME(4),RACFUSER   USERID IS ASSUMED TO BE 4 CHARS          
         MVI   TXTDSNAME+4,C'.'                                                 
         LLC   R1,DSNQUALL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TXTDSNAME+5(0),DSNQUALS   BUILD REMAINDER OF DSN                 
         LA    RF,TXTDSNAME+5(R1)                                               
         MVI   1(RF),C'.'                                                       
         MVC   2(L'TMPDDNAM,RF),TMPDDNAM                                        
         MVC   ATXTNDSP,=A(TEXT_UNIT_NDISP) DISP=(,CATLG,KEEP)                  
         MVI   TXTNDISP,CATLG                                                   
         MVC   ATXTCDSP,=A(TEXT_UNIT_CDISP)                                     
         MVI   TXTCDISP,KEEP                                                    
         MVC   ATXTRLSE,=A(TEXT_UNIT_RLSE)                                      
*                                                                               
ALCTMPM6 DS    0H                                                               
         LA    R1,ARBLK                                                         
         LR    R3,R1               R3=A(REQ BLK IN DUMP)                        
         DYNALLOC ,                                                             
         LR    R6,RF               R6=RETURN CODE IN DUMP                       
         L     R7,RBLKERR          R7=RBLKERR/RBLKINFO IN DUMP                  
         LTR   RF,RF               TEST FOR ERRORS                              
         BZ    ALCTMPM7            NO                                           
         ABEND 6,DUMP              TEMPFILE DYNALLOC FAILURE!                   
*                                                                               
ALCTMPM7 DS    0H                                                               
         LA    R4,TMPDDTLQ(R4)     BUMP TO NEXT DDNAME                          
         B     ALCTMPM3                                                         
         DROP  R4                                                               
*                                                                               
ALCTMPMX DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* DYNAMICALLY DECONCATENATE AND UNALLOCATE THE NIELSEN INPUT FILES.             
*                                                                               
DEALCINP NTR1                                                                   
*                                                                               
         MVC   P(43),=C'DECONCATENATING/DEALLOCATING INPUT DATASETS'            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'E',=CL8'TPQHIN'),(X'80',0)                  
         GOTO1 =V(DYNALLOC),DMCB,(C'U',=CL8'TPQHIN'),C'INFO'                    
*                                                                               
         CLI   MODE,DPCUME         MODE=DPCUME?                                 
         BE    DEALCI40            YES: SKIP PGNAM AND PGNAMUP FILES            
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'E',=CL8'PNQHIN'),(X'80',0)                  
         GOTO1 =V(DYNALLOC),DMCB,(C'U',=CL8'PNQHIN'),C'INFO'                    
         B     DEALCIX             SKIP DPCUME AND DPOPT FILES                  
*                                                                               
DEALCI40 DS    0H                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'E',=CL8'DPTCIN'),(X'80',0)                  
         GOTO1 =V(DYNALLOC),DMCB,(C'U',=CL8'DPTCIN'),C'INFO'                    
         GOTO1 =V(DYNALLOC),DMCB,(C'E',=CL8'DPTOIN'),(X'80',0)                  
         GOTO1 =V(DYNALLOC),DMCB,(C'U',=CL8'DPTOIN'),C'INFO'                    
*                                                                               
DEALCIX  DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* DYNAMICALLY UNALLOCATE TEMPORARY DATASETS.                                    
*                                                                               
DEALTMPM NTR1                                                                   
*                                                                               
         CLC   DSNQUALS,SPACES     DID WE CATALOG THE DATASETS?                 
         BNE   DEALTMPX            YES: LEAVE THEM THERE                        
*                                                                               
         MVC   P(36),=C'DEALLOCATING TEMPORARY WORK DATASETS'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R4,TMPDDTAB         DDNAME TABLE                                 
         USING TMPDDTBD,R4                                                      
*                                                                               
DEALTMP3 DS    0H                                                               
         CLI   0(R4),X'FF'         EOT?                                         
         BE    DEALTMPX                                                         
*                                                                               
         CLC   TMPDDNAM,=CL8'DPTOIN'                                            
         BE    DEALTMP5            DPTOIN WAS ALREADY DEALLOCATED               
*                                                                               
         LLC   R1,MODE             FOR THE CURRENT MODE...                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TMPMODES,0          ...WAS THIS DATASET ALLOCATED?               
         BZ    DEALTMP5            NO                                           
*                                                                               
         CLI   DEBUGSW,C'Y'        DEBUG=YES CARD WAS READ?                     
         BE    *+12                YES: NO NEED TO CHECK DEBUG ATTRIB.          
         TM    TMPMODES,DEBUG      NO: IS THIS A DEBUG DATASET?                 
         BO    DEALTMP5            YES: DON'T TRY TO DEALLOCATE IT              
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'U',TMPDDNAM)                                
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DEALTMP5                                                         
         ABEND 8                   DYNAMIC UNALLOCATION FAILURE                 
*                                                                               
DEALTMP5 DS    0H                                                               
         LA    R4,TMPDDTLQ(R4)     BUMP TO NEXT DDNAME                          
         B     DEALTMP3                                                         
         DROP  R4                                                               
*                                                                               
DEALTMPX DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* CALL ICETOOL TO PERFORM THE LOCAL MONTHLIES CALCULATIONS, AND TO              
* PRODUCE THE OUTPUT FILES TO BE FED INTO THE FINAL CONVERSION.                 
*                                                                               
CALLICE  NTR1                                                                   
*                                                                               
         CLI   CALICESW,C'Y'       SHOULD WE CALL ICETOOL?                      
         BE    CALLIC10            NO                                           
         MVC   P(40),=C'BYPASSING ICETOOL DUE TO ICETOOL=NO CARD'               
         GOTO1 =V(PRINTER)                                                      
         B     CALLICEX            JUST EXIT                                    
*                                                                               
CALLIC10 DS    0H                                                               
         MVC   P(36),=C'CALLING ICETOOL VIA TOOLIN INTERFACE'                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SR    R1,R1               USE TOOLIN INTERFACE                         
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               WERE ALL OPERATORS SUCCESSFUL?               
         BZ    CALLIC20            YES                                          
*                                                                               
         CHI   RF,8                DID A COUNT OPERATOR GET TRAPPED?            
         BE    CALLIC20            YES: THAT'S OKAY                             
*                                                                               
         MVI   ANYWARNS,C'Y'       REMEMBER: WE ISSUED A WARNING                
         CLI   WARNEFLG,C'N'       SEND WARNING E-MAIL...                       
         BE    CALLIC15            ...UNLESS SUPPRESSED VIA WARNMAIL=NO         
         MVC   WRNMSG1D,=C'CALC. FAILURE!'                                      
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',('WRNMSLQ1',WRNMSG1)                  
CALLIC15 DS    0H                                                               
         MVC   P(47),=C'*** ERROR *** NON-ZERO RETURN CODE FROM ICETOOL+        
               '                                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(44),=C'RECORDS FOR THIS MARKET ARE BEING DISCARDED.'           
         GOTO1 =V(PRINTER)                                                      
         B     CALLICEX            IGNORE THIS MARKET!                          
*                                                                               
CALLIC20 DS    0H                                                               
         MVC   P(23),=C'CALCULATIONS SUCCESSFUL'                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   CNTSA1SP,SPACES                                                  
         MVC   CNTSA1DF(8),=C'STAPARS)' PRINT THE NO. OF STATIONS FOUND         
*                                                                               
         LA    R1,ICEPAR1          COUNT OPERATOR VIA PARAMETER LIST            
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL ICETOOL CALL?                     
         JNZ   *+2                 NO: HOW CAN A COUNT OPERATOR FAIL?!?         
*                                                                               
         MVC   P+0(10),=C'XXX       '                                           
         MVC   P+0(L'PNA_HEADER_MARKET),PNA_HEADER_MARKET  MARKET #             
         MVI   P+3,C'/'                                                         
         MVC   P+4(L'PNA_HEADER_STREAM),PNA_HEADER_STREAM                       
         MVC   P+10(39),=C'# OF DISTRIBUTORS (STATIONS):      NNNN'             
         EDIT  CNT#RECS,(4,P+45),COMMAS=YES,ZERO=NOBLANK                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   CPYSA1DF(9),=C'STAPARS) ' TRACE THE STATION LIST                 
         MVC   CPYSA1DT,=C'STATIONS'     LOG DATASET                            
         LA    R1,ICEPAR2          COPY OPERATOR VIA PARAMETER LIST             
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL COPY OPERATION?                   
         JNZ   *+2                 NO: HOW CAN A COPY OPERATOR FAIL?!?          
*                                                                               
         CLI   MODE,NORMAL         MODE=NORMAL?                                 
         BNE   CALLIC25            NO                                           
         CLC   =C'NCM',SERVICE     SERVICE=NCM ?                                
         BNE   *+12                                                             
         LA    R3,NCMOUTLST        YES                                          
         B     CALLIC30                                                         
         LA    R3,NROUTLST                                                      
         B     CALLIC30                                                         
*                                                                               
CALLIC25 DS    0H                                                               
         CLI   MODE,PGNAMUP        MODE=PGNAMUP?                                
         BNE   *+12                                                             
         LA    R3,PUOUTLST                                                      
         B     CALLIC30                                                         
*                                                                               
         CLI   MODE,DPCUME         MODE=DPCUME?                                 
         JNE   *+2                 IMPOSSIBLE: INVALID MODE!                    
         LA    R3,DPOUTLST                                                      
*                                                                               
         USING LSTDDTBD,R3                                                      
CALLIC30 DS    0H                                                               
         CLI   0(R3),X'FF'         ANY MORE DDNAMES?                            
         BE    CALLIC40            NO                                           
*                                                                               
         MVC   CNTSA1SP,SPACES                                                  
         MVC   CNTSA1DF,LSTDD1MK   SINGLE-MARKET OUTPUT DDNAME                  
         LA    R1,CNTSA1DF+7       POINT TO LAST BYTE                           
         CLI   0(R1),C' '          LOOK FOR END OF DDNAME                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         MVI   1(R1),C')'          CLOSE DDNAME WITH RIGHT PARENTHESIS          
*                                                                               
         LA    R1,ICEPAR1          COUNT OPERATOR VIA PARAMETER LIST            
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL ICETOOL CALL?                     
         JNZ   *+2                 NO: HOW CAN A COUNT OPERATOR FAIL?!?         
*                                                                               
         MVC   P+0(10),=C'XXX       '                                           
         MVC   P+0(L'PNA_HEADER_MARKET),PNA_HEADER_MARKET  MARKET #             
         MVI   P+3,C'/'                                                         
         MVC   P+4(L'PNA_HEADER_STREAM),PNA_HEADER_STREAM                       
         MVC   P+10(34),=C'XXXXXXXX RECORD COUNT:      NNN,NNN,NNN'             
         MVC   P+10(8),LSTDD1MK       SINGLE-MARKET OUTPUT DDNAME               
         EDIT  CNT#RECS,(11,P+38),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 =V(PRINTER)                                                      
         AP    LSTRCNT,CNT#RECS    ADD TO RUNNING TOTAL                         
*                                                                               
*                                  APPEND DATA TO FINAL OUTPUT FILE             
         MVC   CPYSA1DF,LSTDD1MK   SINGLE-MARKET OUTPUT DDNAME                  
         MVI   CPYSA1DF+8,C')'     CLOSE DDNAME WITH RIGHT PARENTHESIS          
         MVC   CPYSA1DT,LSTDDALL   ALL-MARKET OUTPUT DDNAME                     
         LA    R1,ICEPAR2          COPY OPERATOR VIA PARAMETER LIST             
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL COPY OPERATION?                   
         JNZ   *+2                 NO: HOW CAN A COPY OPERATOR FAIL?!?          
*                                                                               
         LA    R3,LSTDDTLQ(R3)     BUMP TO NEXT OUTPUT DATASET                  
         B     CALLIC30                                                         
         DROP  R3                                                               
*                                                                               
CALLIC40 DS    0H                                                               
         MVC   P(45),=C'RECORDS SUCCESSFULLY APPENDED TO OUTPUT TAPES'          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*&&DO                                                                           
* REMOVED BY DEIS OCT/2018. WE NO LONGER RECEIVE LPM DMA PREVIEW DATA.          
         CLI   MODE,DPCUME         MODE=DPCUME?                                 
         BNE   CALLICEX                                                         
*                                                                               
         MVC   CNTSA1SP,SPACES                                                  
*                                  SEE DELMICEDPT FOR INFO RE: MRKTPARS         
         MVC   CNTSA1DF,=C'MRKTPARS' FIND OUT IF THIS DATASET IS EMPTY          
         MVI   CNTSA1DF+8,C')'     CLOSE DDNAME WITH RIGHT PARENTHESIS          
*                                                                               
         LA    R1,ICEPAR1          COUNT OPERATOR VIA PARAMETER LIST            
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL ICETOOL CALL?                     
         JNZ   *+2                 NO: HOW CAN A COUNT OPERATOR FAIL?!?         
         CP    CNT#RECS,=P'0'      IS MRKTPARS EMPTY?                           
         JE    *+2                 SHOULD NEVER BE EMPTY. THERE IS...           
*                                  ...NO LONGER LPM DMA PREVIEW DATA!           
*&&                                                                             
*                                                                               
CALLICEX DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* CHECK THE RECORD COUNTS OF THE FINAL OUTPUT FILES, TO MAKE SURE THAT          
* NONE OF THEM ARE EMPTY.                                                       
*                                                                               
CHKFINAL NTR1                                                                   
*                                                                               
         CLI   CALICESW,C'Y'       DID WE CALL ICETOOL?                         
         BNE   CHKFINX             NO: JUST EXIT                                
*                                                                               
         MVC   P(30),=C'PERFORMING FINAL RECORD COUNTS'                         
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   MODE,NORMAL         MODE=NORMAL?                                 
         BNE   CHKFIN05            NO                                           
         CLC   =C'NCM',SERVICE     SERVICE=NCM ?                                
         BNE   *+12                                                             
         LA    R3,NCMOUTLST        YES                                          
         B     CHKFIN10                                                         
         LA    R3,NROUTLST                                                      
         B     CHKFIN10                                                         
*                                                                               
CHKFIN05 DS    0H                                                               
         CLI   MODE,PGNAMUP        MODE=PGNAMUP?                                
         BNE   *+12                                                             
         LA    R3,PUOUTLST                                                      
         B     CHKFIN10                                                         
*                                                                               
         CLI   MODE,DPCUME         MODE=DPCUME?                                 
         JNE   *+2                 IMPOSSIBLE: INVALID MODE!                    
         LA    R3,DPOUTLST                                                      
*                                                                               
         USING LSTDDTBD,R3                                                      
CHKFIN10 DS    0H                                                               
         CLI   0(R3),X'FF'         ANY MORE DDNAMES?                            
         BE    CHKFINX             NO                                           
*                                                                               
         MVC   P(48),=C'XXXXXXXX EXPECTED RECORD COUNT:       NNN,NNN,N+        
               NN'                                                              
         MVC   P(8),LSTDDALL       ALL-MARKET OUTPUT DDNAME                     
         EDIT  LSTRCNT,(11,P+38),COMMAS=YES,ZERO=NOBLANK                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   CNTSA1SP,SPACES                                                  
         MVC   CNTSA1DF,LSTDDALL   ALL-MARKET OUTPUT DDNAME                     
         LA    R1,CNTSA1DF+7       POINT TO LAST BYTE                           
         CLI   0(R1),C' '          LOOK FOR END OF DDNAME                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         MVI   1(R1),C')'          CLOSE DDNAME WITH RIGHT PARENTHESIS          
*                                                                               
         LA    R1,ICEPAR1          COUNT OPERATOR VIA PARAMETER LIST            
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF               SUCCESSFUL ICETOOL CALL?                     
         JNZ   *+2                 NO: HOW CAN A COUNT OPERATOR FAIL?!?         
*                                                                               
         MVC   P(48),=C'XXXXXXXX ACTUAL   RECORD COUNT:       NNN,NNN,N+        
               NN'                                                              
         MVC   P(8),LSTDDALL       ALL-MARKET OUTPUT DDNAME                     
         EDIT  CNT#RECS,(11,P+38),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CHKTOTS,C'N'        DON'T CHECK GRAND TOTALS?                    
         BE    CHKFIN15            CORRECT (FOR DEBUGGING ONLY!)                
*                                                                               
         CP    LSTRCNT,CNT#RECS    GRAND TOTAL = INDIVIDUAL MKT TOTAL?          
         JNE   *+2                 NO: **FATAL** IN PRODUCTION MODE!            
*                                  A FAILURE HERE IN *PRODUCTION* MEANS         
*                                  THAT SOMETHING IS TERRIBLY WRONG.            
*                                  THE GRAND TOTAL RECORD COUNTS FOR            
*                                  ALL MARKETS MUST OBVIOUSLY MATCH             
*                                  THE RUNNING MARKET TOTALS. HOWEVER,          
*                                  FAILURES HERE WHILE TESTING ARE              
*                                  COMMONPLACE. IF THE OUTPUT DATASETS          
*                                  ARE "DUMMY" (FOR EXAMPLE), THEN THE          
*                                  GRAND TOTAL WILL BE ZERO, AND WE'LL          
*                                  NEVER MATCH. SIMILARLY, IF WE'RE             
*                                  WRITING TO TEST OUTPUT DATASETS THAT         
*                                  AREN'T EMPTY AT THE START, THEN THE          
*                                  GRAND TOTALS WILL BE TOO HIGH.               
*                                  PROVIDE A CHECKTOTS=NO CARD (FOR             
*                                  TESTING ONLY) TO BYPASS THIS CHECK.          
*                                                                               
CHKFIN15 DS    0H                                                               
         CP    CNT#RECS,=P'0'      ANY OUTPUT RECORDS?                          
         BNE   CHKFIN20            YES                                          
*                                                                               
         MVI   ANYWARNS,C'Y'       REMEMBER: WE ISSUED A WARNING                
         CLI   WARNEFLG,C'N'       NO: SEND WARNING E-MAIL...                   
         BE    CHKFIN20            ...UNLESS SUPPRESSED VIA WARNMAIL=NO         
         MVC   WRNMSG2D,LSTDDALL   DDNAME OF EMPTY DATASET                      
         MVC   WRNMSG2#,JOBID                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',('WRNMSLQ2',WRNMSG2)                  
*                                                                               
CHKFIN20 DS    0H                                                               
         LA    R3,LSTDDTLQ(R3)     BUMP TO NEXT OUTPUT DATASET                  
         B     CHKFIN10                                                         
         DROP  R3                                                               
*                                                                               
CHKFINX  DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
ARBLK    DC    A(RBLK)             FOR DYNALLOC                                 
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
RBLK     DS    0XL20               DYNALLOC REQUEST BLOCK                       
RBLKLEN  DC    AL1(20)             L'BLOCK                                      
RBLKVERB DC    AL1(S99VRBAL)       ALLOCATE BY DSN VERB CODE                    
RBLKFLG1 DC    XL2'00'                                                          
RBLKERR  DC    XL2'00'             ERROR CODE                                   
RBLKINFO DC    XL2'00'             INFO CODE                                    
RBLKATXT DC    A(TXTUNITS)         A(TEXT UNITS)                                
RBLKS99X DC    A(RBLKRBX)          A(REQUEST BLOCK EXTENSION)                   
RBLKFLAG DC    XL4'00'                                                          
*                                                                               
         DS    0F                  RBX MUST BE FULLWORD ALIGNED                 
RBLKRBX  DC    XL(S99RBXLN)'00'    REQUEST BLOCK EXTENSION                      
         ORG   RBLKRBX+(S99EID-S99RBX)                                          
         DC    CL6'S99RBX'         REQUEST BLOCK EXTENSION IDENTIFIER           
         ORG   RBLKRBX+(S99EVER-S99RBX)                                         
         DC    AL1(S99RBXVR)       VERSION                                      
         ORG   RBLKRBX+(S99EOPTS-S99RBX)                                        
         DC    AL1(S99EIMSG+S99EWTP) MESSAGES GO TO CONSOLE VIA WTO             
         ORG   RBLKRBX+(S99EMGSV-S99RBX)                                        
         DC    AL1(S99XSEVE)       ONLY PUT OUT SEVERE MESSAGES                 
         ORG                                                                    
*                                                                               
TXTUNITS DS    0A                  TEXT UNIT ADDRESS LIST                       
         DC    A(TEXT_UNIT_DACL)   DATA CLASS                                   
         DC    A(TEXT_UNIT_LRECL)  LRECL                                        
         DC    A(TEXT_UNIT_RECFM)  RECFM                                        
         DC    A(TEXT_UNIT_DDNAME) DDNAME                                       
ATXTUDSN DS    A                   DSN=                                         
ATXTNDSP DS    A                   NORMAL DISPOSITION                           
ATXTCDSP DS    A                   CONDITIONAL DISPOSITION                      
ATXTRLSE DS    A                   UNUSED SPACE RELEASE                         
         DC    A(TEXT_UNIT_UNIT)   UNIT=                                        
         DC    A(TEXT_UNIT_TRK)    SPACE=TRK                                    
         DC    A(TEXT_UNIT_PRIM)   PRIMARY SPACE                                
         DC    A(TEXT_UNIT_SEC)    SECONDARY SPACE                              
         DC    A(TEXT_UNIT_STCL)   STORAGE CLASS                                
         DC    A(TEXT_UNIT_STATS)  STATUS SPECIFICATION                         
         ORG   *-4                                                              
         DC    X'80'               EOL                                          
         ORG                                                                    
*                                                                               
TEXT_UNIT_DDNAME DC    AL2(DALDDNAM),AL2(1),AL2(L'TXTDDNAME)                    
TXTDDNAME        DS    CL8          DDNAME (SET PROGRAMMATICALLY)               
TEXT_UNIT_DSN    DC    AL2(DALDSNAM),AL2(1),AL2(L'TXTDSNAME)                    
TXTDSNAME        DS    CL44         DSNAME (SET PROGRAMMATICALLY)               
TEXT_UNIT_UNIT   DC    AL2(DALUNIT),AL2(1),AL2(L'TXTSYSDA)                      
TXTSYSDA         DC    C'SYSDA'     UNIT=SYSDA                                  
TEXT_UNIT_TRK    DC    AL2(DALTRK),AL2(0)   TRACKS                              
TEXT_UNIT_PRIM   DC    AL2(DALPRIME),AL2(1),AL2(L'TXTPRIME)                     
TXTPRIME         DS    AL3          PRIM. SPACE (SET PROGRAMMATICALLY)          
TEXT_UNIT_SEC    DC    AL2(DALSECND),AL2(1),AL2(L'TXTSECND)                     
TXTSECND         DS    AL3          SEC. SPACE (SET PROGRAMMATICALLY)           
TEXT_UNIT_LRECL  DC    AL2(DALLRECL),AL2(1),AL2(2)                              
TXTLRECL         DS    AL2          DCB: LRECL (SET PROGRAMMATICALLY)           
TEXT_UNIT_RECFM  DC    AL2(0),AL2(1),AL2(1)                                     
TXTRECFM         DS    AL1          DCB: RECFM (SET PROGRAMMATICALLY)           
TEXT_UNIT_STCL   DC    AL2(DALSTCL),AL2(1),AL2(L'TXTSTCL)                       
TXTSTCL          DC    C'SCTEMP'    STORCLAS=SCTEMP                             
TEXT_UNIT_DACL   DC    AL2(DALDACL),AL2(1),AL2(L'TXTDACL)                       
TXTDACL          DC    C'DCTEMP'    DATACLAS=DCTEMP                             
TEXT_UNIT_STATS  DC    AL2(DALSTATS),AL2(1),AL2(L'TXTSTATS)                     
TXTSTATS         DS    AL1          STATUS SPECIFICATION                        
TEXT_UNIT_NDISP  DC    AL2(DALNDISP),AL2(1),AL2(L'TXTNDISP)                     
TXTNDISP         DS    AL1          NORMAL DISPOSITION                          
TEXT_UNIT_CDISP  DC    AL2(DALCDISP),AL2(1),AL2(L'TXTCDISP)                     
TXTCDISP         DS    AL1          CONDITIONAL DISPOSITION                     
TEXT_UNIT_RLSE   DC    AL2(DALRLSE),AL2(0)                                      
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
WRNMSG1  DC    C'AUTONOTE*US-DEMOSTEAM,USOPSUP:'                                
WRNMSG1# DS    CL8                 JOBID                                        
         DC    C' WARNING! LOCAL MONTHLIES MKT '                                
WRNMSG1M DC    CL3' ',C'/'         MARKET NUMBER                                
WRNMSG1S DC    CL5' '              STREAM                                       
         DC    C' '                                                             
WRNMSG1D DS    CL14                WARNING DESCRIPTION                          
         DC    C' ('                                                            
WRNMSG1N DS    CL30                MARKET NAME                                  
         DS    C                   LEAVE ROOM FOR CLOSING PARENTHESIS           
WRNMSLQ1 EQU   *-WRNMSG1                                                        
*                                                                               
WRNMSG2  DC    C'AUTONOTE*US-DEMOSTEAM,USOPSUP:'                                
WRNMSG2# DS    CL8                 JOBID                                        
         DC    C' WARNING! LOCAL MONTHLIES FILE '                               
WRNMSG2D DC    C'XXXXXXXX',C' IS EMPTY!'    DDNAME                              
WRNMSLQ2 EQU   *-WRNMSG2                                                        
         SPACE 3                                                                
NFSFILES DCB   DDNAME=NFSFILES,DSORG=PS,MACRF=GM,RECFM=VB,             +        
               EODAD=ENDMKTS                                                    
*                                                                               
* THE LMIN DCB IS USED AS TO HOLD DCB PARAMETERS FOR DDDYNALLOC.                
* THIS DCB ITSELF IS NEVER OPENED.                                              
LMIN     DCB   DSORG=PS,RECFM=FB,MACRF=GM                                       
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
CARD     DS    CL80                                                             
WORK     DS    CL17                                                             
*                                                                               
RETCODE  DC    F'0'                RETURN CODE TO O/S                           
ACOMM    DS    A                   A(COMMUNICATIONS PARAMETER LIST)             
AMKTNAMS DS    A                   A(NEILSEN TV MARKET NAME TABLE)              
MKTNAMLN DS    H                   L'MARKET NAME TABLE ENTRY                    
MKTNAME  DS    CL30                SAVED MARKET NAME                            
DDNAME   DS    CL8                                                              
NUMMKTS  DS    PL8                 TOTAL NUMBER OF MARKETS TO PROCESS           
MKT#CNTR DC    PL8'1'              CURRENT RELATIVE MARKET NUMBER               
RACFUSER DS    CL8                 SUBMITTER'S RACF USERID                      
MODE     DS    X                   PROCESSING MODE (FOR FLAGS)                  
SERVICE  DS    CL(L'PNA_PARM_KEYWORD)  FROM SERVICE= PARAMETER CARD             
CALICESW DC    C'Y'                'Y' = CALL ICETOOL (SET BY ICETOOL=)         
DEBUGSW  DC    C'N'                'Y' = SET DEBUG MODE                         
WARNEFLG DC    C'Y'                'Y' = SEND WARNING E-MAILS                   
CHKTOTS  DC    C'Y'                'Y' = CHECK GRAND TOTALS                     
BADMRKT  DS    C                   'Y' = MARKET FAILED PRE-PROCESSING           
DSNQUALS DC    CL(L'PNA_PARM_VALUE)' '  WORK DATASET QUALIFIER(S)               
DSNQUALL DC    X'00'               NO. OF SIGNIF. CHARS. IN DSNQUALS            
ANYWARNS DC    C'N'                'Y' = A WARNING WAS ISSUED                   
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
JOBID    DS    CL8                 MVS JOB NUMBER                               
*                                                                               
RDW      DS    F                                                                
RECORD   DS    CL256                                                            
         EJECT                                                                  
*                                                                               
* NOTE: ALL DDNAMES MUST BE PRECISELY 8 BYTES LONG (SO THAT THE ICETOOL         
*       CONTROL STATEMENTS ARE BUILT CORRECTLY).                                
*                                                                               
NROUTLST DS    0D                   NORMAL MODE OUTPUT DDNAME LIST              
         DC    CL8'INTBOUT1',CL8'ITBOUTAL',PL8'0'   INTABS                      
PUOUTLST EQU   *                    PGNAMUP MODE OUTPUT DDNAME LIST             
         DC    CL8'PASRTAV1',CL8'PASRTAVG',PL8'0'                               
NCMOUTLST EQU  *                    SERVICE=NCM OUTPUT DDNAME LIST              
         DC    CL8'TPSRTAV1',CL8'TPSRTAVG',PL8'0'                               
         DC    X'FF'                                                            
*                                                                               
DPOUTLST DS    0D                   DPCUME MODE OUTPUT DDNAME LIST              
         DC    CL8'DPSRTAV1',CL8'DPSRTAVG',PL8'0'                               
         DC    X'FF'                                                            
*                                                                               
ICEPAR1  DC    A(0)                ICETOOL PARAMETER LIST                       
         DC    A(CNTSA1L)          A(COUNT STATEMENT)                           
         DC    A(CNTRETRN)         A(COUNT RETURN AREA)                         
         DC    X'FFFFFFFF'         EOL                                          
*                                                                               
ICEPAR2  DC    A(0)                ICETOOL PARAMETER LIST                       
         DC    A(CPYSA1L)          A(COPY STATEMENT)                            
         DC    A(0)                COPY RETURN AREA NOT NEEDED                  
         DC    X'FFFFFFFF'         EOL                                          
*                                                                               
CNTSA1L  DC    AL2(CNTSA1E-CNTSA1S)    L'STATEMENT                              
CNTSA1S  DC    C'COUNT '                                                        
CNTSA1US DS    C'USING(    )'          DFSORT PARAMETER "CNTL" DATASET          
         DC    C' FROM('                                                        
CNTSA1SP DC    CL(80-(*-CNTSA1S))' '   BLANK PAD TO 80-BYTES                    
         ORG   CNTSA1SP                                                         
CNTSA1DF DS    CL8                     'FROM' DDNAME                            
         DC    C')'                                                             
         ORG                                                                    
CNTSA1E  EQU   *                                                                
*                                                                               
CNTRETRN DC    AL2(CNTRETNE-CNTSTAT)                                            
CNTSTAT  DS    AL1                 COUNT OPERATION STATUS                       
CNT#RECS DS    PL8                 RETURNED RECORD COUNT                        
CNTRETNE EQU   *                                                                
*                                                                               
CPYSA1L  DC    AL2(CPYSA1E-CPYSA1S)    L'STATEMENT                              
CPYSA1S  DC    C'COPY FROM('                                                    
CPYSA1DF DC    C'XXXXXXXX'             'FROM' DDNAME                            
         DC    C' '                    SAVE ROOM FOR CLOSING PARENTHES.         
         DC    C' TO('                                                          
CPYSA1DT DC    C'XXXXXXXX'             'TO' DDNAME                              
         DC    C')'                                                             
         DC    CL(80-(*-CPYSA1S))' '   BLANK PAD TO 80-BYTES                    
CPYSA1E  EQU   *                                                                
*                                                                               
UTL      DS    0D                                                               
         DC    4X'00',X'0C'        UTL FOR DEMO SYSTEM                          
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL16'******SSB*******'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         EJECT                                                                  
*                                                                               
NORMAL   EQU X'80'                 "NORMAL" PROCESSING                          
DPCUME   EQU X'40'                 DPCUME PROCESSING                            
PGNAMUP  EQU X'20'                 PROGRAM NAME UPDATES                         
DEBUG    EQU X'01'                 DATASET IS FOR DEBUGGING ONLY                
                                                                                
         MACRO                                                                  
         TMPFL &DDNAME,&TRKS=,&RECFM=0,&DISP=NEW,&LRECL=0,             +        
               &NORMAL=YES,&DPCUME=NO,&PGNAMUP=,&DEBUG=NO                       
.*                                                                              
.* THE TMPFL MACRO DEFINES THE ATTRIBUTES OF TEMPORARY OUTPUT DATASETS          
.* NEEDED BY DFSORT/ICETOOL.                                                    
.*                                                                              
         LCLA  &MODES                                                           
.*                                                                              
&MODES   SETA  0                                                                
         AIF   ('&NORMAL'(1,1) EQ 'N').TMP10                                    
&MODES   SETA  &MODES+NORMAL                                                    
.TMP10   ANOP                                                                   
.*                                                                              
         AIF   ('&DPCUME'(1,1) EQ 'N').TMP20                                    
&MODES   SETA  &MODES+DPCUME                                                    
.TMP20   ANOP                                                                   
.*                                                                              
.* NORMAL=YES DEFAULTS TO PGNAMUP=YES                                           
.* NORMAL=NO DEFAULTS TO PGNAMUP=NO                                             
.*                                                                              
         AIF   (T'&PGNAMUP NE 'O').TMP30                                        
         AIF   ('&NORMAL'(1,1) EQ 'Y').TMP40                                    
         AGO   .TMP50                                                           
.TMP30   AIF   ('&PGNAMUP'(1,1) EQ 'N').TMP50                                   
.TMP40   ANOP                                                                   
&MODES   SETA  &MODES+PGNAMUP                                                   
.TMP50   ANOP                                                                   
.*                                                                              
         AIF   ('&DEBUG'(1,1) EQ 'N').TMP60                                     
&MODES   SETA  &MODES+DEBUG                                                     
.TMP60   ANOP                                                                   
.*                                                                              
         DC    CL8'&DDNAME'                                                     
         DC    AL3(&TRKS(1))                                                    
         DC    AL3(&TRKS(2))                                                    
         DC    AL2(&LRECL)                                                      
         DC    AL1(&RECFM)                                                      
         DC    AL1(&DISP)                                                       
         DC    AL1(&MODES)                                                      
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
*======================================================================         
*                                                                               
* ALL OF DFSORT'S TEMPORARY OUTPUT DATASETS ARE DEFINED IN THIS TABLE           
* VIA THE LOCAL MACRO TMPFL.                                                    
*                                                                               
* SOME OF THE MACRO PARAMETERS DEFINE DATASET ATTRIBUTES, AND OTHER             
* PARAMETERS INDICATE WHETHER OR NOT THE DATASET SHOULD BE ALLOCATED            
* AT ALL (GIVEN THE SYSIN PARAMETERS TO THIS PROGRAM, E.G. THE MODE).           
*                                                                               
*  DDNAME   :  REQUIRED (FIRST POSITIONAL PARAMETER)                            
*  TRKS=    :  REQUIRED SPACE ALLOCATION (PRIMARY,SECONDARY)                    
*               THE SPACE ALLOCATION MUST BE LARGE ENOUGH TO HANDLE             
*               THE LARGEST POSSIBLE MARKET/STREAM COMBINATION.                 
*  LRECL=   :  APPROPRIATE DEFAULT WILL BE USED IF OMITTED                      
*  DISP=MOD :  DEFAULT IS DISP=NEW                                              
*  NORMAL=  :  WHEN MODE=NORMAL, DATASET IS ALLOCATED BY DEFAULT UNLESS         
*               NORMAL=N                                                        
*  PGNAMUP= :  WHEN MODE=PGNAMUP, DATASET IS ALLOCATED IF AND ONLY IF           
*               IT IS ALSO ALLOCATED WHEN MODE=NORMAL, UNLESS                   
*               OVERRIDDEN WITH PGNAMUP=Y                                       
*  DPCUME=  :  DPCUME=Y MUST BE SPECIFIED FOR ANY DATASET NEEDED WHEN           
*               MODE=DPCUME                                                     
*  DEBUG=   :  FOR DEBUGGING. THIS GIVES US A CHANCE TO ALLOCATE OUTPUT         
*               FILES WHICH ARE "MODDED" ON TO OTHER OUTPUT DATASETS.           
*               DATASET IS ALLOCATED IF AND ONLY IF DEBUG=Y CARD IS             
*               PROVIDED.                                                       
*                                                                               
* ALL TEMPORARY DATASET ALLOCATIONS ARE:                                        
*  DDNAME=XXX,                                                                  
*  UNIT=SYSDA,                                                                  
*  SPACE=(TRK,(PRIM,SEC)),                                                      
*  STORCLAS=SCTEMP,                                                             
*  DATACLAS=DCTEMP,                                                             
*  DISP=NEW,         (CAN BE OVERRIDDEN TO MOD)                                 
*  DCB=(LRECL=N)                                                                
*      (RECFM=N FOR INPUT DATASETS)                                             
*                                                                               
*======================================================================         
*                                                                               
TMPDDTAB DS    0D                                                               
*                                                                               
*** PARSER                                                                      
 TMPFL MKTFILE,TRKS=(5,5),DPCUME=Y                                              
 TMPFL STAFILE,TRKS=(20,20),DPCUME=Y                                            
 TMPFL UERFILE,TRKS=(5,5),DPCUME=Y                                              
 TMPFL ITBFILE,TRKS=(5,5),DPCUME=Y                                              
 TMPFL HPTFILE,TRKS=(200,100),DPCUME=Y                                          
 TMPFL QHRFILE,TRKS=(10000,5000),DPCUME=Y                                       
 TMPFL TSAFILE,TRKS=(5000,1000),DPCUME=Y                                        
 TMPFL PMKFILE,TRKS=(5,5)                                                       
 TMPFL PNRFILE,TRKS=(5000,1000)                                                 
 TMPFL MRKTPARS,TRKS=(5,5),DPCUME=Y                                             
 TMPFL STAPARS,TRKS=(20,20),DPCUME=Y                                            
 TMPFL UERPARS,TRKS=(5,5)                                                       
 TMPFL ITBPARS,TRKS=(5,5)                                                       
 TMPFL HPTPARS,TRKS=(200,100),DPCUME=Y                                          
 TMPFL QHRPARS,TRKS=(10000,5000),DPCUME=Y                                       
 TMPFL TSAPARS,TRKS=(5000,1000),DPCUME=Y                                        
 TMPFL PMKPARS,TRKS=(5,5)                                                       
 TMPFL PNRPARS,TRKS=(5000,1000)                                                 
 TMPFL QHROUT,TRKS=(10000,5000),DISP=MOD,DPCUME=Y                               
 TMPFL UEROUT,TRKS=(5,5)                                                        
 TMPFL INTBOUT1,TRKS=(5,5),RECFM=FB                                             
 TMPFL HPTOUT,TRKS=(200,100),DPCUME=Y                                           
 TMPFL PAVOUT,TRKS=(10000,2000)                                                 
 TMPFL PUPOUT,TRKS=(50,50),NORMAL=N,PGNAMUP=Y                                   
 TMPFL PRSWRK1,TRKS=(50,50),DPCUME=Y                                            
 TMPFL PRSWRK2,TRKS=(10000,5000),DPCUME=Y                                       
 TMPFL PRSWRK3,TRKS=(10000,5000)                                                
 TMPFL PRSWRK4,TRKS=(10000,5000)                                                
*                                                                               
*** TP/PAV AVERAGING                                                            
 TMPFL TPPLCOMB,TRKS=(20000,5000)                                               
 TMPFL TPWORK1,TRKS=(10000,5000)                                                
 TMPFL TPWORK2,TRKS=(3000,1000)                                                 
 TMPFL TPWORK3,TRKS=(3000,1000)                                                 
 TMPFL TPWORK4,TRKS=(3000,1000)                                                 
 TMPFL TPWORK10,TRKS=(1000,1000),DISP=MOD                                       
 TMPFL TPWRK10A,TRKS=(1000,1000),DEBUG=Y                                        
 TMPFL TPWRK10B,TRKS=(1000,1000),DEBUG=Y                                        
 TMPFL TPALLAVG,TRKS=(5000,5000),DISP=MOD                                       
 TMPFL TPAVGWK1,TRKS=(10000,5000),DEBUG=Y                                       
 TMPFL TPAVGWK2,TRKS=(10000,5000),DEBUG=Y                                       
 TMPFL TPAVGWK3,TRKS=(10000,5000),DEBUG=Y                                       
 TMPFL TPAVGWK4,TRKS=(10000,5000),DEBUG=Y                                       
 TMPFL TPAVGWK5,TRKS=(10000,5000),DEBUG=Y                                       
 TMPFL TPHUTPUT,TRKS=(100,100),DISP=MOD                                         
 TMPFL TPHUTPTA,TRKS=(100,100),DEBUG=Y                                          
 TMPFL TPHUTPTB,TRKS=(100,100),DEBUG=Y                                          
 TMPFL TPHUTAVG,TRKS=(100,100)                                                  
 TMPFL TPALLAV1,TRKS=(5000,5000),DISP=MOD                                       
 TMPFL TPALAV1A,TRKS=(5000,5000),DEBUG=Y                                        
 TMPFL TPALAV1B,TRKS=(5000,5000),DEBUG=Y                                        
 TMPFL TPALLAV2,TRKS=(5000,5000),DISP=MOD                                       
 TMPFL TPALAV2A,TRKS=(5000,5000),DEBUG=Y                                        
 TMPFL TPALAV2B,TRKS=(5000,5000),DEBUG=Y                                        
 TMPFL TPALLAV3,TRKS=(5000,5000),DISP=MOD                                       
 TMPFL TPALAV3A,TRKS=(5000,5000),DEBUG=Y                                        
 TMPFL TPALAV3B,TRKS=(5000,5000),DEBUG=Y                                        
 TMPFL TPPLCMBT,TRKS=(5000,2000)                                                
 TMPFL TPPLCMBW,TRKS=(5000,2000)                                                
 TMPFL TPPLCMBS,TRKS=(5000,2000)                                                
 TMPFL NORPROGS,TRKS=(100,50)                                                   
 TMPFL PAVDYTEL,TRKS=(500,100)                                                  
 TMPFL PAVDYNOR,TRKS=(200,50)                                                   
 TMPFL PAVMDTL1,TRKS=(100,50)                                                   
 TMPFL PAVMDTL2,TRKS=(50,50)                                                    
 TMPFL PAVALL,TRKS=(500,100),DISP=MOD                                           
 TMPFL PAVALLA,TRKS=(500,100),DEBUG=Y                                           
 TMPFL PAVALLB,TRKS=(500,100),DEBUG=Y                                           
 TMPFL PAVALLC,TRKS=(500,100),DEBUG=Y                                           
 TMPFL PAVALLD,TRKS=(500,100),DEBUG=Y                                           
 TMPFL PAVALLE,TRKS=(500,100),DEBUG=Y                                           
 TMPFL PAVALLF,TRKS=(500,100),DEBUG=Y                                           
 TMPFL PAVAVG,TRKS=(500,100)                                                    
 TMPFL PAVAV1,TRKS=(500,100)                                                    
 TMPFL PAVWRK1,TRKS=(5000,500)                                                  
 TMPFL PAVWRK2,TRKS=(50,50)                                                     
 TMPFL PAVWRK3,TRKS=(3000,500)                                                  
 TMPFL PAVWRK4,TRKS=(50,50)                                                     
 TMPFL PAVWRK5,TRKS=(3000,500)                                                  
 TMPFL REPTIMES,TRKS=(200,100)                                                  
 TMPFL MKTTOT,TRKS=(100,100)                                                    
 TMPFL TPWORK5,TRKS=(10000,2000)                                                
 TMPFL TPWORK6,TRKS=(200,100)                                                   
 TMPFL TPWORK7,TRKS=(200,100)                                                   
 TMPFL TPWORK8,TRKS=(100,100)                                                   
 TMPFL TPWORK9,TRKS=(100,100)                                                   
 TMPFL TPWORK11,TRKS=(500,500)                                                  
 TMPFL TPWORK12,TRKS=(100,100)                                                  
 TMPFL TPSRTAV1,TRKS=(10000,5000),RECFM=FB                                      
 TMPFL PASRTAV1,TRKS=(500,200),RECFM=FB                                         
*                                                                               
*** TRUE WEEKLIES                                                               
 TMPFL TWHUTAVG,TRKS=(500,100)                                                  
 TMPFL TWPLCOMB,TRKS=(10000,5000),DISP=MOD                                      
 TMPFL TWPLCMBA,TRKS=(20000,5000),DEBUG=Y                                       
 TMPFL TWPLCMBB,TRKS=(5000,2000),DEBUG=Y                                        
 TMPFL TWPLCMBC,TRKS=(500,100),DEBUG=Y                                          
 TMPFL TWALLAV1,TRKS=(10000,5000),DISP=MOD                                      
 TMPFL TWALAV1A,TRKS=(10000,5000),DEBUG=Y                                       
 TMPFL TWALAV1B,TRKS=(500,100),DEBUG=Y                                          
 TMPFL TWALLAV2,TRKS=(10000,5000)                                               
 TMPFL TWALLAV3,TRKS=(10000,5000),DISP=MOD                                      
 TMPFL TWALAV3A,TRKS=(10000,5000),DEBUG=Y                                       
 TMPFL TWALAV3B,TRKS=(5000,1000),DEBUG=Y                                        
 TMPFL TWALAV3C,TRKS=(10000,5000),DEBUG=Y                                       
 TMPFL TWWORK9,TRKS=(500,100),DISP=MOD                                          
 TMPFL TWWORK9A,TRKS=(500,100),DEBUG=Y                                          
 TMPFL TWWORK9B,TRKS=(500,100),DEBUG=Y                                          
*                                                                               
*** DAYPART CUMES                                                               
*    DPTOIN IS AN *INPUT* DATASET. THIS ALLOCATION OCCURS ONLY IF THERE         
*    IS NO DPOPT FILE PRESENT FOR THE CURRENT MARKET.                           
*                                                                               
*  NOTE: DPOPT FILES WERE DISCONTINUED EFFECTIVE JAN/2016. AT SOME              
*        POINT, WE MIGHT CHOOSE TO REMOVE ALL OF THE CODE PERTAINING            
*        TO THE DPOPT FILES. THAT MEANS REMOVING SOME ALLOCATIONS FROM          
*        THIS TABLE, AND CHANGING THE CODE IN DELMICEDPT WHICH RELIES           
*        UPON THE PRESENCE OF DPTOIN (EVEN THOUGH DPTOIN HAS BEEN EMPTY         
*        SINCE 2016).                                                           
 TMPFL DPTOIN,TRKS=(1,1),RECFM=FB,DISP=MOD,DPCUME=Y,NORMAL=N                    
 TMPFL PRSMKTF,TRKS=(10,10),DPCUME=Y,NORMAL=N                                   
 TMPFL PRSMKTFO,TRKS=(200,100),DPCUME=Y,NORMAL=N                                
 TMPFL PRSSTAF,TRKS=(20,20),DPCUME=Y,NORMAL=N                                   
 TMPFL PRSUERF,TRKS=(10,10),DPCUME=Y,NORMAL=N                                   
 TMPFL PRSHPTF,TRKS=(50,20),DPCUME=Y,NORMAL=N                                   
 TMPFL PRSHPTFO,TRKS=(200,100),DPCUME=Y,NORMAL=N                                
 TMPFL PRSDPTF,TRKS=(200,100),DPCUME=Y,NORMAL=N                                 
 TMPFL PRSDPTFO,TRKS=(200,100),DPCUME=Y,NORMAL=N                                
 TMPFL CUMWK1,TRKS=(20,10),DPCUME=Y,NORMAL=N                                    
 TMPFL CUMWK2,TRKS=(500,100),DPCUME=Y,NORMAL=N                                  
 TMPFL CUMWKO1,TRKS=(50,50),DPCUME=Y,NORMAL=N                                   
 TMPFL HPWORK,TRKS=(10,10),DPCUME=Y,NORMAL=N                                    
 TMPFL HPWORKO,TRKS=(50,50),DPCUME=Y,NORMAL=N                                   
 TMPFL CUMESF,TRKS=(30000,5000),DISP=MOD,DPCUME=Y,NORMAL=N                      
 TMPFL UNIVF,TRKS=(10,10),DPCUME=Y,NORMAL=N                                     
 TMPFL HUTPUTF,TRKS=(500,500),DISP=MOD,DPCUME=Y,NORMAL=N                        
 TMPFL MKTINPF,TRKS=(5,5),DPCUME=Y,NORMAL=N                                     
 TMPFL MKTINPFO,TRKS=(50,50),DPCUME=Y,NORMAL=N                                  
 TMPFL STAINPF,TRKS=(200,100),DPCUME=Y,NORMAL=N                                 
 TMPFL UERINPF,TRKS=(10,10),DPCUME=Y,NORMAL=N                                   
 TMPFL HPTINPF,TRKS=(50,50),DPCUME=Y,NORMAL=N                                   
 TMPFL HPTINPFO,TRKS=(50,50),DPCUME=Y,NORMAL=N                                  
 TMPFL DPTINPF,TRKS=(500,200),DPCUME=Y,NORMAL=N                                 
 TMPFL DPTINPFO,TRKS=(50,50),DPCUME=Y,NORMAL=N                                  
 TMPFL INPFILE,TRKS=(200,100),DPCUME=Y,NORMAL=N                                 
 TMPFL INPFILEO,TRKS=(50,50),DPCUME=Y,NORMAL=N                                  
 TMPFL DPOUT,TRKS=(500,200),DISP=MOD,DPCUME=Y,NORMAL=N                          
 TMPFL DPWORK1,TRKS=(500,100),DPCUME=Y,NORMAL=N                                 
 TMPFL DPWORK1W,TRKS=(500,200),DPCUME=Y,NORMAL=N                                
 TMPFL DPWORK2,TRKS=(50,50),DPCUME=Y,NORMAL=N                                   
 TMPFL DPWORK3,TRKS=(100,100),DPCUME=Y,NORMAL=N                                 
 TMPFL DPWORK3W,TRKS=(200,100),DPCUME=Y,NORMAL=N                                
 TMPFL DPWORK4,TRKS=(50,50),DPCUME=Y,NORMAL=N                                   
 TMPFL DPSRTAV1,TRKS=(500,200),RECFM=FB,DPCUME=Y,NORMAL=N                       
*                                                                               
*** PROGRAM UPDATES                                                             
 TMPFL PLORGSRT,TRKS=(300,100),NORMAL=N,PGNAMUP=Y                               
 TMPFL PLWORK1,TRKS=(300,100),NORMAL=N,PGNAMUP=Y                                
 TMPFL PLWORK2,TRKS=(300,100),NORMAL=N,PGNAMUP=Y                                
 TMPFL PLNEW,TRKS=(10000,2000),DISP=MOD,NORMAL=N,PGNAMUP=Y                      
 TMPFL PLNEWA,TRKS=(10000,2000),NORMAL=N,PGNAMUP=Y,DEBUG=Y                      
 TMPFL PLNEWB,TRKS=(10000,2000),NORMAL=N,PGNAMUP=Y,DEBUG=Y                      
*                                                                               
         DC    X'FF'               EOT                                          
         EJECT                                                                  
*                                                                               
TMPDDTBD DSECT ,                                                                
TMPDDNAM DS    CL8                 DDNAME                                       
TMPPRIME DS    AL3                 PRIMARY SPACE ALLOCATION                     
TMPSECND DS    AL3                 SECONDARY SPACE ALLOCATION                   
TMPLRECL DS    AL2                 LRECL                                        
TMPRECFM DS    XL1                 RECFM                                        
TMPSTATS DS    XL1                 STATUS SPECIFICATION                         
TMPMODES DS    XL1                 SHOWS FOR WHICH MODE(S) WE NEED THIS         
TMPDDTLQ EQU   *-TMPDDTBD                                                       
*                                                                               
*** THESE ARE IBM-MANDATED EQU VALUES                                           
FB       EQU   X'10'+X'80'         RECFM=FB                                     
VB       EQU   X'10'+X'40'         RECFM=VB                                     
OLD      EQU   X'01'               DISP=OLD                                     
MOD      EQU   X'02'               DISP=MOD                                     
NEW      EQU   X'04'               DISP=NEW                                     
SHR      EQU   X'08'               DISP=SHR                                     
CATLG    EQU   X'02'               DISP=(,CATLG)                                
KEEP     EQU   X'08'               DISP=(,KEEP)                                 
*                                                                               
LSTDDTBD DSECT ,                                                                
LSTDD1MK DS    CL8                 SINGLE-MARKET DDNAME                         
LSTDDALL DS    CL8                 ALL-MARKET DDNAME                            
LSTRCNT  DS    PL8                 RUNNING RECORD COUNT                         
LSTDDTLQ EQU   *-LSTDDTBD                                                       
         EJECT                                                                  
* ++INCLUDE DELMDSECT                                                           
* ++INCLUDE DEDEMTABD                                                           
         PRINT OFF                                                              
       ++INCLUDE DELMDSECT                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 2                                                                
         IEFZB4D0                                                               
         IEFZB4D2                                                               
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         IHAPSA                                                                 
         IHAACEE                                                                
         IHAASCB                                                                
         IHAASXB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         SPACE 2                                                                
* ++INCLUDE DDDPRINT                                                            
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044DELMCALC  02/20/20'                                      
         END                                                                    
