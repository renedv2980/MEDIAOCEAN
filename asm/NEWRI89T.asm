*          DATA SET NEWRI89T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEWRI89    AT LEVEL 093 AS OF 05/02/98                      
*PHASE T32089B,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*                                                                               
         TITLE 'T32089 - NETWORK UNIT HISTORY UPDATE'                           
************************************************************                    
*                                                                               
* THIS REPORT READS RECOVERY FILE AND PRINTS HISTORY OF UNIT CHANGES            
*                                                                               
*************************************************************                   
         SPACE 2                                                                
T32089   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T32089*,RA                                                    
         USING T32089,RB,RA                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
                                                                                
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
*                                                                               
         L     R1,ANETWS1        ANETWS1 AND 2  FOR CLIENT RECORD               
         ST    R1,NBACLI                                                        
*                                                                               
         L     R7,ANETWS3                                                       
         USING MYWORKD,R7                                                       
                                                                                
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    RE,HEDSPECS                                                      
         ST    RE,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'165'                                                 
         L     R1,BOXAWIDE                                                      
         ST    R1,ADRWIDE                                                       
         DROP  R1                                                               
*                                                                               
         L     R5,=A(MYIO)                                                      
         USING RECD,R5                                                          
         LA    RE,28(R5)           POINT PAST RECV HEADER                       
         ST    RE,AUNITREC         UNIT RECORD POINTER                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    MAINLINE                                                         
EXIT     XIT1                                                                   
                                                                                
                                                                                
         EJECT                                                                  
*                                                                               
MAINLINE DS    0H                                                               
         SPACE                                                                  
         BAS   RE,INIT         INITIALIZE                                       
         BAS   RE,NET1         READ RECOVERY                                    
         CLOSE (RECVIN)        CLOSE RECOVERY                                   
         FREEPOOL RECVIN                                                        
         BAS   RE,EXRPT        WRITE REPORT                                     
*                                                                               
         B     EXIT                                                             
*                                                                               
COPY     EQU   1                                                                
CHANGE   EQU   2                                                                
ADDS     EQU   3                                                                
*                                                                               
         EJECT                                                                  
********************************************************                        
* GETS RECORDS FROM THE RECOVERY FILE                                           
* IF SIGNIFICANT CHANGE SEND TO SORTER                                          
*                                                                               
********************************************************                        
         SPACE 2                                                                
NET1     NTR1                                                                   
*                                                                               
*                                                                               
GET      L     R5,=A(MYIO)         REESTABLISH R5                               
         USING RECD,R5                                                          
*                                                                               
         LA    R1,RECVIN                                                        
         L     R0,=A(MYIO)                                                      
         GET   (1),(0)                                                          
                                                                                
         CLI   SKIPCHNG,C'Y'       SKIPCHNG=Y MEANS NO AUDIT FLAG ON            
         BNE   NET00               PREVIOUS COPY SO SKIP THIS CHANGE            
         MVI   SKIPCHNG,0          CLEAR FLAG                                   
         B     GET                 SKIP THIS CHANGE                             
                                                                                
         SPACE                                                                  
NET00    CLI   RFILTY,X'2A'       TEST UNTFILE                                  
         BNE   GET                                                              
                                                                                
* CAN HAVE PREVIOUS NIGHT'S LATE OFF-LINE RUNS ON RECOVERY FILE                 
***      CLC   RDATE,TODAYB        ONLY TODAY'S REC                             
***      BNE   GET                                                              
         CLI   RTASKID,X'FF'       BACKED OUT AFTER A DUMP                      
         BE    GET                 YES/SKIP IT                                  
                                                                                
         CLI   RPRG,X'01'          PFM ?                                        
         BE    GET                 SKIP IT                                      
                                                                                
         L     R6,AUNITREC         POINT TO UNIT RECORD - R6                    
         USING NURECD,R6                                                        
         CLI   NUKTYPE,X'04'       TEST UNIT RECORD                             
         BNE   GET                                                              
         CLI   NUKSUB,X'C1'        SKIP TRAFFIC RECS                            
         BNL   GET                                                              
                                                                                
***************************************************                             
                                                                                
         B     SKIPTHIS                                                         
                                                                                
         CLC   NUKDATE,=X'C4B6'                                                 
         BNE   SKIPTHIS                                                         
         CLC   =C'NBC',NUKNET                                                   
         BNE   SKIPTHIS                                                         
         CLI   NUKEST,68                                                        
         BNE   SKIPTHIS                                                         
         CLI   NUKSUB,1                                                         
         BNE   SKIPTHIS                                                         
         CLC   =C'8HOMIC',NUKPROG                                               
         BNE   SKIPTHIS                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'TT1',RECVHDR,C'DUMP',100,=C'1D'               
         B     GET                                                              
                                                                                
         TM    NUPACKST,X'02'                                                   
         BNO   GET                                                              
                                                                                
         CLI   NUPACK,3                                                         
         BNE   GET                                                              
                                                                                
                                                                                
                                                                                
         GOTO1 =V(PRNTBL),DMCB,=C'TT1',RECVHDR,C'DUMP',500,=C'1D'               
         GOTO1 =V(PRNTBL),DMCB,=C'TT2',RECVHDR+500,C'DUMP',500,=C'1D'           
         GOTO1 =V(PRNTBL),DMCB,=C'TT3',RECVHDR+1000,C'DUMP',500,=C'1D'          
SKIPTHIS DS    0H                                                               
*****************************************************                           
                                                                                
* SET ZERO ELEMENT CODE AT E-O-R                                                
         LH    R1,REC-4                                                         
         LA    R1,REC-4(R1)                                                     
         XC    0(2,R1),0(R1)                                                    
                                                                                
* - TEST IF AGENCY                                                              
         CLC   NUKAM,AMSAVE        IS IT REQUESTED AGENCY ?                     
         BNE   GET                                                              
* - TEST IF CLIENT                                                              
         CLI   ONECLT,0            IS IT REQUESTED CLIENT ?                     
         BE    NET10                                                            
         CLC   NUKCLT,ONECLT                                                    
         BNE   GET                                                              
                                                                                
NET10    DS    0H                                                               
* TEST **********************************************                           
*8       CLC   =C'NBC',NUKNET                                                   
**       BNE   GET                                                              
**       CLC   =X'C4D0',NUKDATE                                                 
**       BNE   GET                                                              
**       CLI   NUKSUB,1                                                         
**       BH    GET                                                              
**       CLI   NUKEST,68                                                        
**       BNE   GET                                                              
*8       GOTO1 =V(PRNTBL),DMCB,=C'RRR',RECVHDR,C'DUMP',100,=C'1D'               
* *****************************************************                         
         B     NET13                                                            
                                                                                
         EJECT                                                                  
NET13    L     R6,AUNITREC                                                      
         USING NURECD,R6                                                        
         MVC   RECDAT,RDATE        SAVE RECOVERY DATE                           
         MVC   RECTIME,RTIME       SAVE RECOVERY TIME                           
         MVC   RECFILTY,RECVHDR                                                 
                                                                                
         CLI   RRECTY,COPY         TEST IF COPY                                 
         BE    NET15                                                            
         CLI   RRECTY,CHANGE       TEST IF CHA                                  
         BE    NET20                                                            
         CLI   RRECTY,ADDS         TEST IF ADD                                  
         BE    NET30                                                            
         DC    H'0'                                                             
         DROP  R5                                                               
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************                     
* - COPY                                                                        
***********************************************************                     
NET15    DS    0H                                                               
                                                                                
         TM    NUPACKST,X'02'      HISTORY FLAG TURNED ON ?                     
         BO    NET15B              YES                                          
         MVI   SKIPCHNG,C'Y'       NO/SO SKIP THIS AND ITS CHANGE               
         B     GET                                                              
                                                                                
                                                                                
NET15B   MVC   PREVIOUS,RECFILTY   SAVE KEY OF CURRENT COPY                     
         MVI   PREVREC,COPY        SET FLAG                                     
*                                                                               
NET17    DS    0H                  PROCESS CURRENT COPY                         
         L     R4,=A(COPYTBL)                                                   
         MVI   BYTE,C'D'           PASS DELETE AS STATUS                        
         BAS   RE,DOHISTRY                                                      
         B     GET                                                              
                                                                                
         EJECT                                                                  
***********************************************************                     
* - CHANGE                                                                      
*                                                                               
***********************************************************                     
NET20    DS    0H                                                               
         CLI   PREVREC,COPY       PREVIOUS REC MUST BE A COPY                   
         BE    *+12                                                             
         BAS   RE,CHANGERR         ELSE PRINT THIS REC AS ERROR                 
         B     GET                 AND GET NEXT REC                             
         MVI   PREVREC,0                                                        
         L     R4,=A(CHANGTBL)                                                  
         MVI   BYTE,C'A'           PASS ADD AS STATUS                           
         BAS   RE,DOHISTRY         PUT HISTRY DATA TO CHANGTBL                  
         BAS   RE,COMPARCT         COMPARE COPY TO CHANGE                       
         CLI   BYTE,0              WAS UNIT CHANGE SIGNIFICANT?                 
         BE    GET                 NO                                           
*                                  YES                                          
* SET COPY AND CHANGE TO SORTER                                                 
*                                                                               
         L     R4,=A(CHANGTBL)     IF CHANGE IS 'X' REASON                      
         USING HISTDATD,R4                                                      
         CLI   HIREAS,C'X'         REASON CODE = 'X' ?                          
         BE    GET                 SKIP BOTH COPY AND CHANGE                    
                                                                                
         CLI   HISTPRMT,C'Y'       IF CHANGE IS PREEMPT                         
         BE    SKIPCOPY            SKIP COPY, ONLY SHOW CHANGE                  
                                                                                
         CLI   BYTE,C'G'           IF CHANGE WAS MKGD BY ?                      
         BNE   *+12                                                             
         MVI   HISTSTAT,C'S'       DELETE STATUS (SPECIAL TO SKIP LINE)         
         B     SKIPCOPY            SHOW CHANGE ONLY                             
         DROP  R4                                                               
*                                                                               
         L     R4,=A(COPYTBL)      PROCESS COPY/CHANGE                          
         BAS   RE,MVSRTBL          MOVE DATA TO SORTREC/PUT TO SORTER           
SKIPCOPY L     R4,=A(CHANGTBL)                                                  
         BAS   RE,MVSRTBL          MOVE DATA TO SORTREC/PUT TO SORTER           
         B     GET                 GET NEXT RECORD                              
                                                                                
         EJECT                                                                  
************************************                                            
*                                                                               
* MOVE COPY/CHANGTBL DATA TO SORTREC                                            
* R4 ---> COPY/CHANG TABLE                                                      
*************************************                                           
*                                                                               
MVSRTBL  NTR1                                                                   
         USING HISTDATD,R4                                                      
         MVC   SRTKCLT,HIUNTKEY+1     SET A/M,CLT                               
         MVC   SRTKGRP,HISTGRP        AUDIT GROUP                               
         MVC   SRTKPRG,HIUNTKEY+11    PROGRAM                                   
         MVC   SRTKNET,HIUNTKEY+7     NETWORK                                   
         MVC   SRTKEST,HIUNTKEY+17    ESTIMATE                                  
         MVC   SRTKDAT,HIUNTKEY+4     AIR DATE                                  
         MVC   SRTKSUB,HIUNTKEY+18    SUBLINE                                   
         MVC   SRTKCHD,HIRECDAT       RECOVERY DATE (BINARY)                    
         MVC   SRTKTIM,HIRECTM        RECOVERY TIME                             
         OI    SRTKTIM+3,X'0F'                                                  
*                                                                               
         MVC   SRUNTDAT,HIUNTKEY+4    AIR DATE                                  
         MVC   SRPRGNM,HISTPNAM                                                 
         MVC   SRROT,HISTROT                                                    
         MVC   SRTIM,HISTIME                                                    
         MVC   SRLEN,HISTLEN                                                    
         MVC   SRCOST,HISTCOST                                                  
         MVC   SRPROD,HISTPRD                                                   
         MVC   SRREASON,HIREAS                                                  
         MVC   SRCOMN,HISTCOMN                                                  
         MVC   SRBUYER,HIUSER                                                   
         MVC   SRTPKG,HISTPKG                                                   
*                                                                               
         XC    SRMKGMSD,SRMKGMSD   SET MISSED OR MKGD                           
         CLI   HISTMIS,0                                                        
         BE    *+14                                                             
         MVC   SRMKGMSD(32),HISTMIS                                             
         B     MVT10                                                            
         CLI   HISTMKD,0                                                        
         BE    MVT10                                                            
         MVC   SRMKGMSD(32),HISTMKD                                             
*                                                                               
                                                                                
MVT10    MVC   SRPRMT,HISTPRMT                                                  
         MVC   SRTYPE,HISTSTAT                                                  
         MVC   SRTAMC,HIUNTKEY+1     SET A/M,CLT                                
*                                                                               
         BAS   RE,CHKTMSUB                                                      
         GOTO1 SORTER,DMCB,=C'PUT',SRTTBL                                       
*                                                                               
         B     EXIT                                                             
                                                                                
                                                                                
************************************************************                    
CHKTMSUB NTR1       IF TIME OF PREV REC=TIME OF THIS REC                        
*                                          BUMP SUB TIME                        
                                                                                
         MVI   SRTKTSUB,0          SET SUB LINE                                 
         OC    TIMESV,TIMESV       FIRST TIME?                                  
         BZ    CTM30               YES                                          
         CLC   TIMESV,SRTKTIM      NO   SAME TIME AS PREVIOUS ?                 
         BNE   CTM30                                                            
         ZIC   R1,TIMSUB           YES SO BUMP SUB TIME                         
         LA    R1,1(R1)                                                         
         STC   R1,TIMSUB                                                        
         STC   R1,SRTKTSUB                     SET SUB LINE                     
         B     CTMX                            RETURN                           
                                                                                
CTM30    MVC   TIMESV,SRTKTIM       SAVE NEW TIME                               
         MVI   TIMSUB,0                                                         
*                                                                               
CTMX     B     EXIT                                                             
                                                                                
         EJECT                                                                  
                                                                                
*********************************************************                       
* ADD                                                                           
*                                                                               
*********************************************************                       
NET30    DS    0H                                                               
         TM    NUPACKST,X'02'      HISTORY FLAG TURNED ON ?                     
         BNO   GET                 NO -  IGNORE THIS REC                        
*                                                                               
         CLI   PREVREC,COPY       PREVIOUS REC A COPY ?                         
         BNE   *+8                NO                                            
         BAS   RE,COPYERR         YES/PRINT OUT PREVIOUS COPY AS ERROR          
         MVI   PREVREC,0           CLEAR PREVREC FLAG                           
*                                                                               
         L     R4,=A(COPYTBL)     USE COPYTBL AREA                              
         MVI   BYTE,C'A'          PASS ADD AS STATUS                            
         BAS   RE,DOHISTRY        PUTS UNIT DATA TO TABLE                       
         USING HISTDATD,R4                                                      
                                                                                
         CLI   HIREAS,C'X'        IS IT X REASON CODE ?                         
         BE    GET                YES-SKIP IT                                   
                                                                                
         CLI   NEWUNIT,C'Y'        IS IT NEW UNIT?                              
         BNE   GET                 NO- SO SKIP IT ON ADD                        
         BAS   RE,MVSRTBL         MOVES TABLE TO SRTREC/PUTS TO SORTER          
         B     GET                                                              
*                                                                               
         EJECT                                                                  
                                                                                
                                                                                
               EJECT                                                            
                                                                                
**************************************************                              
* - COMPARE COPY TO CHANGE                                                      
* - USE BYTE AS FLAG                                                            
* - R5 -> NEW DATA                                                              
***************************************************                             
                                                                                
COMPARCT NTR1                                                                   
*                                                                               
         MVI   BYTE,0                                                           
         L     R4,=A(COPYTBL)      R4 -> COPY                                   
         L     R5,=A(CHANGTBL)      R5 -> CHANGE                                
         LA    R5,HISTPRD-HIUNTKEY(R5)   POINT R5 TO START OF DATA              
         USING HISTDATD,R4                                                      
         CLC   HISTPRD,0(R5)                                                    
         BE    *+8                                                              
         MVI   BYTE,C'B'           BYTE = TYPE OF DATA                          
         LA    R5,L'HISTPRD(R5)                                                 
*                                                                               
         CLC   HISTLEN,0(R5)       LENGTH                                       
         BE    *+8                                                              
         MVI   BYTE,C'L'                                                        
         LA    R5,L'HISTLEN(R5)                                                 
*                                                                               
         CLC   HISTCOST,0(R5)      ACTUAL COST                                  
         BE    *+8                                                              
         MVI   BYTE,C'A'                                                        
         LA    R5,L'HISTCOST(R5)                                                
*                                                                               
         CLC   HISTDATE,0(R5)      UNIT DATE                                    
         BE    *+8                                                              
         MVI   BYTE,C'D'                                                        
         LA    R5,L'HISTDATE(R5)                                                
*                                                                               
         CLC   HISTIME,0(R5)       START-END TIME                               
         BE    *+8                                                              
         MVI   BYTE,C'T'                                                        
         LA    R5,L'HISTIME(R5)                                                 
*                                                                               
         OC    HISTPNAM,SPACES                                                  
         OC    0(L'HISTPNAM,R5),SPACES                                          
         CLC   HISTPNAM,0(R5)      PROGRAM NAME                                 
         BE    *+8                                                              
         MVI   BYTE,C'N'                                                        
         LA    R5,L'HISTPNAM(R5)                                                
*                                                                               
         CLC   HISTROT,0(R5)        ROTATION                                    
         BE    *+8                                                              
         MVI   BYTE,C'R'                                                        
         LA    R5,L'HISTROT(R5)                                                 
*                                                                               
         LA    R3,8                MAX MISSED = 8                               
         LR    R1,R5               SAVE START OF HISTMIS                        
CMP30    DS    0H                                                               
         CLC   HISTMIS,0(R5)                                                    
         BNE   CMP35                                                            
         LA    R5,L'HISTMIS(R5)                                                 
         LA    R4,L'HISTMIS(R4)                                                 
         BCT   R3,CMP30                                                         
         MVI   0(R1),0             SET 1ST BYTE OF HISTMIS TO 0                 
         B     CMP38                                                            
CMP35    MVI   BYTE,C'M'                                                        
         MVC   0(32,R1),0(R5)      PUT NEW DATA TO START OF HISTMIS             
CMP36    LA    R5,L'HISTMIS(R5)    AND BUMP R5 TO START OF MKGD AREA            
         BCT   R3,CMP36                                                         
*                                                                               
CMP38    LA    R3,8               MAX MAKEGOOD = 8                              
         L     R4,=A(COPYTBL)     RESET R4-> COPY TABLE ADDRESS                 
         LR    R1,R5               SAVE START OF HISTMKD                        
CMP40    DS    0H                                                               
         CLC   HISTMKD,0(R5)                                                    
         BNE   CMP41                                                            
         LA    R5,L'HISTMKD(R5)                                                 
         LA    R4,L'HISTMKD(R4)                                                 
         BCT   R3,CMP40                                                         
         MVI   0(R1),0             SET 1ST BYTE OF HISTMKD TO 0                 
         B     CMP43                                                            
CMP41    MVI   BYTE,C'G'                                                        
         MVC   0(32,R1),0(R5)      SET TO START OF HISTMKD                      
CMP42    LA    R5,L'HISTMKD(R5)    AND BUMP R5 TO PREEMPT AREA                  
         BCT   R3,CMP42                                                         
                                                                                
*                                                                               
CMP43    L     R4,=A(COPYTBL)     RESET R4 -> COPY TABLE ADDRESS                
         CLC   HISTPRMT,0(R5)      PREEMPT                                      
         BE    *+8                                                              
         MVI   BYTE,C'P'                                                        
         LA    R5,L'HISTPRMT(R5)                                                
*                                                                               
         OC    HISTCOMN,SPACES          COMMENT                                 
         OC    0(L'HISTCOMN,R5),SPACES                                          
         CLC   HISTCOMN,0(R5)                                                   
         BE    *+8                                                              
         MVI   BYTE,C'C'                                                        
                                                                                
*                                                                               
CMP100   B     EXIT                                                             
         EJECT                                                                  
                                                                                
         EJECT                                                                  
*****************************************************                           
* PULLS DATA FROM NETBLOCK AND UNIT RECORD          *                           
* R4 ->  AREA FILLED WITH HISTORY DATA              *                           
*****************************************************                           
                                                                                
DOHISTRY NTR1                                                                   
         LR    R5,R4               NOTE R5 KEEPS POINTER TO START               
*                                  OF TABLE                                     
         LR    RE,R4               CLEAR TABLE                                  
         LA    RF,HISTLENE                                                      
         XCEF                                                                   
                                                                                
         USING HISTDATD,R4                                                      
                                                                                
         MVC   HISTSTAT,BYTE       STATUS PASSED BY CALLER                      
                                                                                
* - CALL NETVALUE TO FILL NETBLOCK                                              
         BAS   RE,FILLDBLK        FILLS NETBLOCK  RETURNS REASON/USER           
                                                                                
         L     R6,NBAIO            IF NEW UNIT/PRINT IT ON ADD                  
         MVI   ELCODE,2                                                         
         MVI   NEWUNIT,0                                                        
         BAS   RE,GETEL                                                         
         BNE   DH5                                                              
         USING NUSDREL,R6                                                       
         TM    NUSDST4,X'20'                                                    
         BNO   *+8                                                              
         MVI   NEWUNIT,C'Y'                                                     
                                                                                
DH5      MVC   HIREAS,WORK+10      RETURNED BY FILLDBLK ABOVENRED               
         MVC   HIUSER,WORK                                                      
                                                                                
* - SAVE HISTORY FIELDS                                                         
                                                                                
         L     R1,NBAIO                                                         
         MVC   HIUNTKEY,0(R1)         SAVE UNIT KEY                             
         MVC   HIRECTM,RECTIME        TIME OF CHANGE                            
         MVC   HIRECDAT,RECDAT        DATE OF CHANGE                            
*                                                                               
         MVC   HISTPRD(2),NBPRD       PRODUCTS (PRD1 AND PRD2)                  
         CLI   NBPRDNO,0              IS IT MULTIPLE PRODUCTS?                  
         BE    *+10                                                             
         MVC   HISTPRD,NBPRDLST       MULTIPLE PRODUCTS                         
*                                                                               
         MVC   HISTLEN,NBLEN          LENGTH                                    
*                                                                               
         MVC   HISTCOST,NBACTUAL      ACTUAL COST                               
*                                                                               
         MVC   HISTDATE,NBACTDAT      DATE                                      
*                                                                               
         MVC   HISTIME,NBTIME         TIME                                      
*                                                                               
         MVC   HISTPNAM,NBPROGNM      PROGRAM NAME                              
*                                                                               
         MVC   HISTROT,NBSDROT        ROTATION                                  
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'04'           COMMENTS ?                                
         BAS   RE,GETEL                                                         
         BNE   DH7                                                              
         ZIC   R1,1(R6)                                                         
         S     R1,=F'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HISTCOMN(0),4(R6)         YES                                    
                                                                                
* - MISSED                                                                      
DH7      LA    R3,8                    SAVE MAX OF 8                            
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'06'            MISSED DETAILS                           
         BAS   RE,GETEL                                                         
         BNE   DH10                                                             
DH8      MVC   HISTMIS,0(R6)           MISSED DATA                              
         LA    R4,L'HISTMIS(R4)    BUMP TO NEXT MISSED AREA                     
         BAS   RE,NEXTEL           BUMP TO NEXT MISSED ELEM                     
         BNE   DH10                                                             
         BCT   R3,DH8                                                           
                                                                                
* - MAKE-GOOD                                                                   
DH10     LR    R4,R5               RESET R4 POINTER                             
         LA    R3,8                SAVE MAX OF 8                                
         L     R6,NBAIO            RESET R6 TO START OF UNIT RECORD             
         MVI   ELCODE,X'07'        MAKE GOOD DETAILS                            
         BAS   RE,GETEL                                                         
         BNE   DH20                                                             
DH12     MVC   HISTMKD,0(R6)        MAKE GOOD DATA                              
         LA    R4,L'HISTMKD(R4)     BUMP TO NEXT MKGD AREA                      
         BAS   RE,NEXTEL            BUMP TO NEXT MKGD ELEM                      
         BNE   DH20                                                             
         BCT   R3,DH12                                                          
*                                                                               
DH20     LR    R4,R5               RESET POINTER TO START OF TABLE              
         TM    NBUNITST,X'40'      PREEMPT ?                                    
         BNO   DH25                                                             
         MVI   HISTPRMT,C'Y'                                                    
                                                                                
DH25     L     R6,NBAIO                                                         
         MVI   ELCODE,9                                                         
         BAS   RE,GETEL                                                         
         BNE   DH27                                                             
         USING NUAUDEL,R6                                                       
         MVC   HISTGRP,NUAUDGRP                                                 
                                                                                
DH27     DS    0H                                                               
         MVC   HISTPKG,NBPACK                                                   
                                                                                
*                                                                               
DHX      XIT1                                                                   
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
* FILLS NETBLOCK AND RETURNS USER CODE IN WORK                                  
FILLDBLK NTR1                                                                   
         MVC   NBAIO,AUNITREC       SET NBAIO TO START OF UNIT REC              
         MVI   NBESTOPT,0           NO ESTIMATED DEMOS                          
         MVI   NBACTOPT,0           NO ACTUAL DEMOS                             
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*  - GET REASON/USER CODE                                                       
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE 99 ELEMENT                         
         USING NUACTD,R6                                                        
         MVC   WORK+10(4),NUACTRSN    REASON CODE -> WORK+10(4)                 
                                                                                
         MVC   WORK(2),NUACTAGD    AGENCY                                       
         MVC   WORK+2(2),NUACTAID  CREATION PERSONAL ID                         
         BAS   RE,OWHO             RETURNS USER IN WORK(8)                      
*                                                                               
FILLX    B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
                                                                                
         EJECT                                                                  
MYHIGH   NTR1                                                                   
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         B     DIRALL                                                           
                                                                                
MYSEQ    NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
                                                                                
MYDWRT   NTR1                                                                   
         MVC   COMMAND,=CL8'DMWRT'                                              
         B     DIRALL                                                           
                                                                                
DIRALL   DS    0H                                                               
         CLI   FILE,0              WAS REP DIR OVERRIDEN ?                      
         BE    *+6                 NO                                           
         DC    H'0'                SHOULD NOT BE HERE                           
         MVC   FILE,=CL8'UNTDIR'   DEFAULT                                      
         ZIC   R4,UPDTBIT          SET OPTIONAL BIT                             
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,KEY,KEY,0                       
         MVI   FILE,0              CLEAR FILE FOR REP DEFAULT                   
         B     DDRECX                                                           
*                                                                               
MYGET    NTR1                                                                   
         LA    RF,=C'GETREC'                                                    
         ZIC   R4,UPDTBIT          SET OPTIONAL READ FOR UPDATE                 
         B     DDREC5                                                           
MYADD    NTR1                                                                   
         LA    RF,=C'ADDREC'                                                    
         B     DDREC5                                                           
MYPUT    NTR1                                                                   
         LA    RF,=C'PUTREC'                                                    
         B     DDREC5                                                           
*                                                                               
DDREC5   ST    RF,DMCB                                                          
         CLI   FILE,0              OVERIDE DEFAULT UNT FILE?                    
         BNE   DDREC7                                                           
         MVC   FILE,=CL8'UNTFILE'  NO                                           
         L     R3,=A(MYIO2)        HIST RECS READ INTO MYIO2                    
         LA    R2,KEY+21                                                        
         B     DDREC10                                                          
                                                                                
DDREC7   MVC   FILE,=CL8'SPTFILE'                                               
         LA    R2,KEY+14                                                        
         L     R3,=A(MYIO2)         SPOT RECS READ INTO MYIO2                   
         B     DDREC10                                                          
                                                                                
DDREC10  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,,FILE,(R2),(R3),MYDMWORK,0                          
         MVI   UPDTBIT,0           DEFAULT NOT READ FOR UPDATE                  
         MVI   FILE,0              DEFAULT UNT FILE                             
*                                                                               
DDRECX   CLI   8(R1),0             SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
         DS    0F                                                               
FILE     DS    CL8                                                              
MYDMWORK DS    CL96                                                             
UPDTBIT  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
* - PRINTD ONLY HAS ONE PRINT LINE/USE MY OWN                                   
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
                                                                                
* - GET NAME TO MATCH PERSONAL ID                                               
* - EXPECTS - WORK(2) HAS AGENCY  WORK+2(2) HAS PERSONAL ID                     
* - RETURNS - NAME(8) IN WORK                                                   
OWHO     NTR1                                                                   
*                                                                               
         LA    R3,WHOTBL           IS NAME IN TABLE ?                           
         CLI   0(R3),0             FIRST TIME?                                  
         BE    OWHO05              YES                                          
                                                                                
OWHO00   CLC   WORK(4),0(R3)                                                    
         BNE   OWHO01                                                           
         MVC   WORK(8),4(R3)       YES                                          
         B     OWHOX                                                            
                                                                                
OWHO01   LA    R3,12(R3)           NO - BUMP TO NEXT ENTRY                      
         CLI   0(R3),0             ROOM IN TABLE                                
         BE    OWHO05              YES - ADD NEW ID/NAME HERE                   
         CLI   0(R3),X'FF'         NO MORE ROOM IN TABLE                        
         BNE   OWHO00                                                           
                                                                                
* - TABLE IS FULL - MOVE TBL DOWN AND DELETE LAST ENTRY                         
         LA    RF,WHOTBL               POINT RF TO LAST ENTRY                   
         LA    RF,(L'WHOTBL-12)(RF)                                             
         LA    R1,(L'WHOTBL-12)        R1 = LENGTH OF WHOTBL-12                 
         LA    RE,WHOTBL                                                        
         LA    RE,(L'WHOTBL-24)(RE)    POINT RE TO PENULTIMATE ENTRY            
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R3,WHOTBL            POINT R3 TO TBL START FOR NEW ENTRY         
*                                                                               
OWHO05   MVC   MYKEY,KEY           SAVE KEY                                     
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CT0REC,R6                                                        
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,WORK                                                     
         MVC   CT0KNUM,WORK+2                                                   
         L     R6,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                      
         CLC   CT0KEY,KEY                                                       
         BNE   OWHO30                                                           
*                                                                               
         LA    RE,28(R6)                                                        
         SR    R0,R0                                                            
OWHO10   CLI   0(RE),0                                                          
         BE    OWHO30                                                           
         CLI   0(RE),X'C3'                                                      
         BE    *+14                                                             
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     OWHO10                                                           
         MVC   0(4,R3),WORK      ADD TO TABLE AGY/ID                            
         MVC   4(8,R3),2(RE)                  NAME                              
         MVC   WORK(8),2(RE)       PASS BACK TO CALLER                          
         B     OWHO40                                                           
                                                                                
OWHO30   MVC   WORK(8),=C'????????'                                             
                                                                                
OWHO40   DS    0H                                                               
         MVC   KEY,MYKEY           RESET KEY                                    
OWHOX    B     EXIT                                                             
         EJECT                                                                  
**********************************************                                  
*                                                                               
*  WRITE REPORT                                                                 
*                                                                               
*                                                                               
EXRPT    NTR1                                                                   
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
*                                                                               
EXP30    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    EXIT                                                             
         MVC   SRTTBL(200),0(R3)  MOVE SORTED REC TO SORTBL                     
         B     EXP30B                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'SRT',SRTTBL,C'DUMP',100,=C'1D'                
EXP30B   CLC   SVSRTCLT(11),SRTKCLT    NEW CLIENT                               
         BE    *+8                                                              
         BAS   RE,NEWCLT                                                        
                                                                                
         LA    R2,XP                  R2 -> PRINT LINE                          
         USING PLINED,R2                                                        
                                                                                
                                                                                
* - ACTIVITY DATE                                                               
         GOTO1 DATCON,DMCB,(3,SRTKCHD),(X'20',WORK)                             
         MVC   PLACTDAT(2),WORK+2                     MM/DD/YY                  
         MVI   PLACTDAT+2,C'/'                                                  
         MVC   PLACTDAT+3(2),WORK+4                                             
         MVI   PLACTDAT+5,C'/'                                                  
         MVC   PLACTDAT+6(2),WORK                                               
                                                                                
* - STATUS                                                                      
         MVC   PLSTATUS(3),=C'ADD'                                              
         CLI   SRTYPE,C'A'                                                      
         BE    *+10                                                             
         MVC   PLSTATUS,=C'DELETE'                                              
                                                                                
* - UNIT DATE                                                                   
         CLI   SRUNTDAT,0                                                       
         BE    WRTPRG                                                           
         GOTO1 DATCON,DMCB,(2,SRUNTDAT),(4,PLUNTDAT)                            
         MVI   PLUNTDAT+5,C'-'                                                  
         EDIT  (B1,SRTKSUB),(3,PLUNTDAT+6),ALIGN=LEFT                           
                                                                                
* - PROGRAM NAME                                                                
WRTPRG   DS    0H TESTINNNNNNNNNNNNNNNNN                                        
         MVC   PLPROG(6),SRTKPRG     GIVE PROGCODE WHEN TESTING                 
         EDIT  (B1,SRTKEST),(3,PLPROG+7)                                        
         EDIT  (B1,SRTPKG),(3,PLPROG+11)                                        
*****    MVC   PLPROG,SRPRGNM                                                   
                                                                                
* - DAY                                                                         
         CLI   SRROT,0                                                          
         BE    WRTTIME                                                          
         GOTO1 UNDAY,DMCB,SRROT,PLDAY                                           
                                                                                
* - TIME                                                                        
WRTTIME  OC    SRTIM,SRTIM                                                      
         BZ    WRTLEN                                                           
         GOTO1 UNTIME,DMCB,SRTIM,PLTIME                                         
                                                                                
* - LENGTH                                                                      
WRTLEN   EDIT  (B1,SRLEN),(3,PLLEN)                                             
                                                                                
* - ACTUAL COST                                                                 
         OC    SRCOST,SRCOST                                                    
         BZ    WRTPROD                                                          
         ICM   R1,15,SRCOST        ACTUAL COST / DROP PENNIES                   
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(7,PLCOST)                                                  
                                                                                
* - BRAND                                                                       
WRTPROD  CLI   SRPROD,0                                                         
         BE    WRTRSN                                                           
         BAS   RE,GETPRD                                                        
         MVC   PLBRAND,WORK                                                     
                                                                                
                                                                                
* - REASON CODE                                                                 
WRTRSN   MVC   PLREASON,SRREASON                                                
                                                                                
* - TRAIL (MAKE GOOD/MISSED)                                                    
         LA    R3,SRMKGMSD                                                      
         USING NUMGEL,R3                                                        
         CLI   0(R3),0            IF MKGD/MISSED                                
         BNE   TRAIL10            PRINT THEM                                    
         CLI   SRPRMT,C'Y'        ELSE IF PREEMPT                               
         BNE   WRTCOM                                                           
         MVC   PLTRAIL(8),=C'PRE-EMPT'    SHOW AS PREEMPT                       
         MVC   PLSTATUS,=C'DELETE'        AND MAKE IT A DELETE                  
         B     WRTCOM                                                           
TRAIL10  MVC   PLTRAIL(7),=C'MKGD BY'                                           
         CLI   0(R3),X'07'                                                      
         BE    *+10                                                             
         MVC   PLTRAIL+5(3),=C'FOR'                                             
         MVC   PLTRAIL+9(16),NUMGPNM                                            
         GOTO1 DATCON,DMCB,(2,NUMGDATE),(7,PLTRAIL+25)       MMMDD              
         MVI   PLTRAIL+31,C'-'                                                  
         EDIT  (B1,NUMGSUB),(2,PLTRAIL+32),ALIGN=LEFT                           
                                                                                
* - COMMENTS                                                                    
WRTCOM   DS    0H                                                               
         MVC   PLCOMM,SRCOMN                                                    
                                                                                
* - BUYER                                                                       
         MVC   PLBUYER,SRBUYER                                                  
                                                                                
                                                                                
* - PRINT                                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   SRTYPE,C'D'         IS IT A  DELETE?                             
         BE    WRTNXT                                                           
         GOTO1 SPOOL,DMCB,(R8)     GIVE A BLANK LINE                            
                                                                                
* - GET NEXT SORT REC                                                           
WRTNXT   B     EXP30                                                            
                                                                                
                                                                                
                                                                                
                                                                                
EXPX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
********************************************                                    
* PRINT DISTRIBUTION HEADER PAGE AT NEWCLIENT                                   
NEWCLT   NTR1                                                                   
                                                                                
* -      GET DISTRIBUTION INFO                                                  
                                                                                
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         CLC   SVSRTCLT,SRTKCLT    HAS CLIENT CHANGED ?                         
         BE    DST70               NO - JUST PAGEBREAK                          
         CLI   SVSRTCLT,0                                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       YES GET DISTRIBUTION LIST                    
         MVI   RCSUBPRG,2                                                       
         MVC   MYKEY,KEY              SAVE KEY                                  
                                                                                
         BAS   RE,GETCLT       PUTS PRINTABLE CLIENT CODE INTO SAVECLT          
                                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D69'       DISTRIBUTION REC                           
         MVC   KEY+2(3),SRTKCLT      AGY/MED/CLI                                
         MVC   KEY+5(4),DISTCOD    DISTRIBUTION CODE                            
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(5),KEYSAVE                                                   
         BNE   DST30                                                            
         CLI   DISTCOD,0                                                        
         BE    *+14                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DST30                                                            
*                                                                               
         L     R3,=A(MYIO2)       READ RECS INTO MYIO2                          
         USING DSTRECD,R3                                                       
         LA    R4,MYDMWORK                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R3),(R4),0           
         LA    R3,24(R3)                                                        
                                                                                
* - LOAD DISTRIBUTION LIST TO MYDIST                                            
         LA    R2,3                ONLY 3 LEVELS OF DISTRIBUTION                
DST05    CLI   0(R3),5                                                          
         BE    DST07                                                            
DST06    ZIC   R1,1(R3)                                                         
         LTR   R1,R1                                                            
         BZ    DST08                                                            
         AR    R3,R1                                                            
         BCT   R2,DST05                                                         
         B     DST08                                                            
DST07    DS    0H                                                               
         L     R1,=A(MYDIST)                                                    
DST07B   CLI   0(R1),0                                                          
         BE    DST07C                                                           
         LA    R1,200(R1)                                                       
         B     DST07B                                                           
DST07C   MVC   0(200,R1),3(R3)                                                  
         B     DST06                                                            
                                                                                
* PRINT DISTRIBUTION NAMES IN MYDIST                                            
DST08    DS    0H                                                               
         L     R3,=A(MYDIST)                                                    
         MVC   XHEAD6+10(17),=C'DISTRIBUTION LIST'                              
         MVC   XHEAD7+10(17),=C'-----------------'                              
         MVC   XP+10(20),0(R3)     HEADLINES                                    
         LA    R3,200(R3)                                                       
         MVC   XP+61(20),0(R3)                                                  
         LA    R3,200(R3)                                                       
         MVC   XP+112(20),0(R3)                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         LA    R2,12               MAX NAMES                                    
         L     R3,=A(MYDIST)       MYDIST                                       
         LA    R3,20(R3)           R3->DIST NAMES                               
         LR    R4,R3               SAVE START OF NAMES IN R4                    
DST10    XC    XP,XP                                                            
         MVC   XP+10(15),0(R3)                                                  
         LA    R3,200(R3)                                                       
         MVC   XP+61(15),0(R3)                                                  
         LA    R3,200(R3)                                                       
         MVC   XP+112(15),0(R3)                                                 
         OC    XP,XP               IF P LINE EMPTY                              
         BZ    DST30               NO MORE TO PRINT                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,15(R4)          BUMP R4 TO NEXT NAME                          
         LR    R3,R4              SET R3 TO POINT TO NEW LIST OF NAMES          
         BCT   R2,DST10                                                         
                                                                                
DST30    DS    0H                                                               
         MVI   SPACING,5                                                        
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
         MVI   SPACING,1                                                        
                                                                                
         MVC   XP+10(13),=C'REASON CODES:'                                      
         LA    R3,XP+24                                                         
         L     R2,=A(MYIO3)        PRINT REASON CODES                           
*                                  3 CODES/TEXT PER PRINT LINE                  
DST35    OC    0(6,R2),0(R2)                                                    
         BZ    DST50                                                            
         MVC   0(6,R3),0(R2)       CODE                                         
         MVI   6(R3),C'='                                                       
         MVC   8(40,R3),6(R2)      TEXT                                         
         LA    R3,51(R3)                                                        
         LA    R2,46(R2)                                                        
*                                                                               
         OC    0(6,R2),0(R2)                                                    
         BZ    DST50                                                            
         MVC   0(6,R3),0(R2)       CODE                                         
         MVI   6(R3),C'='                                                       
         MVC   8(40,R3),6(R2)      TEXT                                         
         LA    R3,51(R3)                                                        
         LA    R2,46(R2)                                                        
*                                                                               
         OC    0(6,R2),0(R2)                                                    
         BZ    DST50                                                            
         MVC   0(6,R3),0(R2)       CODE                                         
         MVI   6(R3),C'='                                                       
         MVC   8(40,R3),6(R2)      TEXT                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,XP+24             RESET P LINE                                
         LA    R2,46(R2)           BUMP REASON CODES                            
         B     DST35                                                            
*                                                                               
DST50    GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVC   KEY,MYKEY           RESTORE SEQUENTIAL READ                      
         BAS   RE,MYHIGH                                                        
DST70    MVC   SVSRTCLT,SRTKCLT    SAVE NEW CLIENT                              
         MVC   SVSRTGRP,SRTKGRP                                                 
         MVC   SVSRTNET,SRTKNET                                                 
         MVI   RCSUBPRG,1          SET USUAL HEAD PRINTING                      
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*******************************************************************             
* - GET CLIENT RECORD FOR PRINTABLE CLIENT CODE                                 
*                                                                               
GETCLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SRTKCLT                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MYDMWORK                                                      
         L     R4,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R4),(R3),0           
         USING CLTHDR,R4                                                        
         GOTO1 CLUNPK,DMCB,(CPROF+6,SRTKCLT+1),SAVECLT                          
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4048,                                             X        
               MACRF=GM,                                               X        
               EODAD=EXIT                                                       
               SPACE                                                            
         SPACE                                                                  
UTL      DC    F'0',X'00'                                                       
SSB      DC    F'2'                                                             
                                                                                
* - COPY WITH NO CHANGE                                                         
COPYERR  NTR1                                                                   
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         MVC   XP(16),=C'COPY - NO CHANGE'                                      
         GOTO1 HEXOUT,DMCB,PREVIOUS,XP+20,44                                    
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
* - CHANGE WITH NO PREVIOUS COPY                                                
CHANGERR NTR1                                                                   
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         MVC   XP(16),=C'CHANGE - NO COPY'                                      
         GOTO1 HEXOUT,DMCB,RECFILTY,XP+20,100                                   
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS SPROG 1,2                                                              
         WSPEC H1,1,C'CLIENT NAME'                                              
         WSPEC H2,1,C'AUDIT GROUP'                                              
         WSPEC H3,1,C'NETWORK'                                                  
         WSPEC H4,1,C'CHANGE DATE'                                              
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,150,PAGE                                                      
         WSPEC H1,80,C'NOTICE OF CHANGE'                                        
         DC    X'00'                                                            
                                                                                
HDRTN    NTR1                                                                   
         L     R5,ADRWIDE                                                       
         USING WIDED,R5                                                         
         MVC   XHEAD1+12(3),SAVECLT                                             
         MVC   XHEAD2+12(4),SRTKGRP                                             
         MVC   XHEAD3+12(4),SRTKNET                                             
         MVC   XHEAD4+12(8),CHANGDAT                                            
         CLI   RCSUBPRG,2                                                       
         BE    HDRTNX                                                           
         LA    R1,XHEAD5                                                        
         USING PLINED,R1                                                        
         MVC   PLACTDAT,=C'ACTIVITY'                                            
         MVC   PLACTDAT+200(4),=C'DATE'                                         
         MVC   PLSTATUS,=C'STATUS'                                              
         MVC   PLUNTDAT+2(4),=C'UNIT'                                           
         MVC   PLUNTDAT+200(4),=C'DATE'                                         
         MVC   PLPROG+3(7),=C'PROGRAM'                                          
         MVC   PLDAY(3),=C'DAY'                                                 
         MVC   PLTIME+3(4),=C'TIME'                                             
         MVC   PLLEN(3),=C'LEN'                                                 
         MVC   PLCOST+1(5),=C'$COST'                                            
         MVC   PLBRAND(5),=C'BRAND'                                             
         MVC   PLREASON+1(3),=C'RSN'                                            
         MVC   PLTRAIL+1(5),=C'TRAIL'                                           
         MVC   PLCOMM+1(8),=C'COMMENTS'                                         
         MVC   PLBUYER(5),=C'BUYER'                                             
HDRTNX   B     EXIT                                                             
         DROP  R1,R5                                                            
                                                                                
         EJECT                                                                  
                                                                                
INIT     NTR1                                                                   
* - RUN DATE                                                                    
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYC)     COMPRESSED                      
*                                                                               
*                                                                               
****     GOTO1 DATCON,DMCB,(2,TODAYC),(3,TODAYB)     BINARY                     
         GOTO1 DATCON,DMCB,(2,TODAYC),(X'20',WORK)   YYMMDD PRINTABLE           
                                                                                
         MVC   CHANGDAT(2),WORK+2                                               
         MVI   CHANGDAT+2,C'/'                                                  
         MVC   CHANGDAT+3(2),WORK+4                                             
         MVI   CHANGDAT+5,C'/'                                                  
         MVC   CHANGDAT+6(2),WORK                                               
                                                                                
         MVI   WHOTBLX,X'FF'                                                    
                                                                                
         OPEN  (RECVIN,(INPUT))                                                 
                                                                                
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD      INITIALIZE SORTER              
         B     INIT10                                                           
                                                                                
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
*                                                                               
                                                                                
INIT10   DS    0H                                                               
         BAS   RE,RCODES           SAVE REASON CODES IN MYIO3                   
                                                                                
*                                                                               
INITX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*******************************************************                         
*                                                                               
* READ AND STORE REASON CODES                                                   
*                                                                               
RCODES   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D77'       REASON CODES                               
         MVC   KEY+2(2),NBSELAGY                                                
         MVI   KEY+4,C'N'                                                       
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(5),KEYSAVE                                                   
         BNE   RSN30                                                            
*                                                                               
         LA    R6,100            MAX NUMBER OF CODES                            
         L     R2,=A(MYIO3)      SAVE REASON CODES IN MYIO3                     
         L     R3,=A(MYIO2)      READ RECS INTO MYIO2                           
         USING RSNRECD,R3                                                       
         LA    R4,MYDMWORK                                                      
RSN10    GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R3),(R4),0           
         MVC   0(6,R2),RSNKCODE                                                 
         MVC   6(40,R2),RSNTEXT                                                 
         LA    R2,46(R2)                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(5),KEYSAVE                                                   
         BNE   RSN30                                                            
         BCT   R6,RSN10                                                         
         DC    H'0'              TABLE BLOWN - TOO MANY REASON CODES            
                                                                                
RSN30    DS    0H                                                               
         B     EXIT                                                             
******************************************************************              
         EJECT                                                                  
***********************************************************                     
* CLIENT REC IN ANETWS1                                                         
* PRODUCT IN SRPROD                                                             
*                                                                               
*  NOTEEEEEEEEE ONLY GETS 1ST PROD IN LIST OF 6 POSSIBLE PRODS                  
******************************************************************              
GETPRD   NTR1                                                                   
         L     R1,ANETWS1        CLIENT REC SITS IN ANETWS1                     
         CLC   SRTAMC,1(R1)       SAME CLIENT ?                                 
         BE    PRD10               YES                                          
         MVC   MYKEY,KEY           NO / SAVE KEY                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SRTAMC     AGY/MED/CLT                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ANETWS1                                                       
         LA    R4,MYDMWORK                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R3),(R4),0           
                                                                                
                                                                                
PRD10    L     R1,ANETWS1                                                       
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         LA    R2,220              MAX NUMBER OF PRODUCTS                       
PRD12    CLC   3(1,R1),SRPROD                                                   
         BE    PRD20                                                            
         LA    R1,4(R1)                                                         
         BCT   R2,PRD12                                                         
         MVC   WORK(3),=C'UNA'                                                  
         B     PRD30                                                            
*                                                                               
PRD20    MVC   WORK(3),0(R1)                                                    
*                                                                               
PRD30    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*                                                                               
         DC    CL8'**COPY**'                                                    
COPYTBL  DS    CL(HISTLENE)        COPY UNIT HIST DATA                          
*                                                                               
         DC    CL8'**CHANG*'                                                    
CHANGTBL DS    CL(HISTLENE)        CHANGED UNIT HIST DATA                       
*                                                                               
*                                                                               
         DC    CL8'**MYIO**'                                                    
MYIO     DS    0D                                                               
         DS    4048C                                                            
MYIOLNE  EQU   *-MYIO                                                           
                                                                                
         DC    CL8'**MYIO2*'                                                    
MYIO2    DS    0D                                                               
         DS    4048C                                                            
MYIO2LNE EQU   *-MYIO2                                                          
                                                                                
         DC    CL8'**MYIO3*'                                                    
MYIO3    DS    0D                                                               
         DS    4048C                                                            
MYIO3LNE EQU   *-MYIO3                                                          
                                                                                
         DC    CL8'**MYDST*'                                                    
MYDIST   DS    CL600               ROOM FOR 3 LEVELS OF DISTRIBUTION            
                                                                                
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
HISTDATD DSECT                     HISTORY DATA DSECT                           
*                                                                               
HISTARTD DS    0C                                                               
HIUNTKEY DS    CL20                KEY OF COPY/CHANGE UNIT                      
HIRECTM  DS    XL4                 RECOVERY HEADER TIME                         
HIRECDAT DS    XL3                 RECOVERY HEADER DATE                         
HIREAS   DS    CL4                 REASON CODE                                  
HIUSER   DS    CL8                 USER CODE                                    
*                                                                               
HISTPRD  DS    CL6                 PRODUCT (MAX OF 6)                           
HISTLEN  DS    CL1                 LENGTH                                       
HISTCOST DS    CL4                 ACTUAL COST                                  
HISTDATE DS    CL2                 UNIT DATE                                    
HISTIME  DS    CL4                 START-END TIME                               
HISTPNAM DS    CL16                PROGRAM NAME                                 
HISTROT  DS    CL1                 ROTATION                                     
*                                                                               
HISTMIS  DS    CL32                MISSED DETAILS                               
         DS    CL(7*32)            LEAVE ROOM FOR 7 MORE MISSED                 
*                                                                               
HISTMKD  DS    CL32                MAKE-GOOD DETAILS                            
         DS    CL(7*32)            LEAVE ROOM FOR 7 MORE MAKE-GOOD              
*                                  0-5    PROGRAM CODE                          
*                                  6-21   PROGRAM NAME                          
*                                  22-23  DATE                                  
*                                  24     SUB-LINE NUMBER                       
*                                                                               
HISTPRMT DS    CL1                 PREEMPT                                      
*                                                                               
HISTCOMN DS    CL60                COMMENT                                      
HISTSTAT DS    CL1                 A=ADD,D=DELETE (TO PASS TO PRINT)            
HISTGRP  DS    CL4                 AUDIT GROUP                                  
HISTPKG  DS    CL1                 PACKAGE                                      
HISTLENE EQU   *-HISTARTD                                                       
         EJECT                                                                  
*                                                                               
                                                                                
MYWORKD  DSECT                                                                  
AMSAVE   DS    CL1    *** FROM EDIT MODULE - DO NOT MOVE                        
ONECLT   DS    CL2    ***                                                       
DISTCOD  DS    CL4    ***          DISTRIBUTION CODE                            
*                                                                               
AUNITREC DS    A                   UNIT RECORD POINTER                          
ADRWIDE  DS    A                                                                
*                                                                               
PREVREC  DS    CL1                                                              
SAVECLT  DS    CL3                                                              
SAVCLT2  DS    CL2                                                              
SAVEPROD DS    CL7                                                              
SAVEEST  DS    CL1                                                              
SAVENET  DS    CL4                                                              
NEWUNIT  DS    CL1                                                              
USERNUM  DS    CL2                                                              
PWORK    DS    PL16                                                             
PREVIOUS DS    CL44                                                             
RECFILTY DS    CL100               RECOVERY HEADER+PART OF RECORD               
RECDAT   DS    CL3                 RECOVERY DATE                                
RECTIME  DS    CL4                 RECOVERY TIME                                
TIMESV   DS    CL4                                                              
TIMSUB   DS    CL1                                                              
                                                                                
*                                                                               
*                                                                               
TODAYB   DS    CL3                 BINARY                                       
TODAYC   DS    CL2                 COMPRESSED                                   
PREVFLG  DS    CL1                                                              
SKIPCHNG DS    CL1                                                              
SVSRTCLT DS    CL3                                                              
SVSRTGRP DS    CL4                                                              
SVSRTNET DS    CL4                                                              
CHANGDAT DS    CL8                                                              
MYKEY    DS    CL40                                                             
                                                                                
*                                                                               
WHOTBL   DS    CL(12*50)           ROOM FOR 50 ID(2)/AGY(2)/NAME(8)             
WHOTBLX  DS    CL1                 END OF TABLE = X'FF'                         
*                                                                               
*********************************************************                       
*                                                                               
SRTTBL   DS    0CL200    *** NOTE HARD CODED                                    
SRTKCLT  DS    CL3               * AGY/MED/CLT      HIST KEY                    
SRTKGRP  DS    CL4               * AUDIT GROUP                                  
SRTKNET  DS    CL4               * NETWORK          HIST KEY                    
SRTKCHD  DS    CL3               * CHANGEDATE                                   
SRTKTIM  DS    CL4               * CHANGETIME                                   
SRTKTSUB DS    CL1                 TIME SUB LINE                                
* SORT KEY = 18                                                                 
SRTKPRG  DS    CL6               * PROGRAM          HIST KEY                    
SRTKEST  DS    CL1               * ESTIMATE         HIST KEY                    
SRTKDAT  DS    CL2               * AIR DATE         HIST KEY                    
SRTKSUB  DS    CL1               * SUB LINE         HIST KEY                    
         DS    CL1               * SPARE                                        
* SORT KEY = 30                                                                 
*                                                                               
SRDATA   DS    0CL2                DATA FIELDS START HERE                       
SRUNTDAT DS    CL2                 DATE                                         
SRPRGNM  DS    CL16                PROGRAM NAME                                 
SRROT    DS    CL1                 ROTATION                                     
SRTIM    DS    CL4                 TIME                                         
SRLEN    DS    CL1                 LENGTH                                       
SRCOST   DS    CL4                 ACTUAL                                       
SRPROD   DS    CL6                 PRODUCT                                      
SRREASON DS    CL4                 REASON                                       
SRCOMN   DS    CL60                COMMENT                                      
SRBUYER  DS    CL8                 USER CODE                                    
SRMKGMSD DS    CL35                MAKEGOOD/MISSED                              
SRPRMT   DS    CL1                 PREEMPT C'Y'                                 
SRTAMC   DS    CL3                 AGY/MEDIA/CLI                                
SRTPKG   DS    CL1                 PACKAGE                                      
         DS    CL23                SPARE                                        
SRTYPE   DS    CL1                 A=ADD/D=DELETE                               
SRTBLEN  EQU   *-SRTTBL            TOTAL SORT REC LENGTH                        
SRTDLEN  EQU   *-SRDATA            DATA LENGTH                                  
*                                                                               
*                                                                               
*                                                                               
         PRINT ON                                                               
MYWRKDLE EQU   *-MYWORKD                                                        
*                                                                               
                                                                                
*                                                                               
PLINED   DSECT                                                                  
PLACTDAT DS    CL8                                                              
         DS    CL2                                                              
PLSTATUS DS    CL6                                                              
         DS    CL2                                                              
PLUNTDAT DS    CL8                                                              
         DS    CL2                                                              
PLPROG   DS    CL16                                                             
         DS    CL2                                                              
PLDAY    DS    CL3                                                              
         DS    CL2                                                              
PLTIME   DS    CL10                                                             
         DS    CL2                                                              
PLLEN    DS    CL3                                                              
         DS    CL2                                                              
PLCOST   DS    CL7                                                              
         DS    CL2                                                              
PLBRAND  DS    CL4                                                              
         DS    CL2                                                              
PLREASON DS    CL4                                                              
         DS    CL2                                                              
PLTRAIL  DS    CL33                                                             
         DS    CL2                                                              
PLCOMM   DS    CL30                                                             
         DS    CL1                                                              
PLBUYER  DS    CL8                                                              
PLLENE   EQU   *-PLINED                                                         
*                                                                               
RECD     DSECT                                                                  
RECLN    DS    XL2                                                              
         DS    CL2                                                              
REC      DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
       ++INCLUDE NEGENHIST                                                      
       ++INCLUDE SPGENDIST                                                      
       ++INCLUDE SPGENREAS                                                      
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICFD                                                       
*                                                                               
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI89T  05/01/02'                                      
         END                                                                    
