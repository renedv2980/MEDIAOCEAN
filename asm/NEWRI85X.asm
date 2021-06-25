*          DATA SET NEWRI85X   AT LEVEL 034 AS OF 05/01/02                      
*          DATA SET NEWRI85    AT LEVEL 026 AS OF 06/15/01                      
*PHASE T32085C,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*                                                                               
         TITLE 'T32085 - NETWORK UNIT HISTORY UPDATE'                           
************************************************************                    
*                                                                               
* THIS REPORT READS RECOVERY FILE AND UPDATES NETWORK UNIT                      
* HISTORY RECORDS                                                               
*                                                                               
*************************************************************                   
         SPACE 2                                                                
T32085   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T32085*,RA,R8                                                 
         USING T32085,RB,RA,R8                                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
                                                                                
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
                                                                                
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
*                                                                               
         L     R1,ANETWS1        ANETWS1 AND 2  FOR CLIENT RECORD               
         ST    R1,NBACLI                                                        
*                                                                               
         L     R7,ANETWS3                                                       
         USING MYWORKD,R7                                                       
                                                                                
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    RE,HEDSPECS                                                      
         ST    RE,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
                                                                                
         DROP  R5                                                               
*                                                                               
         L     R5,=A(MYIO)                                                      
         USING RECD,R5                                                          
         LA    RE,28(R5)           POINT PAST RECV HEADER                       
         ST    RE,AUNITREC         UNIT RECORD POINTER                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    MAINLINE                                                         
EXIT     XIT1                                                                   
                                                                                
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
*                                                                               
MAINLINE DS    0H                                                               
         SPACE                                                                  
         BAS   RE,INIT         INITIALIZE                                       
         BAS   RE,NET1         READ RECOVERY                                    
         GOTO1 =V(PRNTBL),DMCB,=C'COPG',COPYGRS,C'DUMP',8,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'CHGG',CHGGRS,C'DUMP',8,=C'1D'                 
         GOTO1 =V(PRNTBL),DMCB,=C'TOTPUT$',TOTPUT$,C'DUMP',8,=C'1D'             
         GOTO1 =V(PRNTBL),DMCB,=C'ELMNUM',ELMNUM,C'DUMP',4,=C'1D'               
         GOTO1 =V(PRNTBL),DMCB,=C'PUTNUM',PUTNUM,C'DUMP',4,=C'1D'               
         GOTO1 =V(PRNTBL),DMCB,=C'UNITS',UNITS,C'DUMP',4,=C'1D'                 
****     EDIT  (P8,COPYGRS),(10,MYP),2                                          
****     EDIT  (P8,CHGGRS),(10,MYP+15),2                                        
****     BAS   RE,PRINTIT                                                       
         CLOSE (RECVIN)        CLOSE RECOVERY                                   
         FREEPOOL RECVIN                                                        
*                                                                               
         B     EXIT                                                             
*                                                                               
COPY     EQU   1                                                                
CHANGE   EQU   2                                                                
ADDS     EQU   3                                                                
*                                                                               
         EJECT                                                                  
                                                                                
INIT     NTR1                                                                   
                                                                                
                                                                                
         ZAP   COPYGRS,=P'0'                                                    
         ZAP   CHGGRS,=P'0'                                                     
         ZAP   TOTPUT$,=P'0'                                                    
         MVI   MYSPACES,X'40'                                                   
         MVC   MYSPACES+1(L'MYSPACES-1),MYSPACES                                
         MVC   MYP,MYSPACES                                                     
         LA    RE,WHOTBL                                                        
         LA    RF,L'WHOTBL                                                      
         XCEF                                                                   
         MVI   WHOTBLX,X'FF'                                                    
         OPEN  (RECVIN,(INPUT))                                                 
INITX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
********************************************************                        
* GETS RECORDS FROM THE RECOVERY FILE                                           
* IF SIGNIFICANT CHANGE TO REC UPDATE HISTORY RECORD                            
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
         PRINT GEN                                                              
         GET   (1),(0)                                                          
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   RFILTY,X'2A'       TEST UNTFILE                                  
         BNE   GET                                                              
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
                                                                                
* SET ZERO ELEMENT CODE AT E-O-R                                                
         LH    R1,REC-4                                                         
         LA    R1,REC-4(R1)                                                     
         XC    0(2,R1),0(R1)                                                    
                                                                                
* - TEST IF AGENCY                                                              
         CLC   NUKAM,AMSAVE        IS IT REQUESTED AGENCY ?                     
         BNE   GET                                                              
* - TEST IF CLIENT                                                              
         CLI   ONECLT,0            IS IT REQUESTED CLIENT ?                     
         BE    NET13                                                            
         CLC   NUKCLT,ONECLT                                                    
         BNE   GET                                                              
         B     NET13                                                            
* - PATCH EST FILTERS                                                           
         CLC   NUKEST,FILTEST                                                   
         BNE   GET                                                              
         B     NET13                                                            
FILTEST  DS    CL1                                                              
         EJECT                                                                  
                                                                                
NET13    L     R6,AUNITREC                                                      
         USING NURECD,R6                                                        
***      MVC   RECDAT,RDATE        SAVE RECOVERY DATE                           
***      MVC   RECTIME,RTIME       SAVE RECOVERY TIME                           
***      MVC   RECFILTY,RECVHDR                                                 
                                                                                
         CLI   RRECTY,COPY         TEST IF COPY                                 
         BE    NET15                                                            
         CLI   RRECTY,CHANGE       TEST IF CHA                                  
         BE    NET20                                                            
         B     GET                 THAT'S ALL                                   
                                                                                
                                                                                
***********************************************************                     
* - COPY                                                                        
***********************************************************                     
NET15    DS    0H                                                               
                                                                                
         MVI   HISTFLG,C'C'        COPY                                         
         TM    NURSTAT,X'80'       IF IT'S DELETED                              
         BNO   NET17                                                            
         MVI   HISTFLG,C'D'        DELETED                                      
         B     GET                 GET NEXT REC                                 
* PROCESS COPY                                                                  
NET17    L     R4,=A(COPYTBL)                                                   
         BAS   RE,DOHISTRY                                                      
         CLI   PREVFLG,C'B'        DID WE FIND BILLELEMS?                       
         BE    GET                                                              
         MVI   HISTFLG,0           NO/CLEAR HISTFLG                             
         B     GET                    SO WE WON'T PICK UP CHANGE                
                                                                                
*                                                                               
* - CHANGE                                                                      
***********************************************************                     
NET20    DS    0H                                                               
         CLI   HISTFLG,C'D'        WAS 'COPY' DELETED?                          
         BNE   NET20A                                                           
         MVI   HISTFLG,0           YES/CLEAR FLAG                               
         B     GET                     AND SKIP CHANGE FOR NOW                  
*                                                                               
NET20A   CLI   HISTFLG,C'C'        PREVIOUS WAS COPY?                           
         BE    NET20B              YES - CONTINUE NORMAL PROCESSING             
         MVI   HISTFLG,0           NO CLEAR FLAG                                
         B     GET                    AND SKIP FOR NOW                          
                                                                                
NET20B   TM    NURSTAT,X'80'       CHANGE DELETED ?                             
         BNO   NET22               NO                                           
         MVI   HISTFLG,0           YES - CLEAR FLAG                             
         B     GET                       AND SKIP FOR NOW                       
NET22    L     R4,=A(CHANGTBL)                                                  
         MVI   HISTFLG,C'H'        SAY ITS CHANGE                               
         BAS   RE,DOHISTRY         PUT HISTRY DATA TO CHANGTBL                  
         BAS   RE,COMPARCT         COMPARE COPY TO CHANGE                       
         MVI   HISTFLG,0           CLEAR PREVREC FLAG                           
         B     GET                 GET NEXT RECORD                              
                                                                                
               EJECT                                                            
                                                                                
**************************************************                              
* - COMPARE COPY TO CHANGE                                                      
***************************************************                             
                                                                                
COMPARCT NTR1                                                                   
*                                                                               
         MVI   UPDTFLG,0           CLEAR FLAG                                   
         ZAP   PUT$$$,=P'0'        CLEAR $$ AREA                                
         L     R2,=A(COPYTBL)                                                   
         L     R3,=A(CHANGTBL)                                                  
*                                        A PARANOID CHECK                       
         CLC   0(20,R2),0(R3)      UNIT KEYS MUST BE THE SAME?                  
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(20),0(R2)       FOR POSSIBLE READ                            
*                                                                               
         LA    R2,HIST10N-HISTARTD(R2)  POINT TO NUMB OF X'10'                  
         LA    R3,HIST10N-HISTARTD(R3)                                          
         CLC   0(1,R3),0(R2)            COMPARE CHG TO COPY                     
         BNL   COMPX                                                            
                                                                                
         L     R2,=A(COPYTBL)                                                   
         USING HISTARTD,R2                                                      
         GOTO1 =V(PRNTBL),DMCB,=C'KEY1',HIUNTKEY,C'DUMP',20,=C'1D'              
         LA    R3,HIST10S                                                       
         ZIC   R4,HIST10N                                                       
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                  SHOULD HAVE SKIPPED COPY WITH                
*                                  NO BILLS                                     
COMP20   GOTO1 =V(PRNTBL),DMCB,=C'ELEM',0(R3),C'DUMP',32,=C'1D'                 
         L     R1,ELMNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,ELMNUM                                                        
         LA    R3,32(R3)                                                        
         BCT   R4,COMP20                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'$$$$',HISTGRS,C'DUMP',8,=C'1D'                
         AP    COPYGRS,HISTGRS       ADD TO REPORT COPY TOTAL                   
*                                                                               
         L     R2,=A(CHANGTBL)                                                  
         USING HISTARTD,R2                                                      
         GOTO1 =V(PRNTBL),DMCB,=C'KEY2',HIUNTKEY,C'DUMP',20,=C'1D'              
         LA    R3,HIST10S                                                       
         ZIC   R4,HIST10N          WERE THEY DELETED?                           
         LTR   R4,R4                                                            
         BZ    COMP35                                                           
COMP30   GOTO1 =V(PRNTBL),DMCB,=C'ELEM',0(R3),C'DUMP',32,=C'1D'                 
         LA    R3,32(R3)                                                        
         BCT   R4,COMP30                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'$$$$',HISTGRS,C'DUMP',8,=C'1D'                
         AP    CHGGRS,HISTGRS                                                   
*                                                                               
COMP35   GOTO1 MYHIGH              GET MATCHING UNIT ON FILE                    
         CLC   KEY(20),KEYSAVE     SHOULD FIND IT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MYGET                                                         
         L     R1,UNITS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,UNITS                                                         
         L     R6,=A(MYIO2)                                                     
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   COMP40                                                           
*                               REC HAS BILLING                                 
         GOTO1 =V(PRNTBL),DMCB,=C'KEY***',KEY,C'DUMP',20,=C'1D'                 
COMP37   GOTO1 =V(PRNTBL),DMCB,=C'ELM***',0(R6),C'DUMP',30,=C'1D'               
         BAS   RE,NEXTEL                                                        
         BE    COMP37                                                           
         B     COMPX                                                            
*                                                                               
COMP40   DS    0H                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'KEYOK',KEY,C'DUMP',20,=C'1D'                  
*                                                                               
         B     COMPX                                                            
*                              UPDATE UNIT                                      
         L     R2,=A(COPYTBL)                                                   
         USING HISTARTD,R2                                                      
         LA    R3,HIST10S          R3->BILLELEMS                                
         ZIC   R4,HIST10N          R4->FOR BCT LOOP                             
         DROP  R2                                                               
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
COMP60   XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING NUBILEL,R2                                                       
         ZIC   R1,1(R3)                                                         
*                                                                               
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                JUST PARANOID                                
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R3)       MOVE BILL TO ELEM                            
         OI    NUBILST,X'01'       MARK IN CASE OF DISASTER                     
         ICM   R1,15,NUBILGRS      TOTAL $$$ PUT TO UNIT                        
         CVD   R1,DUB                                                           
         AP    PUT$$$,DUB                                                       
         L     R6,=A(MYIO2)                                                     
         GOTO1 =V(PRNTBL),DMCB,=C'BEFR',0(R6),C'DUMP',400,=C'1D'                
         XC    DMCB(16),DMCB                                                    
         GOTO1 HELLO,DMCB,(C'P',=CL8'UNTFILE'),(R6),ELEM,=C'ADD=CODE'           
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,PUTNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,PUTNUM                                                        
         LA    R3,32(R3)                                                        
         BCT   R4,COMP60                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'AFTR',0(R6),C'DUMP',400,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'PUT$',PUT$$$,C'DUMP',8,=C'1D'                 
         AP    TOTPUT$,PUT$$$                                                   
         B     COMPX                                                            
         BAS   RE,MYPUT           WRITE TO FILE                                 
*                                                                               
COMPX    XIT1                                                                   
*                                                                               
         EJECT                                                                  
*****************************************************                           
* CHEKC ON BILLING ELEMENTS                                                     
* R4 -> POINTS TO COPY/CHANGE TABLES                                            
*****************************************************                           
                                                                                
DOHISTRY NTR1                                                                   
         MVI   PREVFLG,0           USE TO INDICATE IF BILL ELEM                 
         LR    RE,R4               CLEAR TABLE                                  
         LA    RF,HISTLENE                                                      
         XCEF                                                                   
         USING HISTDATD,R4                                                      
         ZAP   HISTGRS,=P'0'                                                    
* - CALL NETVALUE TO FILL NETBLOCK                                              
         BAS   RE,FILLDBLK                                                      
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'10'          ANY BILLING ELEMENTS?                      
         BAS   RE,GETEL                                                         
         BE    DH07                                                             
         CLI   HISTFLG,C'H'        NO-ARE WE DEALING WITH CHANGE REC?           
DH05     BNE   DHX                    NO-SO GET OUT                             
*                                     YES-PROCESS EVEN WITH NO ELEMS            
*                                     BECAUSE COPY HAD BILLING                  
*                                     ELSE WE WOULD HAVE SKIPPED THIS           
*                                     CHANGE                                    
         USING NUBILEL,R6                                                       
DH07     MVI   PREVFLG,C'B'        SAY BILLING ELEM FOUND                       
         L     R3,NBAIO                                                         
         MVC   HIUNTKEY,0(R3)      SAVE UNIT KEY                                
         CLI   0(R6),X'10'         AND CHECK TO SEE IF WE HAVE BILLS            
         BNE   DHX                 IN CASES THIS IS A CHANGE WITHOUT            
*                                                                               
         SR    R3,R3                                                            
         LA    R2,HIST10S                                                       
DH10     CLI   0(R2),0             ROOM?                                        
         BE    DH15                                                             
         LA    R2,32(R2)                                                        
         LA    R3,1(R3)                                                         
         CHI   R3,30               30 MAX NUMBER OF BILLING ELEMS               
         BNH   DH10                                                             
         DC    H'0'                INCREASE TABLE FOR X'10' ELEMS               
DH15     ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R6)                                                    
*                                                                               
         ICM   R1,15,NUBILGRS         GET GROSS $                               
         CVD   R1,DUB                                                           
         AP    HISTGRS,DUB                                                      
*                                                                               
         ZIC   R1,HIST10N          ADD TO COUNT OF 10 ELEMENTS                  
         LA    R1,1(R1)                                                         
         STC   R1,HIST10N                                                       
         BAS   RE,NEXTEL                                                        
         B     DH05                                                             
DHX      XIT1                                                                   
                                                                                
         EJECT                                                                  
*                                                                               
FILLDBLK NTR1                                                                   
         MVC   NBAIO,AUNITREC       SET NBAIO TO START OF UNIT REC              
         MVI   NBESTOPT,0           NO ESTIMATED DEMOS                          
         MVI   NBACTOPT,0           NO ACTUAL DEMOS                             
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
FILLX    B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
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
         L     R5,ASPOOLD          RA - DPRINT                                  
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         GOTO1 SPOOL,DMCB,(R5)                                                  
         B     EXIT                                                             
         DROP  R5                                                               
                                                                                
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
         L     R6,=A(MYIO3)                                                     
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
         MVC   MYP(16),=C'COPY - NO CHANGE'                                     
         GOTO1 HEXOUT,DMCB,PREVIOUS,MYP+20,44                                   
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
* - CHANGE WITH NO PREVIOUS COPY                                                
CHANGERR NTR1                                                                   
         MVC   MYP(16),=C'CHANGE - NO COPY'                                     
         GOTO1 HEXOUT,DMCB,RECFILTY,MYP+20,100                                  
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
* - UNIT REC WITH HIST FLAG DELETED - SHOULD NOT BE !!!                         
DELETERR NTR1                                                                   
         MVC   MYP(16),=C'HIST DELETE ERR '                                     
         GOTO1 HEXOUT,DMCB,RECFILTY,MYP+20,100                                  
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS SPROG 1,2                                                              
         SSPEC H1,1,C'CLIENT NAME'                                              
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,120,PAGE                                                      
         DC    X'00'                                                            
                                                                                
HDRTN    NTR1                                                                   
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   H1+12(3),SRTKCLT                                                 
         MVC   H2+12(4),SRTKGRP                                                 
         MVC   H3+12(4),SRTKNET                                                 
         CLI   RCSUBPRG,2                                                       
         BE    HDRTNX                                                           
HDRTNX   B     EXIT                                                             
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
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
HISTDATD DSECT                     HISTORY DATA DSECT                           
*                                                                               
HISTARTD DS    0C                                                               
HIUNTKEY DS    CL20                KEY OF COPY/CHANGE UNIT                      
HIST10S  DS    CL960               30 X 32 - ROOM FOR 20 X'10'                  
HISTGRS  DS    PL8                 TOTAL GROSS DOLLARS                          
HIST10N  DS    CL1                 NUMBER OF X'10' ELEMENTS                     
HISTLENE EQU   *-HISTARTD                                                       
         EJECT                                                                  
*                                                                               
                                                                                
MYWORKD  DSECT                                                                  
AMSAVE   DS    CL1    *** FROM EDIT MODULE - DO NOT MOVE                        
ONECLT   DS    CL2    ***                                                       
*                                                                               
AUNITREC DS    A                   UNIT RECORD POINTER                          
*                                                                               
PREVREC  DS    CL1                                                              
SAVECLT  DS    CL3                                                              
SAVCLT2  DS    CL2                                                              
SAVEPROD DS    CL7                                                              
SAVEEST  DS    CL1                                                              
SAVENET  DS    CL4                                                              
SAVEDEM  DS    CL6                                                              
USERNUM  DS    CL2                                                              
PWORK    DS    PL16                                                             
PREVIOUS DS    CL44                                                             
RECFILTY DS    CL100               RECOVERY HEADER+PART OF RECORD               
RECDAT   DS    CL3                 RECOVERY DATE                                
RECTIME  DS    CL4                 RECOVERY TIME                                
                                                                                
COPYGRS  DS    PL8                 COPY REPORT TOTAL                            
CHGGRS   DS    PL8                 CHANGE REPORT TOTAL                          
*                                                                               
*                                                                               
*                                                                               
AGYNAMSV DS    CL33                                                             
AGYADRSV DS    CL33                                                             
PKCLISV  DS    CL2                 CLIENT FILTER                                
CLIENTNM DS    CL20                                                             
PRODNAME DS    CL20                                                             
ESTNAME  DS    CL20                                                             
*                                                                               
RCVRYSV  DS    CL60                                                             
TODAYB   DS    CL3                 BINARY                                       
TODAYC   DS    CL2                 COMPRESSED                                   
PREVFLG  DS    CL1                                                              
UPDTFLG  DS    CL1                                                              
HISTFLG  DS    CL1                                                              
WRITEFLG DS    CL1                                                              
SVSRTCLT DS    CL3                                                              
UNITS    DS    F                                                                
PUTNUM   DS    F                                                                
ELMNUM   DS    F                                                                
SAVAMC   DS    CL3                 SAVE AGY/MED/CLT OF HIST REC                 
MYKEY    DS    CL40                                                             
                                                                                
MYP      DS    CL132                                                            
MYSPACES DS    CL132                                                            
*                                                                               
WHOTBL   DS    CL(12*50)           ROOM FOR 50 ID(2)/AGY(2)/NAME(8)             
WHOTBLX  DS    CL1                 END OF TABLE = X'FF'                         
*                                                                               
*********************************************************                       
*                                                                               
SRTTBL   DS    0CL200    *** NOTE HARD CODED                                    
SRTKCLT  DS    CL3               * CLIENT           HIST KEY                    
SRTKGRP  DS    CL4               * AUDIT GROUP                                  
SRTKNET  DS    CL4               * NETWORK          HIST KEY                    
SRTKPRG  DS    CL6               * PROGRAM          HIST KEY                    
SRTKDAT  DS    CL2               * AIR DATE         HIST KEY                    
SRTKEST  DS    CL1               * ESTIMATE         HIST KEY                    
SRTKSUB  DS    CL1               * SUB LINE         HIST KEY                    
SRTKCHD  DS    CL2               * CHANGEDATE                                   
SRTKTIM  DS    CL4               * CHANGETIME                                   
         DS    CL3               * SPARE                                        
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
SRREASON DS    CL3                 REASON                                       
SRSTATUS DS    CL1                 STATUS BYTE                                  
*                                  X'01' = REMOVED MKGD/MSD ELEM                
SRCOMN   DS    CL60                COMMENT                                      
**SRBUYER  DS    CL8                 USER CODE                                  
PUT$$$   DS    PL8                 DOLLARS PUT BACK TO UNITS                    
***SRMKGMSD DS    CL35                MAKEGOOD/MISSED                           
TOTPUT$  DS    PL8                                                              
SRPRMT   DS    CL1                 PREEMPT C'Y'                                 
SRTAMC   DS    CL3                 AGY/MEDIA/CLI                                
         DS    CL24                SPARE                                        
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
*                                                                               
RECD     DSECT                                                                  
RECLN    DS    XL2                                                              
         DS    CL2                                                              
REC      DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICFD                                                       
*                                                                               
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034NEWRI85X  05/01/02'                                      
         END                                                                    
