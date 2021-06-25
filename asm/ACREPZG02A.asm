*          DATA SET ACREPZG02A AT LEVEL 037 AS OF 10/18/00                      
***********************************************************************         
*              OPT1:'Y'= RUN AT CLIENT LEVEL ONLY                     *         
*              OPT2:' '= RUN ALL AGENCIES                             *         
*                   'B'= RUN ONLY ON BATES                            *         
*                   'D'= RUN ONLY ON SAATCHI & SAATCHI                *         
*                   'C'= RUN ON BD/NE                                           
*              OPT3:' '= RUN ON ALL FILES                             *         
*                   'N'= RUN ON NETWORK FILES ONLY                    *         
*                   'P'= RUN ON PRINT   FILES ONLY                    *         
*                   'S'= RUN ON SPOT    FILES ONLY                    *         
*              OPT4:'D'= DUMP RECORDS - MAX. 15                       *         
*              OPT5:'T'= OUTPUT IS TAPE - SJ RECORDS                  *         
*                  :'Y'= CTFILE TAPE CREATION/SUPRESS PRINTOUTS 4 & 5 *         
*              OPT6:' '= RUN ALL PRINT ROUTINES                       *         
*                   '1'= RUN PRINT ROUTINE 1 ONLY                     *         
*                   '2'= RUN PRINT ROUTINE 2 ONLY                     *         
*                   '3'= RUN PRINT ROUTINE 3 ONLY                     *         
*              OPT7:'T'= OUTPUT IS TAPE - CTFILE RECORDS              *         
***********************************************************************         
*PHASE ACZG02A                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'FILE CREATION'                                                  
ACZG02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZG**,R7,R9    BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACZGD,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     RF,MCUTL                                                         
         ST    RF,AUTL                                                          
         MVC   UTL(L'UTL),0(RF)     SAVE PRESENT UTL SETTING                    
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         USING BIND,R1                                                          
         L     R1,AAGNTAB          A(AGENCY TABLE)                              
         XC    BININ,BININ         CLEAR AGENCY TABLE - BIN TABLE 2             
*                                                                               
         L     R1,ASJTAB           A(SJ TABLE)                                  
         XC    BININ,BININ         CLEAR SJ TABLE - BIN TABLE 3                 
         DROP  R1                                                               
*                                                                               
         LA    R1,PKFLDS           INITIALIZE PACKED FIELDS R1=(START)          
         LA    R0,PKNUMQ                                                        
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         XC    SYSSAVE,SYSSAVE     CLEAR SAVED AREA FOR SYSTEM ID               
         XC    SVAGY,SVAGY         CLEAR SAVED AREA FOR AGENCY ID               
         ZAP   DMPTOT,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
         NI    OPTN,TURNOFF-OPTTAPE                                             
         CLI   QOPT1,C'Y'          RUN AT CLIENT LEVEL                          
         BNE   *+8                                                              
         OI    OPTN,OPTCLT                                                      
         CLI   QOPT2,C' '          PRINT ONLY SELECT AGENCY                     
         BE    *+14                                                             
         OI    OPTN,OPTAGY                                                      
         MVC   SVAGY,QOPT2         MOVE AGENCY INTO SAVED AREA                  
         CLI   QOPT3,C' '          RUN ON SPECIFIC SYSTEM ONLY                  
         BE    REQF20                                                           
         MVC   SYSSAVE,QOPT3                                                    
         LA    R1,SYSTAB           R1 = A(SYSTEM TABLE)                         
         USING SYSTBLD,R1                                                       
REQF10   CLI   0(R1),X'FF'                                                      
         BE    REQF20                                                           
         CLC   SYSTID,SYSSAVE                                                   
         BE    *+12                                                             
         LA    R1,SYSTLNQ(R1)      BUMP TO NEXT ENTRY                           
         B     REQF10                                                           
         DROP  R1                                                               
*                                                                               
         OI    OPTN,OPTSYS         TURN ON BIT ONLY IF MATCH IS FOUND           
REQF20   CLI   QOPT4,C'D'          DUMP OUTPUT                                  
         BNE   *+8                                                              
         OI    OPTN,OPTDUMP                                                     
         CLI   QOPT6,C' '                                                       
         BNE   *+12                                                             
         OI    OPTNPRT,OPTPRT1+OPTPRT2+OPTPRT3+OPTPRT4  ALL PRINT OPTS          
         B     REQF30                                                           
         CLI   QOPT6,C'M'          PRINT MEDIA REPORTS                          
         BNE   *+8                                                              
         OI    OPTNPRT,OPTPRT1+OPTPRT2    PRINT MEDIA                           
         CLI   QOPT6,C'1'          PRINT ROUTINE 1                              
         BNE   *+8                                                              
         OI    OPTNPRT,OPTPRT1     TURN ON PRINT ROUTINE 1                      
         CLI   QOPT6,C'2'          PRINT ROUTINE 2                              
         BNE   *+8                                                              
         OI    OPTNPRT,OPTPRT2     TURN ON PRINT ROUTINE 2                      
         CLI   QOPT6,C'3'          PRINT ROUTINE 3                              
         BNE   *+8                                                              
         OI    OPTNPRT,OPTPRT3     TURN ON PRINT ROUTINE 3                      
         CLI   QOPT6,C'4'          PRINT ROUTINE 4                              
         BNE   *+8                                                              
         OI    OPTNPRT,OPTPRT4     TURN ON PRINT ROUTINE 4                      
REQF30   CLI   QOPT5,C'T'          TAPE OUTPUT - FOR SJ RECORDS                 
         BNE   REQF35                                                           
         L     R3,AOUTFIL                                                       
         OPEN  ((R3),(OUTPUT))                                                  
REQF35   CLI   QOPT7,C'T'          TAPE OUTPUT - CTFILE RECORDS                 
         BNE   REQF40                                                           
         L     R3,ATOUT                                                         
         OPEN  ((R3),(OUTPUT))                                                  
         CLI   QOPT5,C'Y'          DO WE WANT TO SUPRESS PRINTING?              
         BNE   REQF40                NO, LET ALL PRINTS RUN                     
         NI    OPTNPRT,TURNOFF-(OPTPRT3+OPTPRT4)  FOR CT TURNOFF 3/4            
REQF40   DS    0H                                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD     INITIALIZE SORTER             
*                                                                               
         XC    SVSENUM,SVSENUM     CLEAR SAVED AREA FOR SYS SE#                 
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   SVSENUM,4(R1)       SAVE SYSTEM SE NUMBER                        
         USING SYSTBLD,R2                                                       
         LA    R2,SYSTAB           R2 = A(SYSTEM TABLE)                         
         XC    SESAVE,SESAVE       CLEAR SAVED AREA FOR SE #                    
REQF50   CLI   0(R2),X'FF'         CHECK FOR END OF TABLE                       
         BE    REQFX               EXIT LOOP                                    
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),SYSTSE      MOVE SE# INTO UTL+4                          
         TM    OPTN,OPTSYS         TEST IF SELECTED SYSTEM REQUESTED            
         BNO   *+14                                                             
         CLC   SYSTID,SYSSAVE      COMPARE ON ID'S                              
         BNE   REQF70                                                           
         TM    OPTN,OPTAGY         TEST IF SELECTED SYSTEM REQUESTED            
         BNO   *+14                                                             
         CLC   SYSTAGY,SVAGY       COMPARE ON AGENCIES                          
         BNE   REQF70                                                           
         CLC   SESAVE,SYSTSE       CHECK IF SE MATCHES PREVIOUS SE              
         BE    REQF60                IF EQUAL SKIP OPEN ROUTINE                 
         CLC   SVSENUM,SYSTSE      CHECK IF SE MATCHES ORIGINAL SE              
         BE    REQF60                IF EQUAL SKIP OPEN ROUTINE                 
         SR    R5,R5                                                            
         ICM   R5,15,ASYSFIL                                                    
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',SYSTNAM,(R5)                             
REQF60   MVC   SESAVE,SYSTSE       MOVE IN NEW SE# TO SAVE AREA                 
         SR    RF,RF               INITIALIZE RF FOR BASR                       
         ICM   RF,15,ASYSRD        RF = A(SYSTEM READ ROUTINE)                  
         BASR  RE,RF               READ RECORDS AND SEND TO SORTER              
REQF70   LA    R2,SYSTLNQ(R2)      BUMP TO NEXT ENTRY                           
         B     REQF50                                                           
*                                                                               
REQFX    DS    0H                                                               
         BAS   RE,RUNLST                                                        
         GOTO1 ADSORTER,DMCB,=C'END'                                            
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),SVSENUM     RESET SYSTEM SE NUMBER                       
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
         SPACE 1                                                                
RUNLST   NTR1                                                                   
         XC    FLAG,FLAG           SET FLAG TO NULLS FOR XSRT RTE               
         TM    OPTNPRT,OPTPRT1     TEST IF 1ST P/O REQUESTED                    
         BNO   RUNLST10              IF NOT SKIP                                
         MVI   RCSUBPRG,1          SET FOR PRINTOUT 1                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         BAS   RE,PRTRTE           RETREIVE RECORDS FROM 1ST TABLE              
*                                                                               
RUNLST10 TM    OPTNPRT,OPTPRT2     TEST IF 2ND P/O REQUESTED                    
         BNO   RUNLST20              IF NOT SKIP                                
         MVI   RCSUBPRG,2          SET FOR PRINTOUT 2                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         BAS   RE,XSRT             XSORT AND RETRIEVE FROM 2ND TABLE            
*                                                                               
RUNLST20 TM    OPTNPRT,OPTPRT3     TEST IF 3RD P/O REQUESTED                    
         BNO   RUNLST30              IF NOT SKIP                                
         MVI   RCSUBPRG,0          SET FOR PRINTOUT 3                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         BAS   RE,GETSRT           RETRIEVE RECORDS FROM SORT                   
*                                                                               
RUNLST30 TM    OPTNPRT,OPTPRT4     TEST IF 4TH P/O REQUESTED                    
         BNO   RUNLST40              IF NOT SKIP                                
         MVI   RCSUBPRG,3          SET FOR PRINTOUT 4                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   DMPTOT,=P'0'        RE-INITIALIZE DUMP COUNTER                   
         BAS   RE,BLDPST           BUILD POSTING RECORDS                        
         BAS   RE,PRTPST           PRINT POSTING ACCOUNTS                       
*                                                                               
RUNLST40 CLI   QOPT7,C'T'          DO THEY WANT CTFILE BUILT?                   
         BNE   RUNLST50                                                         
         MVI   RCSUBPRG,4          SET FOR PRINTOUT 5                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         OI    FLAG,FLGCTF         TURN ON CTFILE FLAG FOR XSRT                 
         BAS   RE,XSRT             XSORT AND RETRIEVE FROM 2ND TABLE            
         ZAP   DMPTOT,=P'0'        RE-INITIALIZE DUMP COUNTER                   
         BAS   RE,BLDCTF           BUILD CONTROL FILE                           
*                                                                               
RUNLST50 MVI   RCSUBPRG,5          SET FOR PRINTOUT 6 - TOTALS                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         BAS   RE,PRTTOT                                                        
*                                                                               
         CLI   QOPT5,C'T'          SJ RECORD TAPE OPEN?                         
         BNE   *+12                                                             
         L     R3,AOUTFIL                                                       
         B     RUNLST60            CLOSE TAPE                                   
         CLI   QOPT7,C'T'          CT RECORD TAPE OPEN?                         
         BNE   RUNLSTX                                                          
         L     R3,ATOUT                                                         
*                                                                               
RUNLST60 DS    0H                                                               
         CLOSE ((R3))              CLOSE TAPE                                   
*                                                                               
RUNLSTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ ACC FILES                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SYSTBLD,R2                                                       
ACCRD    NTR1                                                                   
         USING BIND,R4                                                          
         L     R4,ACLITAB          R4 = A(BINADD TABLE)                         
         XC    BININ,BININ         CLEAR BINSEARCH TABLE                        
         MVC   DKEY,SPACES                                                      
         XC    SVAGCLT,SVAGCLT     CLEAR SAVED AREA FOR CLIENT CODE             
         XC    SAVEID,SAVEID       CLEAR FIELD FOR COMPANY ID                   
         MVC   SAVEID,SYSTCODE     MOVE IN AGENCY CHARACTER CODE                
         USING ACTRECD,R4                                                       
         LA    R4,DKEY             R4 = A(KEY)                                  
         MVC   ACTKCPY,SYSTCPY     COMPANY CODE                                 
         MVC   ACTKUNT(2),=C'SJ'   MOVE IN UNIT/LEDGER 'SJ'                     
*                                                                               
ACCRD10  DS    0H                                                               
         BAS   RE,DMHIADIR         READ HI ON SPOT DIR                          
         B     ACCRD20                                                          
*                                                                               
ACCRD15  DS    0H                                                               
         BAS   RE,DMSEADIR           READ SEQ FOR SPOT DIR                      
ACCRD20  LA    R4,DIR                R4 = A(RETURNED KEY)                       
         CLC   DIR(ACTKACT-ACTKEY),DKEY  COMPARE TYPE WITH ORIGINAL KEY         
         BNE   ACCRDX                                                           
         CLC   ACTKACT(3),SPACES     CLIENT LEVEL?                              
         BE    ACCRD15                                                          
         CLC   ACTKACT+3(9),SPACES                                              
         BNE   ACCRD15                                                          
         BAS   RE,ACCBLD             CLIENT ROUTINENE                           
         MVI   ACTKACT+3,X'FF'       GET NEXT CLIENT RECORD                     
         B     ACCRD15               IF NOT FOUND READ SEQ                      
*                                                                               
ACCRDX   DS    0H                                                               
         BAS   RE,SYSOUT           WRITE OUTPUT RECORDS TO SORT                 
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* READ SPOT/NET FILES                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING SYSTBLD,R2                                                       
SPTRD    NTR1                                                                   
         USING BIND,R4                                                          
         L     R4,ACLITAB          R4 = A(BINADD TABLE)                         
         XC    BININ,BININ         CLEAR BINSEARCH TABLE                        
         XC    DKEY,DKEY                                                        
         XC    SAVEKEY,SAVEKEY     CLEAR SAVED AREA FOR KEY                     
         XC    SAVEID,SAVEID       CLEAR FIELD FOR COMPANY ID                   
         MVC   SAVEID,SYSTCODE     MOVE IN AGENCY CHARACTER CODE                
         USING SPESTD,R4                                                        
         LA    R4,DKEY             R4 = A(KEY)                                  
         MVI   EKEYTYPE,X'00'      RECORD TYPE                                  
         MVC   EKEYAM,SYSTMED      MOVE IN AGENCY/MEDIA                         
*                                                                               
SPTRD10  DS    0H                                                               
         BAS   RE,DMHISDIR         READ HI ON SPOT DIR                          
         B     SPTRD20                                                          
*                                                                               
SPTRD15  DS    0H                                                               
         BAS   RE,DMSESDIR           READ SEQ FOR SPOT DIR                      
SPTRD20  LA    R4,DIR                R4 = A(RETURNED KEY)                       
         CLC   DIR(L'EKEYTYPE),DKEY  COMPARE TYPE WITH ORIGINAL KEY             
         BNE   SPTRDX                                                           
         MVC   SAVEAGN,EKEYAM            MOVE IN AGENCY MEDIA                   
         NI    SAVEAGN,X'F0'             TURNOFF ALL MEDIA BITS                 
         CLC   DKEY+1(L'EKEYAM),SAVEAGN  COMPARE AGENCY W/ORIGINAL KEY          
         BNE   SPTRDX                                                           
         OC    EKEYEST,EKEYEST     CHECK IF ESTIMATE CODE EXISTS                
         BZ    SPTRD15               IF NOT - READ SEQUENTIAL                   
         BAS   RE,SPEST              ESTIMATE ROUTINE                           
         B     SPTRD15               IF NOT FOUND READ SEQ                      
*                                                                               
SPTRDX   DS    0H                                                               
         BAS   RE,SYSOUT           WRITE OUTPUT RECORDS TO SORT                 
         BAS   RE,SYSRTE                                                        
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* READ PRINT FILES                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SYSTBLD,R2                                                       
PRTRD    NTR1                                                                   
         USING BIND,R4                                                          
         L     R4,ACLITAB          R4 = A(BINADD TABLE)                         
         XC    BININ,BININ         CLEAR BINSEARCH TABLE                        
         XC    DKEY,DKEY           CLEAR KEY                                    
         XC    SAVEKEY,SAVEKEY     CLEAR SAVED AREA FOR KEY                     
         XC    SAVEID,SAVEID       CLEAR FIELD FOR COMPANY ID                   
         MVC   SAVEID,SYSTCODE     MOVE IN AGENCY CHARACTER CODE                
         USING PRESTD,R4                                                        
         LA    R4,DKEY             R4 = A(KEY)                                  
         LA    R3,MEDTAB           R3 = A(MEDIA CODE TABLE)                     
PRTRD10  CLI   0(R3),X'FF'         COMPARE FOR END OF TABLE                     
         BE    PRTRDX                                                           
         MVC   PESTKAGY,SYSTCODE   MOVE IN AGENCY                               
         MVC   PESTKMED,0(R3)      MOVE IN MEDIA                                
         MVI   PESTKRCD,X'07'      ESTIMATE RECORD CODE                         
*                                                                               
PRTRD20  DS    0H                                                               
         BAS   RE,DMHIPDIR         READ HIGH FOR PRINT DIR                      
         B     PRTRD30                                                          
*                                                                               
PRTRD25  DS    0H                                                               
         BAS   RE,DMSEPDIR                 READ SEQ FOR SPOT DIR                
PRTRD30  CLC   DIR(PESTKCLT-PESTREC),DKEY  COMPARE WITH ORIGINAL KEY            
         BNE   PRTRD40                                                          
         BAS   RE,PREST                    PRINT ESTIMATE ROUTINE               
         B     PRTRD25                                                          
*                                                                               
PRTRD40  LA    R3,L'MEDTAB(R3)     BUMP R3 TO NEXT MEDIA CODE                   
         B     PRTRD10                                                          
*                                                                               
PRTRDX   DS    0H                                                               
         BAS   RE,SYSOUT           WRITE OUTPUT RECORDS TO SORT                 
         BAS   RE,SYSRTE                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD CLIENT TABLE  (ACC)                                           *         
***********************************************************************         
         SPACE 1                                                                
ACCBLD   NTR1                                                                   
         BAS   RE,DMGETACC                                                      
         USING ACTRECD,R4                                                       
         L     R4,AIO1             R4 = A(IO1)                                  
*                                                                               
         LA    R6,CLIWRK                                                        
         USING CLID,R6                                                          
         XC    CLIWRK,CLIWRK                                                    
         XC    NAME,NAME                                                        
         MVC   CLICLI(3),ACTKACT            MOVE IN CLIENT  CODE                
         MVC   CLIAGY,SAVEID                MOVE IN AGENCY  CODE                
         MVI   CLISYS,X'40'                 MOVE IN SYSTEM FLAG - ACC           
         MVC   CLISE,SESAVE                 MOVE IN SE NUMBER                   
         MVC   CLIID,SYSTID                 MOVE IN SYSTEM ID                   
         MVC   CLIAGNID,SYSTAGY             MOVE IN AGENCY ID                   
         AH    R4,=Y(L'ACCKEY+L'ACCRLEN+L'ACCRSTA+L'ACCRLNK)                    
         USING NAMELD,R4                                                        
ACCBLD10 CLI   0(R4),0             END OF ELEMENTS?                             
         BNE   *+6                                                              
         DC    H'0'                DIE IF END OF ELEMENTS                       
         CLI   0(R4),NAMELQ        NAME ELEMENT                                 
         BE    ACCBLD20                                                         
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     ACCBLD10                                                         
ACCBLD20 SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   CLINAM(0),NAMEREC                                                
         EX    R1,*+4                                                           
         MVC   NAME(0),NAMEREC   SAVE NAME FOR NEXT BINADD                      
         GOTO1 BINADD,DMCB,CLIWRK,ACLITAB                                       
*                                                                               
         USING ACTRECD,R4                                                       
         L     R4,AIO1             R4 = A(IO1) - RESET R4                       
         LA    R6,AGNWRK                                                        
         USING AGND,R6                                                          
         XC    AGNWRK,AGNWRK                                                    
         XC    SVMEDLS,SVMEDLS     CLEAR SAVED AREA FOR MEDIA LIST              
         XC    SPTMED,SPTMED       CLEAR SAVED AREA FOR SPOT/NET MEDIA          
         XC    SVSYSID,SVSYSID     CLEAR SAVED AREA FOR SYSTEM ID               
*                                                                               
         MVC   AGNACLI,ACTKACT     MOVE IN CLIENT CODE                          
         MVC   AGNSCLI,AGNACLI     MOVE IN CLIENT CODE FOR SORT                 
         MVC   AGNAGY,SAVEID       MOVE IN AGENCY  CODE                         
         MVI   AGNSYS,X'40'        MOVE IN SYSTEM FLAG - SPOT                   
         MVC   AGNID,SYSTID        MOVE IN SYSTEM ID                            
         MVC   AGNSYSID,SYSTAGY    MOVE IN AGENCY ID                            
         MVC   AGNCNAM,NAME        PUT NAME IN TABLE                            
         MVC   AGNACMD(2),=C'AC'   MOVE 'AC' INTO MEDIA LIST                    
         ZAP   AGNBKT,=P'1'                                                     
         MVC   SVSYSID,SYSTID      SAVE SYSTEM ID                               
         GOTO1 BINADD,DMCB,AGNWRK,AAGNTAB                                       
*                                                                               
ACCBLDX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADD CLIENT/PRODUCT TO TABLE FOR QUALIFYING ESTIMATES (SPOT/NET)     *         
***********************************************************************         
         SPACE 1                                                                
SPEST    NTR1                                                                   
         BAS   RE,DMGETSPT                                                      
         USING SPESTD,R4                                                        
         L     R4,AIO1             R4 = A(IO1)                                  
*                                                                               
         CLC   EEND,=C'930101'     CHECK FOR ACTIVITY IN LAST 2 YRS             
         BL    EXIT                  IF NOT EXIT W/INEQUALITY                   
         LA    R6,CLIWRK                                                        
         USING CLID,R6                                                          
         XC    CLIWRK,CLIWRK                                                    
         MVC   CLICLI(L'EKEYCLT),EKEYCLT    MOVE IN CLIENT  CODE                
         MVC   CLIPRD,EKEYPRD               MOVE IN PRODUCT CODE                
         MVC   CLIAGY,SAVEID                MOVE IN AGENCY  CODE                
         MVC   CLIMED,EKEYAM                MOVE IN AGENCY/MEDIA CODE           
         MVI   CLISYS,X'80'                 MOVE IN SYSTEM FLAG - SPOT          
         MVC   CLISE,SESAVE                 MOVE IN SE NUMBER                   
         MVC   CLIID,SYSTID                 MOVE IN SYSTEM ID                   
         MVC   CLIAGNID,SYSTAGY             MOVE IN AGENCY ID                   
         GOTO1 BINADD,DMCB,CLIWRK,ACLITAB                                       
*                                                                               
         CLC   SAVEKEY(EKEYEST-EKEY),DIR                                        
         BE    SPESTX                                                           
         MVC   SAVEKEY(EKEYEST-EKEY),DIR                                        
         LA    R6,AGNWRK                                                        
         USING AGND,R6                                                          
         XC    AGNWRK,AGNWRK                                                    
         XC    SVMEDLS,SVMEDLS     CLEAR SAVED AREA FOR MEDIA LIST              
         XC    SPTMED,SPTMED       CLEAR SAVED AREA FOR SPOT/NET MEDIA          
         XC    SVSYSID,SVSYSID     CLEAR SAVED AREA FOR SYSTEM ID               
         MVC   AGNPCLI,EKEYCLT     MOVE IN PACKED CLIENT CODE                   
         GOTO1 CLUNPK,DMCB,EKEYCLT,AGNACLI                                      
         MVC   AGNSCLI,AGNACLI     MOVE IN CLIENT CODE FOR SORT                 
         MVC   AGNAGY,SAVEID                MOVE IN AGENCY  CODE                
         MVC   AGNMED,EKEYAM                MOVE IN AGENCY/MEDIA CODE           
         MVI   AGNSYS,X'80'                 MOVE IN SYSTEM FLAG - SPOT          
         MVC   AGNID,SYSTID                 MOVE IN SYSTEM ID                   
         MVC   AGNSYSID,SYSTAGY             MOVE IN AGENCY ID                   
*                                                                               
*        CHECK FOR MEDIA AND MOVE IT IN BINTABLE                                
*                                                                               
         MVC   SPTMED,EKEYAM                                                    
         MVC   SVMEDLS(1),SYSTID            MOVE IN SYSTEM ID                   
         NI    SPTMED,X'0F'        TURNOFF ALL AGENCY BITS                      
         LA    R5,SPTTAB           R5 = A(SPOT/NET MEDIA TABLE)                 
SPEST05  CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                MEDIA MUST EXIST                             
         CLC   0(1,R5),SPTMED      COMPARE MEDIA TO TABLE                       
         BE    *+12                                                             
         LA    R5,L'SPTTAB(R5)                                                  
         B     SPEST05                                                          
         MVC   SVMEDLS+1(1),1(R5)                                               
*                                                                               
         CLI   SYSTID,C'S'                  CHECK IF SPOT                       
         BNE   SPEST10                                                          
         MVC   AGNSPSE,SESAVE               MOVE IN SPOT SE NUMBER              
         MVC   AGNSPMD(2),SVMEDLS  MOVE CURRENT MEDIA LIST INTO STORAGE         
         B     SPEST20                                                          
*                                                                               
SPEST10  DS    0H                                                               
         MVC   AGNNTSE,SESAVE               MOVE IN SPOT SE NUMBER              
         MVC   AGNNTMD(2),SVMEDLS  MOVE CURRENT MEDIA LIST INTO TABLE           
*                                                                               
SPEST20  DS    0H                                                               
         ZAP   AGNBKT,=P'1'                                                     
         MVC   SVSYSID,SYSTID      SAVE SYSTEM ID                               
         GOTO1 BINADD,DMCB,AGNWRK,AAGNTAB                                       
*                                                                               
SPESTX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADD CLIENT/PRODUCT TO TABLE FOR QUALIFYING ESTIMATES (PRINT)        *         
***********************************************************************         
         SPACE 1                                                                
PREST    NTR1                                                                   
         BAS   RE,DMGETPRT                                                      
         USING PRESTD,R4                                                        
         L     R4,AIO1             R4 = A(IO1)                                  
*                                                                               
         CLC   PESTEND,=C'930101'    CHECK FOR ACTIVITY IN LAST 2 YRS           
         BL    EXIT                  IF NOT EXIT                                
         LA    R6,CLIWRK           R2 = A(WORK AREA FOR BINADD)                 
         USING CLID,R6                                                          
         XC    CLIWRK,CLIWRK       CLEAR WORK AREA                              
         MVC   CLICLI,PESTKCLT     MOVE CLIENT  CODE INTO WORK AREA             
         MVC   CLIPRD,PESTKPRD     MOVE PRODUCT CODE INTO WORK AREA             
         MVC   CLIAGY,PESTKAGY     MOVE AGENCY  CODE INTO WORK AREA             
         MVC   CLIMED,PESTKMED     MOVE MEDIA   CODE INTO WORK AREA             
         MVC   CLISE,SESAVE        MOVE SE NUMBER    INTO WORK AREA             
         MVC   CLIID,SYSTID                 MOVE IN SYSTEM ID                   
         MVC   CLIAGNID,SYSTAGY             MOVE IN AGENCY ID                   
         GOTO1 BINADD,DMCB,CLIWRK,ACLITAB                                       
*                                                                               
         CLC   SAVEKEY(PESTKEST-PESTKEY),DIR                                    
         BE    PRESTX                                                           
         MVC   SAVEKEY(PESTKEST-PESTKEY),DIR                                    
         LA    R6,AGNWRK           R2 = A(WORK AREA FOR BINADD)                 
         USING AGND,R6                                                          
         XC    AGNWRK,AGNWRK       CLEAR WORK AREA                              
         XC    SVMEDLS,SVMEDLS     CLEAR SAVED AREA FOR MEDIA LIST              
         XC    SVSYSID,SVSYSID     CLEAR SAVED AREA FOR SYSTEM ID               
         MVC   AGNACLI,PESTKCLT    MOVE CLIENT  CODE INTO WORK AREA             
         MVC   AGNSCLI,AGNACLI     MOVE IN CLIENT CODE FOR SORT                 
         MVC   AGNAGY,PESTKAGY     MOVE AGENCY  CODE INTO WORK AREA             
         MVC   AGNMED,PESTKMED     MOVE MEDIA   CODE INTO WORK AREA             
         MVC   AGNPTSE,SESAVE      MOVE SE NUMBER    INTO WORK AREA             
         ZAP   AGNBKT,=P'1'                                                     
         MVC   AGNPTMD(1),SYSTID   MOVE IN SYSTEM ID                            
         MVC   AGNPTMD+1(1),PESTKMED                                            
         MVC   SVMEDLS,AGNPTMD     MOVE CURRENT MEDIA LIST INTO STORAGE         
         MVC   SVSYSID,SYSTID      SAVE SYSTEM ID                               
         MVC   AGNSYSID,SYSTAGY             MOVE IN AGENCY ID                   
         MVC   AGNID,SYSTID                 MOVE IN SYSTEM ID                   
         GOTO1 BINADD,DMCB,AGNWRK,AAGNTAB                                       
*                                                                               
PRESTX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* PUT CLIENT/PRODUCT RECORDS TO SORT                                  *         
***********************************************************************         
         SPACE 1                                                                
SYSOUT   NTR1                                                                   
         L     R5,ACLITAB                                                       
         USING BIND,R5                                                          
         SR    R0,R0                                                            
         ICM   R0,15,BININ         R0=NUMBER IN TABLE                           
         BZ    SYSOUTX             NOTHING TO PROCESS                           
         XC    LSTCLI,LSTCLI                                                    
         LA    R6,BINTAB                                                        
         USING CLID,R6                                                          
SYSOUT10 OC    CLISYS,CLISYS       TEST IF SYSTEM FLAG IS SET                   
         BZ    SYSOUT25            IF NOT SET - PRINT                           
         CLI   CLISYS,X'80'        X'80' SPOT/NET FILE                          
         BNE   SYSOUT20              IF NOT SPOT/NET MUST BE ACC                
         CLC   CLICLI,LSTCLI       COMPARE IF CLIENT IS SAME AS BEFORE          
         BE    *+8                   IF IT IS SKIP CLIENT ROUTINE               
         BAS   RE,SPCLT            PROCESS CLIENT  RECORD (SPOT/NET)            
         BAS   RE,SPPRD            PROCESS PRODUCT RECORD (SPOT/NET)            
         B     SYSOUT30                                                         
*                                                                               
SYSOUT20 BAS   RE,ACCLT            ONLY CLIENT ROUTINE FOR ACC                  
         B     SYSOUT30                                                         
*                                                                               
SYSOUT25 CLC   CLICLI,LSTCLI       COMPARE IF CLIENT IS SAME AS BEFORE          
         BE    *+8                   IF IT IS SKIP CLIENT ROUTINE               
         BAS   RE,PRCLT            PROCESS CLIENT  RECORD (PRINT)               
         BAS   RE,PRPRD            PROCESS PRODUCT RECORD (PRINT)               
*                                                                               
SYSOUT30 MVC   LSTCLI,CLICLI       UPDATE LAST CLIENT FIELD                     
         LA    R6,CLILNQ(R6)       BUMP TO NEXT ENTRY                           
         BCT   R0,SYSOUT10                                                      
*                                                                               
SYSOUTX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS CLIENT ROUTINE  (ACC)                                       *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
ACCLT    NTR1                                                                   
         LA    R2,SRTBUFF          R2 = A(SAVED SORT BUFFER)                    
         USING SORTD,R2                                                         
         XC    SRTBUFF,SRTBUFF     CLEAR SORT BUFFER                            
         XC    SAVEID,SAVEID                                                    
*                                                                               
         MVC   SRTCLT,CLICLI       MOVE OVERRIDE INTO SORT                      
         MVC   SRTCNAM,CLINAM      MOVE CLIENT NAME TO STORAGE                  
         MVC   SRTSYS,CLIID        MOVE SYSTEM ID TO SORT FIELD                 
         MVC   SRTAGNID,CLIAGNID   MOVE AGENCY ID TO SORT FIELD                 
         MVC   SAVEID,CLIAGY       MOVE IN AGENCY TO SAVED AREA                 
         BAS   RE,SRTRTE           SORT ROUTINE                                 
*                                                                               
ACCLTX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS CLIENT ROUTINE  (SPOT/NET)                                  *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
SPCLT    NTR1                                                                   
         LA    R2,SRTBUFF          R2 = A(SAVED SORT BUFFER)                    
         USING SORTD,R2                                                         
         LA    R4,DKEY                                                          
         USING SPCLID,R4                                                        
         XC    SRTBUFF,SRTBUFF     CLEAR SORT BUFFER                            
         XC    DKEY,DKEY                                                        
         XC    SAVEID,SAVEID                                                    
         XC    SVZEN,SVZEN         CLEAR AREA FOR ZENITH CODE                   
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,CLIMED       AGY/MED                                      
         MVC   CKEYCLT,CLICLI      CLIENT CODE                                  
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),CLISE       MOVE SE# INTO UTL+4                          
         BAS   RE,DMHISDIR                                                      
         CLC   DIR(CKEYZRO-CLTHDR),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETSPT                                                      
         L     R4,AIO1             R4 = A(IO1)                                  
*                                                                               
         CLC   CZENCLT,SPACES      CHECK IF ZENITH CODE EXIST                   
         BH    SPCLT10                                                          
         GOTO1 CLUNPK,DMCB,CKEYCLT,SRTCLT                                       
         B     SPCLT20                                                          
SPCLT10  MVC   SRTCLT,CZENCLT      MOVE OVERRIDE INTO SORT                      
         MVC   SVZEN,CZENCLT       SAVE ZENITH CODE FOR PRODUCT                 
         GOTO1 CLUNPK,DMCB,CKEYCLT,SRTACLT                                      
SPCLT20  MVC   SRTCNAM,CNAME       MOVE CLIENT NAME TO STORAGE                  
         MVC   SRT1CPRD,CLIPRD     MOVE IN PRODUCT CODE                         
         MVC   SRTSYS,CLIID        MOVE SYSTEM ID TO SORT FIELD                 
         MVC   SRTAGNID,CLIAGNID   MOVE AGENCY ID TO SORT FIELD                 
         MVC   SAVEID,CLIAGY       MOVE IN AGENCY TO SAVED AREA                 
         BAS   RE,SRTRTE           SORT ROUTINE                                 
*                                                                               
SPCLTX B       EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS PRODUCT ROUTINE (SPOT/NET)                                  *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
SPPRD    NTR1                                                                   
         LA    R2,SRTBUFF          R2 = A(SAVED SORT BUFFER)                    
         USING SORTD,R2                                                         
         LA    R4,DKEY                                                          
         USING SPPRDD,R4                                                        
         XC    SRTBUFF,SRTBUFF     CLEAR SORT BUFFER                            
         XC    DKEY,DKEY                                                        
         XC    SAVEID,SAVEID                                                    
         MVI   PKEYTYPE,0                                                       
         MVC   PKEYAM,CLIMED       AGY/MED                                      
         MVC   PKEYCLT,CLICLI      CLIENT CODE                                  
         MVC   PKEYPRD,CLIPRD      PRODUCT                                      
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),CLISE       MOVE SE# INTO UTL+4                          
         BAS   RE,DMHISDIR                                                      
         CLC   DIR(PKEYZRO-PRDHDR),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETSPT                                                      
         L     R4,AIO1             R4 = A(IO1)                                  
*                                                                               
         OC    SVZEN,SVZEN         CHECK IF ZENITH CODE EXIST                   
         BNZ   SPPRD10                                                          
         GOTO1 CLUNPK,DMCB,PKEYCLT,SRTCLT                                       
         B     SPPRD20                                                          
SPPRD10  MVC   SRTCLT,SVZEN        MOVE OVERRIDE INTO SORT                      
         GOTO1 CLUNPK,DMCB,PKEYCLT,SRTACLT                                      
SPPRD20  MVC   SRTPRD,PKEYPRD      MOVE IN PRODUCT CODE TO STORAGE              
         MVC   SRTPNAM,PNAME       MOVE PRODUCT NAME TO STORAGE                 
         MVC   SRTSYS,CLIID        MOVE SYSTEM ID TO SORT FIELD                 
         MVC   SRTAGNID,CLIAGNID   MOVE AGENCY ID TO SORT FIELD                 
         MVC   SAVEID,CLIAGY       MOVE IN AGENCY TO SAVED AREA                 
         BAS   RE,SRTRTE           SORT ROUTINE                                 
*                                                                               
SPPRDX   B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS CLIENT ROUTINE  (PRINT)                                     *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
PRCLT    NTR1                                                                   
         LA    R2,SRTBUFF          R2 = A(SAVED AREA FOR SORT)                  
         USING SORTD,R2                                                         
         LA    R4,DKEY             R4 = A(KEY)                                  
         USING PRCLID,R4                                                        
         XC    SRTBUFF,SRTBUFF     CLEAR SORT BUFFER                            
         XC    DKEY,DKEY           CLEAR KEY                                    
         XC    SAVEID,SAVEID                                                    
         XC    SVZEN,SVZEN                                                      
         MVC   PCLTKAGY,CLIAGY     MOVE AGENCY CODE INTO THE KEY                
         MVC   PCLTKMED,CLIMED     MOVE MEDIA  CODE INTO THE KEY                
         MVI   PCLTKRCD,X'02'      MOVE RECORD CODE INTO THE KEY                
         MVC   PCLTKCLT,CLICLI     MOVE CLIENT CODE INTO THE KEY                
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),CLISE       MOVE SE# INTO UTL+4                          
         BAS   RE,DMHIPDIR         READ HIGH FROM PRINT DIR                     
         CLC   DIR(7),DKEY         COMPARE KEY WITH ORIGINAL                    
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
*                                                                               
         BAS   RE,DMGETPRT         GET RECORD FROM PRINT FILE                   
         L     R4,AIO1                                                          
         MVC   SRTCLT,PCLTKCLT     MOVE IN CLIENT CODE                          
         MVC   SRT1CPRD,CLIPRD     MOVE IN PRODUCT CODE                         
         MVC   SRTCNAM,PCLTNAME    MOVE IN CLIENT NAME                          
         MVC   SRTSYS,CLIID        MOVE SYSTEM ID TO SORT FIELD                 
         MVC   SRTAGNID,CLIAGNID   MOVE AGENCY ID TO SORT FIELD                 
         MVC   SAVEID,CLIAGY       MOVE IN AGENCY TO SAVED AREA                 
         LA    R4,33(R4)           BUMP TO FIRST ELEMENT                        
         USING PCLTZEL,R4                                                       
PRCLT10  CLI   0(R4),X'00'         X'00' NO MORE ELEMENTS                       
         BE    PRCLT30                                                          
         CLI   0(R4),X'32'         X'32' ZENITH ELEMENT                         
         BNE   PRCLT20                                                          
         MVC   SRTACLT,SRTCLT                                                   
         MVC   SRTCLT,PCLTZEN      MOVE IN ZENITH CODE                          
         MVC   SVZEN,PCLTZEN                                                    
         B     PRCLT30                                                          
PRCLT20  SR    R1,R1                                                            
         IC    R1,1(R4)            INSERT ELEMENT LENGTH INTO R1                
         AR    R4,R1               BUMP TO NEXT ELEMENT                         
         B     PRCLT10                                                          
*                                                                               
PRCLT30  BAS   RE,SRTRTE           SORT ROUTINE                                 
*                                                                               
PRCLTX   B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS PRODUCT ROUTINE (PRINT)                                     *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
PRPRD    NTR1                                                                   
         LA    R2,SRTBUFF          R2 = A(SAVED AREA FOR SORT)                  
         USING SORTD,R2                                                         
         LA    R4,DKEY             R4 = A(KEY)                                  
         USING PRPRDD,R4                                                        
         XC    SRTBUFF,SRTBUFF     CLEAR SORT BUFFER                            
         XC    DKEY,DKEY           CLEAR KEY                                    
         XC    SAVEID,SAVEID                                                    
         MVC   PPRDKAGY,CLIAGY     MOVE AGENCY  CODE INTO THE KEY               
         MVC   PPRDKMED,CLIMED     MOVE MEDIA   CODE INTO THE KEY               
         MVI   PPRDKRCD,X'06'      MOVE RECORD  CODE INTO THE KEY               
         MVC   PPRDKCLT,CLICLI     MOVE CLIENT  CODE INTO THE KEY               
         MVC   PPRDKPRD,CLIPRD     MOVE PRODUCT CODE INTO THE KEY               
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),CLISE       MOVE SE# INTO UTL+4                          
*                                                                               
         BAS   RE,DMHIPDIR         READ HIGH FROM PRINT DIR                     
         CLC   DIR(10),DKEY        COMPARE KEY WITH ORIGINAL                    
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
*                                                                               
         BAS   RE,DMGETPRT         GET RECORD FROM PRINT FILE                   
         L     R4,AIO1                                                          
*                                                                               
         OC    SVZEN,SVZEN         CHECK IF ZENITH CODE EXIST                   
         BNZ   PRPRD10                                                          
         MVC   SRTCLT,PPRDKCLT     MOVE IN CLIENT  CODE                         
         B     PRPRD20                                                          
PRPRD10  MVC   SRTCLT,SVZEN        MOVE OVERRIDE INTO SORT                      
         MVC   SRTACLT,PPRDKCLT                                                 
PRPRD20  MVC   SRTPRD,PPRDKPRD     MOVE IN PRODUCT CODE                         
         MVC   SRTPNAM,PPRDNAME    MOVE IN PRODUCT NAME                         
         MVC   SRTSYS,CLIID        MOVE SYSTEM ID TO SORT FIELD                 
         MVC   SRTAGNID,CLIAGNID   MOVE AGENCY ID TO SORT FIELD                 
         MVC   SAVEID,CLIAGY       MOVE IN AGENCY TO SAVED AREA                 
         BAS   RE,SRTRTE           SORT ROUTINE                                 
*                                                                               
PRPRDX   B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* FIND ID AND SEND RECORDS TO SORT                                    *         
***********************************************************************         
         SPACE 1                                                                
SRTRTE   NTR1                                                                   
         LA    R2,SRTBUFF          R2 = A(SORT BUFFER)                          
         USING SORTD,R2                                                         
         LA    R1,IDTAB            R1 = A(2 AGENCY CODE/NAME TABLE)             
SRTRTE10 CLI   0(R1),X'FF'         CHECK FOR END OF TABLE                       
         BNE   *+6                                                              
         DC    H'0'                CODE MUST BE IN TABLE                        
         CLC   SAVEID,0(R1)        CHECK FOR CODE MATCH                         
         BE    *+12                  EXIT IF FOUND                              
         LA    R1,L'IDTAB(R1)      BUMP TO NEXT ENTRY                           
         B     SRTRTE10                                                         
*                                                                               
         MVC   SRTAGY,SAVEID       MOVE CODE TO SORT FIELD                      
         MVC   SRTANAM,2(R1)       MOVE IN AGENCY NAME TO SORT FIELD            
*                                                                               
SRTRTEX  DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'PUT',SRTBUFF                                    
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO READ FROM 2ND BINADD ROUTINE                     *          
**********************************************************************          
         SPACE 1                                                                
SYSRTE   NTR1                                                                   
         L     R5,AAGNTAB                                                       
         USING BIND,R5                                                          
         SR    R0,R0                                                            
         ICM   R0,15,BININ         R0=NUMBER IN TABLE                           
         BZ    SYSRTEX             NOTHING TO PROCESS                           
         LA    R6,BINTAB                                                        
         USING AGND,R6                                                          
SYSRTE10 TM    AGNSYS,AGNRAN       WAS THIS ENTRY ALREADY RUN?                  
         BO    SYSRTE30              YES - SKIP!                                
         CLI   AGNSYS,AGNPRT       TEST SYSTEM ID                               
         BE    SYSRTE20            IF NOT SET - PRINT                           
         CLI   AGNSYS,AGNSPT       SPOT?                                        
         BNE   SYSRTE30            ACCOUNTING JUST SKIP                         
         BAS   RE,SPAGN            PROCESS AGENCY  RECORD (SPOT/NET)            
         B     SYSRTE30                                                         
*                                                                               
SYSRTE20 BAS   RE,PRAGN            PROCESS AGENCY  RECORD (PRINT)               
*                                                                               
SYSRTE30 LA    R6,AGNLNQ(R6)       BUMP TO NEXT ENTRY                           
         BCT   R0,SYSRTE10                                                      
*                                                                               
SYSRTEX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS AGENCY ROUTINE  (SPOT/NET)                                  *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
SPAGN    NTR1                                                                   
         LA    R4,DKEY                                                          
         USING SPCLID,R4                                                        
         XC    DKEY,DKEY                                                        
         XC    SAVEID,SAVEID                                                    
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,AGNMED       AGY/MED                                      
         MVC   CKEYCLT,AGNPCLI     CLIENT CODE                                  
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),AGNNTSE     MOVE NET  SE# INTO UTL+4                     
         CLI   AGNID,C'S'          CHECK IF CURRENT ENTRY IS SPOT               
         BNE   *+10                  IF NOT SKIP NEXT MVC                       
         MVC   4(1,R1),AGNSPSE     MOVE SPOT SE# INTO UTL+4                     
*                                                                               
         BAS   RE,DMHISDIR                                                      
         CLC   DIR(CKEYZRO-CLTHDR),DKEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETSPT                                                      
         L     R4,AIO1             R4 = A(IO1)                                  
*                                                                               
         MVC   AGNCNAM,CNAME       MOVE CLIENT NAME TO STORAGE                  
         OC    CZENCLT,CZENCLT     IS THERE A ZENITH CODE?                      
         BZ    SPAGNX                                                           
         MVC   AGNZCLI,CZENCLT     MOVE IN ZENITH CODE                          
         MVC   AGNSCLI,AGNZCLI     MOVE IN ZENITH CODE TO SORT CLI              
*                                                                               
SPAGNX   OI    AGNSYS,AGNRAN       SET BIT TO SHOW WAS RUN                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS AGENCY ROUTINE  (PRINT)                                     *         
*         R6=CURRENT CLIENT PRODUCT ENTRY                             *         
***********************************************************************         
         SPACE 1                                                                
PRAGN    NTR1                                                                   
         LA    R4,DKEY             R4 = A(KEY)                                  
         USING PRCLID,R4                                                        
         XC    DKEY,DKEY           CLEAR KEY                                    
         MVC   PCLTKAGY,AGNAGY     MOVE AGENCY CODE INTO THE KEY                
         MVC   PCLTKMED,AGNMED     MOVE MEDIA  CODE INTO THE KEY                
         MVI   PCLTKRCD,X'02'      MOVE RECORD CODE INTO THE KEY                
         MVC   PCLTKCLT,AGNACLI    MOVE CLIENT CODE INTO THE KEY                
         L     R1,AUTL             R1 = A(UTL)                                  
         MVC   4(1,R1),AGNPTSE     MOVE SE# INTO UTL+4                          
         BAS   RE,DMHIPDIR         READ HIGH FROM PRINT DIR                     
         CLC   DIR(7),DKEY         COMPARE KEY WITH ORIGINAL                    
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
*                                                                               
         BAS   RE,DMGETPRT         GET RECORD FROM PRINT FILE                   
         L     R4,AIO1                                                          
         MVC   AGNCNAM,PCLTNAME    MOVE IN CLIENT NAME                          
         LA    R4,33(R4)           BUMP TO FIRST ELEMENT                        
         USING PCLTZEL,R4                                                       
PRAGN10  CLI   0(R4),X'00'         X'00' NO MORE ELEMENTS                       
         BE    PRAGNX                                                           
         CLI   0(R4),X'32'         X'32' ZENITH ELEMENT                         
         BNE   PRAGN20                                                          
         MVC   AGNZCLI,PCLTZEN     MOVE IN ZENITH CODE                          
         MVC   AGNSCLI,AGNZCLI     MOVE IN ZENITH CODE TO SORT CLI              
         B     PRAGNX                                                           
PRAGN20  SR    R1,R1                                                            
         IC    R1,1(R4)            INSERT ELEMENT LENGTH INTO R1                
         AR    R4,R1               BUMP TO NEXT ELEMENT                         
         B     PRAGN10                                                          
*                                                                               
PRAGNX   OI    AGNSYS,AGNRAN       SET BIT TO SHOW WAS RUN                      
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET RECORDS FROM SORT/FIND DUPS AND CREATE SJ ACCOUNTS              *         
***********************************************************************         
         SPACE 1                                                                
GETSRT   NTR1                                                                   
         LA    R2,SRTBUFF          R5 = A(SORT BUFFER)                          
         USING SORTD,R2                                                         
         LA    R4,XP               R4 = A(PRINT LINE)                           
         USING PLINED,R4                                                        
         MVC   PRTLNE(PRLNQ),XSPACES CLEAR PRINT LINE WITH SPACES               
         XC    LSTCPA,LSTCPA         CLEAR LAST CLIENT/PROD/AGENCY AREA         
         XC    LSTAGID,LSTAGID       ONE BYTE AGENCY ID                         
*                                                                               
GETSRT10 DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R5,15,4(R1)         R5 = A(SORTED RECORD)                        
         BZ    GETSRTX               IF ZERO, NO MORE RECORDS                   
         XC    SRTBUFF,SRTBUFF     CLEAR SORT BUFFER                            
         MVC   SRTBUFF,0(R5)       MOVE SORTED RECORD INTO BUFFER               
         TM    OPTN,OPTCLT         TEST IF CLIENT LEVEL RUN ONLY                
         BNO   GETSRT15              IF ON SKIP PRODUCT COMPARE                 
         OC    SRTPRD,SRTPRD       CHECK IF PRODUCT CODE IS EMPTY               
         BNZ   GETSRT30              IF YES - SAME CLI/DIFF PRD - SKIP          
         CLC   LSTCLI,SRTCLT       CHECK IF CLIENT  IS SAME AS BEFORE           
         BNE   GETSRT20              IF NOT, CREATE SJ RECORD                   
         CLC   LSTAGY,SRTAGY       CHECK IF AGENCY IS SAME AS BEFORE            
         BE    GETSRT30              IF EQUAL DISCARD RECORD                    
         CLC   LSTAGID,SRTAGNID    CHECK IF AGENCY ID IS SAME AS BEFORE         
         BE    GETSRT30              IF EQUAL CREATE SJ RECORD                  
         MVC   PDPL,=C'***DUP***'                                               
         BAS   RE,PRTDUP             ELSE - PRINT DUPLICATE ROUTINE             
         B     GETSRT30                                                         
*                                                                               
GETSRT15 CLC   LSTCLI,SRTCLT       CHECK IF CLIENT  IS SAME AS BEFORE           
         BNE   GETSRT20              IF NOT, CREATE SJ RECORD                   
         CLC   LSTPRD,SRTPRD       CHECK IF PRODUCT IS SAME AS BEFORE           
         BNE   GETSRT20              IF NOT, CREATE SJ RECORD                   
         CLC   LSTAGY,SRTAGY       CHECK IF AGENCY IS SAME AS BEFORE            
         BE    GETSRT30              IF EQUAL DISCARD RECORD                    
         CLC   LSTAGID,SRTAGNID    CHECK IF AGENCY ID IS SAME AS BEFORE         
         BE    GETSRT30              IF EQUAL DISCARD RECORD                    
         MVC   PDPL,=C'***DUP***'                                               
         BAS   RE,PRTDUP             ELSE - PRINT DUPLICATE ROUTINE             
         B     GETSRT30                                                         
GETSRT20 DS    0H                                                               
         BAS   RE,BLDREC           BUILD SJ RECORD                              
         BAS   RE,PRTDUP                                                        
*                                                                               
GETSRT30 MVC   LSTCPA,0(R2)        SAVE CLIENT/PROD/AGENCY FOR COMPARE          
         MVC   LSTAGID,SRTAGNID    UPDATE AGENCY ID                             
         B     GETSRT10                                                         
*                                                                               
GETSRTX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT LIST OF RECORDS (CREATED/DUPLICATES)                          *         
*       R2 = A(SORT BUFFER)                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R2                                                         
PRTDUP   NTR1                                                                   
         XC    SYSNAME,SYSNAME     CLEAR SAVED AREA FOR SYSTEM NAME             
         LA    R4,XP               R4 = A(PRINT LINE)                           
         USING PLINED,R4                                                        
*                                                                               
PRTDUP10 DS    0H                                                               
         CLC   SRTACLT,SPACES      CHECK IF OVERRIDE EXISTS                     
         BH    PRTDUP15                                                         
         MVC   PCLT,SRTCLT         MOVE CLIENT  CODE TO PRINT LINE              
         B     PRTDUP20                                                         
PRTDUP15 MVC   PZENCLT,SRTCLT                                                   
         MVC   PCLT,SRTACLT        MOVE CLIENT  CODE TO PRINT LINE              
         MVC   PZENPRD,SRTPRD                                                   
PRTDUP20 MVC   PCLTNAM,SRTCNAM     MOVE CLIENT  NAME TO PRINT LINE              
         MVC   PPRD,SRTPRD         MOVE PRODUCT CODE TO PRINT LINE              
         MVC   PRECV,SRTSRRC       MOVE RECEIVABLE KEY TO PRINT LINE            
         MVC   PCOST,SRT1CRC       MOVE COSTING KEY TO PRINT LINE               
         OC    SRTPRD,SRTPRD       CHECK IF AT PRODUCT LEVEL                    
         BZ    *+10                  IF NOT SKIP NEXT MVC                       
         MVC   PPRDNAM,SRTPNAM     MOVE PRODUCT NAME TO PRINT LINE              
         MVC   PAGY,SRTAGY         MOVE AGENCY  CODE TO PRINT LINE              
         MVC   PAGYNAM,SRTANAM     MOVE AGENCY  NAME TO PRINT LINE              
         CLI   SRTSYS,C'A'         COMPARE IF SYSTEM ID IS 'N'                  
         BNE   *+20                  IF NOT SKIP MVC COMMAND                    
         MVC   PSYSNAM,=C'ACC'       MOVE IN 'ACC'   TO PRINT LINE              
         MVC   SYSNAME,=C'ACCOUNT'   MOVE IN 'ACC'   TO SAVED AREA              
         B     PRTDUPX                                                          
         CLI   SRTSYS,C'N'         COMPARE IF SYSTEM ID IS 'N'                  
         BNE   *+20                  IF NOT SKIP MVC COMMAND                    
         MVC   PSYSNAM,=C'NET'       MOVE IN 'NET'   TO PRINT LINE              
         MVC   SYSNAME,=C'NETWORK'   MOVE IN 'NET'   TO SAVED AREA              
         B     PRTDUPX                                                          
         CLI   SRTSYS,C'S'         COMPARE IF SYSTEM ID IS 'S'                  
         BNE   *+20                  IF NOT SKIP MVC COMMAND                    
         MVC   PSYSNAM,=C'SPT'       MOVE IN 'SPOT'  TO PRINT LINE              
         MVC   SYSNAME(4),=C'SPOT'   MOVE IN 'SPOT'  TO SAVED AREA              
         B     PRTDUPX                                                          
         CLI   SRTSYS,C'P'         COMPARE IF SYSTEM ID IS 'P'                  
         BNE   PRTDUPX               IF NOT SKIP MVC COMMAND                    
         MVC   PSYSNAM,=C'PRT'       MOVE IN 'PRINT' TO PRINT LINE              
         MVC   SYSNAME(5),=C'PRINT'  MOVE IN 'PRINT' TO SAVED AREA              
*                                                                               
PRTDUPX  DS    0H                                                               
         GOTO1 ACREPORT            PRINT LINE                                   
         CLC   LINE,MAXLINES       ARE WE AT END OF PAGE?                       
         BNL   EXIT                                                             
         GOTO1 ACREPORT            SKIP LINE                                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD NEW ACCOUNT RECORD                                            *         
*       R2 = A(SORT BUFFER)                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SORTD,R2                                                         
BLDREC   NTR1                                                                   
         XC    NAME,NAME                                                        
         L     R3,AIO1             R3 = A(IO2)                                  
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVI   ACTKCPY,X'CE'       COMPANY CODE CE - ZENITH                     
         MVC   ACTKULA(2),=C'SJ'   UL - SJ                                      
         OC    SRTPRD,SRTPRD       CHECK IF PRODUCT FIELD IS EMPTY              
         BZ    BLDREC10                                                         
         MVC   ACTKACT(L'SRTCLT+L'SRTPRD),0(R2)   MOVE IN CLIENT/PROD           
         MVC   NAME(L'SRTPNAM),SRTPNAM      MOVE PROD NAME INTO NAME            
         B     BLDREC20                                                         
*                                                                               
BLDREC10 MVC   ACTKACT(L'SRTCLT),SRTCLT     MOVE CLIENT CODE INTO KEY           
         MVC   NAME(L'SRTCNAM),SRTCNAM      MOVE CLIENT NAME INTO NAME          
*                                                                               
BLDREC20 MVC   ACTRLEN,=Y(ACTRFST-ACTRECD)                                      
         XC    ACTRSTA,ACTRSTA                                                  
         XC    ACTRFST(2),ACTRFST                                               
         BAS   RE,ELEM20           NAME ELEMENT                                 
         BAS   RE,ELEM24           PRODUCTION ELEMENTS                          
         BAS   RE,ELEM30           STATUS ELEMENT                               
         BAS   RE,ADDR             ADD IT                                       
         AP    PKSJCNT,=P'1'       INCREMENT COUNTER                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD NAME ELEMENT                                                    *         
***********************************************************************         
         SPACE 1                                                                
ELEM20   NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         USING NAMELD,R4                                                        
         LA    R4,ELEMENT          BUILD NAME ELEMENT                           
         MVI   NAMEL,NAMELQ        X'20' - NAME ELEMENT                         
         LA    R1,L'NAME           R1 = LENGTH OF FIELD                         
         LA    R5,NAME+L'NAME-1    R5 = A(LAST POSITION OF NAME FIELD)          
         CLI   0(R5),X'00'         COMPARE IF NULLS                             
         BNE   *+10                                                             
         BCTR  R5,0                DECREMENT R5                                 
         BCT   R1,*-10                                                          
         CLI   0(R5),X'40'         COMPARE IF SPACES                            
         BNE   *+10                                                             
         BCTR  R5,0                DECREMENT R5                                 
         BCT   R1,*-10                                                          
         BCTR  R1,0                DECREMENT R1 FOR EX                          
         EX    R1,*+4                                                           
         MVC   NAMEREC(0),NAME                                                  
         AH    R1,=H'3'            ADD 3 FOR DECREM,TYPE,AND LEN BYTES          
         STC   R1,NAMLN            PUT LENGTH OF NAME INTO ELEMENT              
         BAS   RE,ADDL                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD PRODUCTION ELEMENT                                              *         
***********************************************************************         
         SPACE 1                                                                
ELEM24   NTR1                                                                   
         LA    R5,SJWRK            R5 = A(BIN TABLE WORK AREA)                  
         USING SJRECD,R5                                                        
         XC    SJWRK,SJWRK         CLEAR WORK AREA                              
         LA    R4,ELEMENT           R4 = A(ELEMENT)                             
         USING PPRELD,R4                                                        
         MVC   ELEMENT,SPACES                                                   
         MVI   PPREL,PPRELQ         X'24' - PRODUCTION PROFILE ELEMENT          
         MVI   PPRLN,PPRLN1Q          LENGTH                                    
         MVI   PPRRECVC,X'CE'         COMPANY CODE                              
         MVC   PPRRECVU(2),=C'SR'     UNIT/LEDGER 'SR'                          
         MVI   PPRRECVA,C'A'          RECEIVABLES TYPE 'A'                      
         CLI   SRTAGNID,C'B'          CHECK IF BATES/BACKER                     
         BNE   *+10                                                             
         MVC   PPRRECVA+1(2),=C'BS'   BA - BATES/BACKER                         
         CLI   SRTAGNID,C'D'          CHECK IF SAATCHI & SAATCHI                
         BNE   *+10                                                             
         MVC   PPRRECVA+1(2),=C'SA'   SA - SAATCHI & SAATCHI                    
         MVC   PPRRECVA+3(6),=C'000000'   PAD WITH ZEROES                       
         MVC   SRTSRRC,PPRRECVA       MOVE RECEIVABLE ACCOUNT TO SORT           
         MVC   0(SJKYLNQ,R5),PPRRECV  MOVE SJ ACCOUNT TO WORK AREA              
         MVC   SJANAM,SRTANAM         AGENCY NAME                               
         GOTO1 BINADD,DMCB,SJWRK,ASJTAB                                         
*                                                                               
         XC    SJWRK,SJWRK          CLEAR WORK AREA                             
         MVI   PPRCOSTC,X'CE'       COMPANY CODE                                
         MVC   PPRCOSTU(2),=C'1C'   UNIT/LEDGER '1C'                            
         CLI   SRTAGNID,C'B'        CHECK IF BATES/BACKER                       
         BNE   *+8                                                              
         MVI   PPRCOSTA,C'B'        BATES/BACKER TYPE 'B'                       
         CLI   SRTAGNID,C'D'        CHECK IF SAATCHI & SAATCHI                  
         BNE   *+8                                                              
         MVI   PPRCOSTA,C'S'        SAATCHI & SAATCHI TYPE 'S'                  
         MVC   PPRCOSTA+1(2),=C'01' '01' FOR ALL CLIENTS                        
         MVC   PPRCOSTA+3(3),SRTCLT    MOVE IN CLIENT CODE                      
         MVC   PPRCOSTA+6(3),SRTPRD    MOVE IN PRODUCT CODE - PRD LEVEL         
         MVC   0(SJKYLNQ,R5),PPRCOST   MOVE SJ ACCOUNT TO WORK AREA             
         OC    SRTPRD,SRTPRD                                                    
         BNZ   *+10                                                             
         MVC   PPRCOSTA+6(3),=C'000'   PAD WITH ZEROES                          
         MVC   SRT1CRC,PPRCOSTA        MOVE COSTING ACCOUNT KEY TO SORT         
*                                                                               
         MVC   PPRGAOFF,=C'01'         OFFICE 01                                
         MVI   PPRBTYPE,PPRBCLIT       BILLING TYPE - CLIENT                    
         XC    PPRBLAMT,PPRBLAMT       CLEAR AMOUNT FOR NOW                     
         MVC   SJCPNAM,SRTCNAM         CLIENT NAME                              
         OC    SRTPRD,SRTPRD                                                    
         BZ    *+10                                                             
         MVC   SJCPNAM,SRTPNAM         IF AT PRODUCT LEVEL - PROD NAME          
         GOTO1 BINADD,DMCB,SJWRK,ASJTAB                                         
         BAS   RE,ADDL                                                          
*                                                                               
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD RECORD STATUS ELEMENT                                           *         
***********************************************************************         
         SPACE 1                                                                
ELEM30   NTR1                                                                   
         LA    R4,ELEMENT                                                       
         USING RSTELD,R4           ACCOUNT STATUS                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   RSTEL,RSTELQ        X'30' - ACCOUNT STATUS ELEMENT               
         MVI   RSTLN,RSTLN3Q                                                    
         MVC   RSTBDATE,TODAYP                                                  
         MVC   RSTTDATE,TODAYP                                                  
         MVC   RSTCOSTG,SPACES                                                  
         MVI   RSTFILT1,C' '       CLEAR FILTER 1                               
         MVI   RSTFILT2,C' '       CLEAR FILTER 2                               
         MVI   RSTFILT3,C' '       CLEAR FILTER 3                               
         MVI   RSTFILT4,C' '       CLEAR FILTER 4                               
         MVI   RSTFILT5,C' '       CLEAR FILTER 5                               
*                                                                               
         MVI   ACTRSAF1,C' '       CLEAR FILTER 1                               
         MVI   ACTRSAF2,C' '       CLEAR FILTER 2                               
         MVI   ACTRSAF3,C' '       CLEAR FILTER 3                               
         MVI   ACTRSAF4,C' '       CLEAR FILTER 4                               
         MVI   ACTRSAF5,C' '       CLEAR FILTER 5                               
         BAS   RE,ADDL                                                          
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ACCOUNT BALANCE ELEMENT                                             *         
***********************************************************************         
         SPACE 1                                                                
ELEM32   NTR1                                                                   
         LA    R4,ELEMENT                                                       
         USING ABLELD,R4           ACCOUNT STATUS                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   ABLEL,ABLELQ        X'32' - ACCOUNT BALANCE ELEMENT              
         MVI   ABLLN,ABLLN2Q       LENGTH                                       
         ZAP   ABLFRWD,=P'0'       INITIALIZE BALANCE BROUGHT FORWARD           
         ZAP   ABLDR,=P'0'         INITIALIZE DEBITS SINCE BAL FORWARD          
         ZAP   ABLCR,=P'0'         INITIALIZE CREDITS SINCE BAL FORWARD         
         ZAP   ABLURG,=P'0'        INITIALIZE URGENT CREDITS                    
*                                                                               
         BAS   RE,ADDL                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD RECORD TO THE FILE                                              *         
***********************************************************************         
         SPACE 1                                                                
ADDR     NTR1                                                                   
         L     R2,AIO1                                                          
         USING ACTRECD,R2                                                       
         SR    R5,R5                                                            
         ICM   R5,3,ACTRLEN        SET LENGTH FOR TAPE                          
         LA    R5,4(R5)            INCREASE LENGTH BY 4                         
         STH   R5,RLN1             SET LENGTH                                   
         LA    R5,RLN1                                                          
*                                                                               
         TM    OPTN,OPTDUMP        DUMP OUTPUT                                  
         BNO   ADDR10                                                           
         MVC   MSG,=CL10'ACCT  REC'                                             
         SR    R4,R4                                                            
         LH    R4,RLN1                                                          
         GOTO1 DUMP,DMCB,(RC),RLN1,(R4)                                         
*                                                                               
ADDR10   CLI   QOPT5,C'T'          TEST TAPE OUTPUT                             
         BNE   ADDRX                                                            
         L     R3,AOUTFIL                                                       
         PUT   (R3),(R5)                                                        
ADDRX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT                                                         *         
*     R4 = A(ELEMENT)                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDL     NTR1                                                                   
         L     R2,AIO1                                                          
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R4)                               
         LR    RE,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT LIST OF RECORDS (NUMBER OF OCCURENCES)                        *         
***********************************************************************         
         SPACE 1                                                                
PRTRTE   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,AAGNTAB          R5 = A(TABLE)                                
         SR    R0,R0                                                            
         ICM   R0,15,BININ                                                      
         BZ    PRTRTEX                                                          
         USING AGND,R6                                                          
         LA    R6,BINTAB                                                        
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         MVC   PRTLNE(PRLNQ2),XSPACES CLEAR PRINT LINE WITH SPACES              
         XC    LSTCPA,LSTCPA         CLEAR LAST CLIENT/PROD/AGENCY AREA         
         XC    DUPFLG,DUPFLG         CLEAR DUPLICATE FLAG                       
         ZAP   PKDPCNT,=P'0'         INITIALIZE DUP COUNTER                     
*                                                                               
PRTRTE10 DS    0H                                                               
         OC    LSTCLI,LSTCLI       TEST IF 1ST RUN THROUGH OR NOT               
         BZ    PRTRTE20              IF IT IS - SKIP NEXT COMPARE               
         CLC   LSTCLI,AGNACLI      COMPARE IF CLIENT IS SAME AS LAST            
         BNE   PRTRTE90                                                         
         CLC   LSTPRD,AGNZCLI      COMPARE IF CLIENT IS SAME AS LAST            
         BNE   PRTRTE90                                                         
*                                                                               
PRTRTE20 MVC   PACLT,AGNACLI       MOVE IN AGENCY CLIENT CODE                   
         MVC   PZCLT,AGNZCLI       MOVE IN ZENITH CLIENT CODE                   
         MVC   PACTNAM,AGNCNAM     MOVE IN AGENCY CLIENT NAME                   
         CLC   AGNAGY,=C'BS'                                                    
         BNE   PRTRTE25                                                         
         MVI   BYTE,X'01'          BATES/SAATCHI CONVERSION                     
         EDIT  (P8,AGNBKT),(10,PBSCNT)                                          
         TM    DUPFLG,DUPFLGB      TEST IF BATES FLAG ALREADY SET               
         BO    PRTRTE80              IF IT IS - SKIP                            
         AP    PKDPCNT,=P'1'         ELSE ADD 1 TO COUNT                        
         OI    DUPFLG,DUPFLGB        TURN ON BATE'S FLAG                        
         B     PRTRTE80                                                         
PRTRTE25 CLC   AGNAGY,=C'BT'                                                    
         BNE   PRTRTE30                                                         
         MVI   BYTE,X'01'          BATES/SAATCHI CONVERSION                     
         EDIT  (P8,AGNBKT),(10,PBTCNT)                                          
         TM    DUPFLG,DUPFLGB      TEST IF BATE'S FLAG ALREADY SET              
         BO    PRTRTE80              IF IT IS - SKIP                            
         AP    PKDPCNT,=P'1'         ELSE ADD 1 TO COUNT                        
         OI    DUPFLG,DUPFLGB        TURN ON BATE'S FLAG                        
         B     PRTRTE80                                                         
PRTRTE30 CLC   AGNAGY,=C'DW'                                                    
         BNE   PRTRTE35                                                         
         MVI   BYTE,X'01'          BATES/SAATCHI CONVERSION                     
         EDIT  (P8,AGNBKT),(10,PDWCNT)                                          
         TM    DUPFLG,DUPFLGD      TEST IF SAATCHI'S FLAG ALREADY SET           
         BO    PRTRTE80              IF IT IS - SKIP                            
         AP    PKDPCNT,=P'1'         ELSE ADD 1 TO COUNT                        
         OI    DUPFLG,DUPFLGD        TURN ON SAATCHI'S FLAG                     
         B     PRTRTE80                                                         
PRTRTE35 CLC   AGNAGY,=C'DF'                                                    
         BNE   PRTRTE40                                                         
         MVI   BYTE,X'01'          BATES/SAATCHI CONVERSION                     
         EDIT  (P8,AGNBKT),(10,PDFCNT)                                          
         TM    DUPFLG,DUPFLGD      TEST IF SAATCHI'S FLAG ALREADY SET           
         BO    PRTRTE80              IF IT IS - SKIP                            
         AP    PKDPCNT,=P'1'         ELSE ADD 1 TO COUNT                        
         OI    DUPFLG,DUPFLGD        TURN ON SAATCHI'S FLAG                     
         B     PRTRTE80                                                         
PRTRTE40 CLC   AGNAGY,=C'DT'                                                    
         BNE   PRTRTE45                                                         
         MVI   BYTE,X'01'          BATES/SAATCHI CONVERSION                     
         EDIT  (P8,AGNBKT),(10,PDTCNT)                                          
         TM    DUPFLG,DUPFLGD      TEST IF SAATCHI'S FLAG ALREADY SET           
         BO    PRTRTE80              IF IT IS - SKIP                            
         AP    PKDPCNT,=P'1'         ELSE ADD 1 TO COUNT                        
         OI    DUPFLG,DUPFLGD        TURN ON SAATCHI'S FLAG                     
         B     PRTRTE80                                                         
PRTRTE45 CLC   AGNAGY,=C'SF'                                                    
         BNE   PRTRTE50                                                         
         MVI   BYTE,X'01'          BATES/SAATCHI CONVERSION                     
         EDIT  (P8,AGNBKT),(10,PSFCNT)                                          
         TM    DUPFLG,DUPFLGD      TEST IF SAATCHI'S FLAG ALREADY SET           
         BO    PRTRTE80              IF IT IS - SKIP                            
         AP    PKDPCNT,=P'1'         ELSE ADD 1 TO COUNT                        
         OI    DUPFLG,DUPFLGD        TURN ON SAATCHI'S FLAG                     
*                                                                               
PRTRTE50 CLC   AGNAGY,=C'BD'                                                    
         BNE   PRTRTE55                                                         
         MVI   BYTE,X'02'          BD/NE CONVERSION                             
         EDIT  (P8,AGNBKT),(10,PBDCNT)                                          
         TM    DUPFLG,DUPFLGO      TEST IF BD/NE'S FLAG ALREADY SET             
         BO    PRTRTE80              IF IT IS - SKIP                            
         AP    PKDPCNT,=P'1'         ELSE ADD 1 TO COUNT                        
         OI    DUPFLG,DUPFLGO        TURN ON BD/NE'S FLAG                       
PRTRTE55 CLC   AGNAGY,=C'NE'                                                    
         BNE   PRTRTE80                                                         
         MVI   BYTE,X'02'          BD/NE CONVERSION                             
         EDIT  (P8,AGNBKT),(10,PBTCNT)                                          
         TM    DUPFLG,DUPFLGO      TEST IF BD/NE'S FLAG ALREADY SET             
         BO    PRTRTE80              IF IT IS - SKIP                            
         AP    PKDPCNT,=P'1'         ELSE ADD 1 TO COUNT                        
         OI    DUPFLG,DUPFLGD        TURN ON BD/NE'S FLAG                       
*                                                                               
PRTRTE80 DS    0H                                                               
         MVC   LSTCPA,0(R6)        UPDATE LAST CLIENT                           
         LA    R6,AGNLNQ(R6)                                                    
         BCT   R0,PRTRTE10                                                      
         B     PRTRTEX                                                          
*                                                                               
PRTRTE90 DS    0H                                                               
         MVC   LSTCPA,0(R6)        UPDATE LAST CLIENT                           
         CP    PKDPCNT,=P'2'                                                    
         BL    PRTRTE95                                                         
         MVC   PDPL2,=C'***DUP***'                                              
         BAS   RE,PRCDUP           PROCESS DUPLICATES ROUTINE                   
         ZAP   PKDPCNT,=P'0'       RE-INITIALIZE COUNT                          
         XC    DUPFLG,DUPFLG       CLEAR DUPLICATE FLAG                         
         B     PRTRTE20                                                         
*                                                                               
PRTRTE95 DS    0H                                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
PRTRTE99 ZAP   PKDPCNT,=P'0'       RE-INITIALIZE COUNT                          
         XC    DUPFLG,DUPFLG       CLEAR DUPLICATE FLAG                         
         B     PRTRTE20                                                         
*                                                                               
PRTRTEX  DS    0H                                                               
         GOTO1 ACREPORT                                                         
         CLC   LINE,MAXLINES       ARE WE AT END OF PAGE?                       
         BNL   EXIT                                                             
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO PROCESS DUPLICATES                               *          
*                R6 = CURRENT POSITION IN BINTABLE                   *          
**********************************************************************          
         SPACE 1                                                                
PRCDUP   NTR1                                                                   
         LA    R3,AGNLNQ                                                        
         SR    R6,R3                BUMP UP TO PRIOR ENTRY                      
         MVC   AGNDUP,=C'***DUP***' MARK AGENCY AS DUPLICATE                    
         MVC   SVAGZN,0(R6)         SAVE AGY/ZEN CODES FOR COMPARE              
*                                                                               
         CLC   AGNAGY,=C'BD'        CHECK IF AGENCY IS BD                       
         BNE   PRCDUP03               IF NOT SKIP TO NEXT COMPARE               
         MVC   PNECNT(58),XSPACES     CLEAR ALL OTHER PRINT FIELDS              
         GOTO1 ACREPORT             PRINT OUT LINE                              
PRCDUP03 CLC   AGNAGY,=C'NE'        CHECK IF AGENCY IS NE                       
         BNE   PRCDUP05               IF NOT SKIP TO NEXT COMPARE               
         MVC   PBDCNT,XSPACES       CLEAR BD FIELD                              
         GOTO1 ACREPORT             PRINT OUT LINE                              
PRCDUP05 CLC   AGNAGY,=C'BS'        CHECK IF AGENCY IS BS                       
         BNE   PRCDUP10               IF NOT SKIP TO NEXT COMPARE               
         MVC   PBTCNT(58),XSPACES     CLEAR ALL OTHER PRINT FIELDS              
         GOTO1 ACREPORT             PRINT OUT LINE                              
PRCDUP10 CLC   AGNAGY,=C'BT'        CHECK IF AGENCY IS BT                       
         BNE   PRCDUP20               IF NOT SKIP TO NEXT COMPARE               
         MVC   PBSCNT,XSPACES       CLEAR BS FIELD                              
         MVC   PDWCNT(46),XSPACES   CLEAR DW/DF/DT/SF PRINT FIELDS              
         GOTO1 ACREPORT             PRINT OUT LINE                              
PRCDUP20 CLC   AGNAGY,=C'DW'        CHECK IF AGENCY IS DW                       
         BNE   PRCDUP30               IF NOT SKIP TO NEXT COMPARE               
         MVC   PBSCNT(24),XSPACES   CLEAR BS/BT PRINT FIELDS                    
         MVC   PDFCNT(34),XSPACES   CLEAR DF/DT/SF PRINT FIELDS                 
         GOTO1 ACREPORT             PRINT OUT LINE                              
PRCDUP30 CLC   AGNAGY,=C'DF'        CHECK IF AGENCY IS DF                       
         BNE   PRCDUP40               IF NOT SKIP TO NEXT COMPARE               
         MVC   PBSCNT(36),XSPACES   CLEAR BS/BT/DW PRINT FIELDS                 
         MVC   PDTCNT(22),XSPACES   CLEAR DT/SF PRINT FIELDS                    
         GOTO1 ACREPORT             PRINT OUT LINE                              
PRCDUP40 CLC   AGNAGY,=C'DT'        CHECK IF AGENCY IS DT                       
         BNE   PRCDUP50               IF NOT SKIP TO NEXT COMPARE               
         MVC   PBSCNT(48),XSPACES   CLEAR BS/BT/DW/DF PRINT FIELDS              
         MVC   PSFCNT,XSPACES       CLEAR SF PRINT FIELD                        
         GOTO1 ACREPORT             PRINT OUT LINE                              
PRCDUP50 CLC   AGNAGY,=C'SF'        CHECK IF AGENCY IS SF                       
         BNE   PRCDUP60               IF NOT - EXIT                             
         MVC   PBSCNT(60),XSPACES   CLEAR ALL OTHER PRINT FIELD                 
         GOTO1 ACREPORT             PRINT OUT LINE                              
*                                                                               
PRCDUP60 DS    0H                                                               
         SR    R6,R3               BUMP UP TO THE PRIOR ENTRY IN TABLE          
         CLC   SVAGCLT,AGNACLI                                                  
         BNE   PRCDUPX                                                          
         CLC   SVZNCLT,AGNZCLI                                                  
         BNE   PRCDUPX                                                          
         MVC   AGNDUP,=C'***DUP***' MARK AS DUPLICATE                           
         MVC   PDPL2,AGNDUP                                                     
         MVC   PACLT,AGNACLI       MOVE IN AGENCY CLIENT CODE                   
         MVC   PZCLT,AGNZCLI       MOVE IN ZENITH CLIENT CODE                   
         MVC   PACTNAM,AGNCNAM     MOVE IN AGENCY CLIENT NAME                   
         CLC   AGNAGY,=C'BD'                                                    
         BNE   PRCDUP67                                                         
         EDIT  (P8,AGNBKT),(10,PBDCNT)                                          
PRCDUP67 CLC   AGNAGY,=C'NE'                                                    
         BNE   PRCDUP69                                                         
         EDIT  (P8,AGNBKT),(10,PNECNT)                                          
PRCDUP69 CLC   AGNAGY,=C'BS'                                                    
         BNE   PRCDUP70                                                         
         EDIT  (P8,AGNBKT),(10,PBSCNT)                                          
PRCDUP70 CLC   AGNAGY,=C'BT'                                                    
         BNE   PRCDUP75                                                         
         EDIT  (P8,AGNBKT),(10,PBTCNT)                                          
PRCDUP75 CLC   AGNAGY,=C'DW'                                                    
         BNE   PRCDUP80                                                         
         EDIT  (P8,AGNBKT),(10,PDWCNT)                                          
PRCDUP80 CLC   AGNAGY,=C'DF'                                                    
         BNE   PRCDUP85                                                         
         EDIT  (P8,AGNBKT),(10,PDFCNT)                                          
PRCDUP85 CLC   AGNAGY,=C'DT'                                                    
         BNE   PRCDUP90                                                         
         EDIT  (P8,AGNBKT),(10,PDTCNT)                                          
PRCDUP90 CLC   AGNAGY,=C'SF'                                                    
         BNE   PRCDUP95                                                         
         EDIT  (P8,AGNBKT),(10,PSFCNT)                                          
*                                                                               
PRCDUP95 DS    0H                                                               
         GOTO1 ACREPORT                                                         
PRCDUP99 MVC   SVAGZN,0(R6)        UPDATE CODES                                 
         B     PRCDUP60                                                         
*                                                                               
PRCDUPX  CLC   LINE,MAXLINES       ARE WE AT END OF PAGE?                       
         BNL   EXIT                                                             
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO READ FROM 2ND BINTABLE AND SORT ON AGENCY        *          
**********************************************************************          
         SPACE 1                                                                
XSRT     NTR1                                                                   
         L     R5,AAGNTAB                                                       
         USING BIND,R5                                                          
         SR    R0,R0                                                            
         ICM   R0,15,BININ         R0=NUMBER IN TABLE                           
         BZ    XSRTX               NOTHING TO PROCESS                           
         LA    R6,BINTAB                                                        
         USING AGND,R6                                                          
*                                                                               
         TM    FLAG,FLGCTF         IS THIS THE CTFILE RTE OR NOT?               
         BNZ   XSRT10                YES IT IS!!!                               
*                                                                               
         LA    R2,AGNLNQ           R2 = LENGTH OF RECORD                        
         LA    R4,L'AGNSYSID       R4 = LENGTH OF AGENCY ID(1 BYTE)             
         LA    R3,AGNXL1Q          R3 = DISPLACEMENT TO ID                      
         GOTO1 XSORT,DMCB,(0,(R6)),(R0),(R2),(R4),(R3)                          
         B     XSRT15                                                           
*                                                                               
XSRT10   LA    R2,AGNLNQ           R2 = LENGTH OF RECORD                        
         LA    R4,L'AGNSCLI        R4 = LENGTH OF SORT CLI(2 BYTE)              
         LA    R3,AGNXL2Q          R3 = DISPLACEMENT TO SORT CLI CODE           
         GOTO1 XSORT,DMCB,(0,(R6)),(R0),(R2),(R4),(R3)                          
         B     XSRTX                                                            
*                                                                               
         USING PLINED,R4                                                        
XSRT15   LA    R4,XP                                                            
         MVC   PRTLNE(PRLNQ2),XSPACES CLEAR PRINT LINE WITH SPACES              
         XC    LSTCPA,LSTCPA       CLEAR LAST CLIENT/PROD/AGENCY AREA           
         XC    SVSYSID,SVSYSID     CLEAR SYSTEM ID SAVED AREA                   
*                                                                               
*        FIND AGENCY NAME IN ID TABLE                                           
*                                                                               
XSRT20   LA    R1,IDTAB            R1 = A(AGENCY TABLE)                         
XSRT30   CLI   0(R1),X'FF'         COMPARE TO END OF TABLE MARKER               
         BNE   *+6                   IF NOT EQUAL CONTINUE                      
         DC    H'0'                  ELSE DIE - AGENCY MUST BE THERE            
         CLC   AGNAGY,0(R1)        COMPARE AGENCY TO TABLE                      
         BE    *+12                  IF EQUAL EXIT LOOP                         
         LA    R1,L'IDTAB(R1)      SKIP TO NEXT ENTRY                           
         B     XSRT30                                                           
         MVC   HEADNM,2(R1)        MOVE IN AGENCY NAME                          
         CLI   AGNSYSID,C'B'       IF BS/BT ADJUST NAME FIELD                   
         BNE   *+10                                                             
         MVC   HEADNM,=CL36'BATES USA/BACKER SPIELVOGEL'                        
         B     XSRT60              SKIP FIRST SET OF COMPARES                   
*                                                                               
*        PROCESS TABLE AND SEND TO PRINT                                        
*                                                                               
XSRT40   CLC   SVSYSID,AGNSYSID                                                 
         BE    XSRT50                                                           
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVC   PAGE,=H'1'                                                       
         B     XSRT20              FIND NEW AGENCY NAME                         
XSRT50   CLC   LSTCLI,AGNACLI      COMPARE IF CLIENT IS SAME AS LAST            
         BNE   XSRT95                                                           
         CLC   LSTAGY,AGNAGY       COMPARE IF AGENCY IS SAME AS LAST            
         BNE   XSRT99                                                           
XSRT60   MVC   P3ACLT,AGNACLI      MOVE CLIENT CODE TO PRINT LINE               
         MVC   P3ZCLT,AGNZCLI      MOVE ZENITH CODE TO PRINT LINE               
         MVC   P3ACTNAM,AGNCNAM    MOVE CLIENT NAME TO PRINT LINE               
         MVC   P3AGY,AGNAGY        MOVE AGENCY CODE TO PRINT LINE               
         MVC   P3DPL,AGNDUP        MOVE DUPLICATE MARKER TO PRINT LINE          
         LA    R1,AGNACMD          R1 = A(ACC MEDIA LIST - TABLE)               
         LA    R2,P3ACMD           R2 = A(ACC MEDIA LIST - PRINT LINE)          
         BAS   RE,MEDRT                                                         
         LA    R1,AGNSPMD          R1 = A(SPOT MEDIA LIST - TABLE)              
         LA    R2,P3SPMD           R2 = A(SPOT MEDIA LIST - PRINT LINE)         
         BAS   RE,MEDRT                                                         
         LA    R1,AGNNTMD          R1 = A(NET MEDIA LIST - TABLE)               
         LA    R2,P3NTMD           R2 = A(NET MEDIA LIST - PRINT LINE)          
         BAS   RE,MEDRT                                                         
         LA    R1,AGNPTMD          R1 = A(PRNT MEDIA LIST - TABLE)              
         LA    R2,P3PTMD           R2 = A(PRNT MEDIA LIST - PRINT LINE)         
         BAS   RE,MEDRT                                                         
*                                                                               
         DS    0H                                                               
         MVC   SVSYSID,AGNSYSID    UPDATE SYSTEM ID                             
         MVC   LSTCPA,0(6)         UPDATE LAST CLIENT/PROD/AGY                  
         LA    R6,AGNLNQ(R6)                                                    
         BCT   R0,XSRT40                                                        
         B     XSRTPX                                                           
*                                                                               
XSRT95   MVC   LSTCLI,0(R6)        UPDATE LAST CLIENT/PROD/AGY                  
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         B     XSRT60                                                           
*                                                                               
XSRT99   MVC   LSTCLI,0(R6)        UPDATE LAST CLIENT/PROD/AGY                  
         GOTO1 ACREPORT                                                         
         B     XSRT60                                                           
*                                                                               
XSRTPX   DS    0H                                                               
         GOTO1 ACREPORT                                                         
         CLC   LINE,MAXLINES       ARE WE AT END OF PAGE?                       
         BNL   XSRTX                                                            
         GOTO1 ACREPORT                                                         
XSRTX    B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO READ MEDIA LIST AND PUT TO PRINT                 *          
*                R1 = MEDIA LIST FROM BINTABLE                       *          
*                R2 = MEDIA LIST ON PRINT LINE                       *          
**********************************************************************          
         SPACE 1                                                                
MEDRT    NTR1                                                                   
         LA    R0,6                MAXIMUM LOOP IS 5 TIMES                      
MEDRT10  OC    0(2,R1),0(R1)       TEST IF END OF LIST                          
         BZ    MEDRTX                                                           
         MVC   0(2,R2),0(R1)       MOVE INTO PRINT LINE                         
         MVI   2(R2),C' '          MOVE IN A SPACE                              
         LA    R1,2(R1)            BUMP TO NEXT ENTRY IN MEDIA LIST             
         LA    R2,3(R2)            BUMP TO NEXT POSITION ON PRINT LINE          
         BCT   R0,MEDRT10                                                       
*                                                                               
MEDRTX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD POSTING ACCOUNT RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
BLDPST   NTR1                                                                   
         USING BIND,R2                                                          
         L     R2,ASJTAB           R2 = A(SJ TABLE)                             
         SR    R0,R0                                                            
         ICM   R0,15,BININ         R0 = NUMBER IN TABLE                         
         BZ    BLDPSTX             NOTHING TO PROCESS                           
         USING SJRECD,R6                                                        
         LA    R6,BINTAB           R6 = A(TABLE)                                
         USING ACTRECD,R3                                                       
         L     R3,AIO1                                                          
*                                                                               
BLDPST05 XC    NAME,NAME                                                        
         MVC   ACTKEY,SPACES                                                    
         CLC   1(2,R6),=C'SR'      TWO TABLE ENTRIES 'SR' & '1C'                
         BNE   BLDPST10                    MUST BE 1C                           
         MVC   ACTKCULA(SJKYLNQ),0(R6)     MOVE IN SR KEY                       
         MVC   NAME,SJANAM                 MOVE IN AGENCY NAME                  
         AP    PKSRCNT,=P'1'       INCREMENT SR COUNTER                         
         B     BLDPST20                                                         
*                                                                               
BLDPST10 DS    0H                                                               
         MVC   ACTKCULA(SJKYLNQ),0(R6)     MOVE IN 1C KEY                       
         AP    PK1CCNT,=P'1'               INCREMENT 1C COUNTER                 
         OC    SJPRD,SJPRD                                                      
         BNZ   *+20                                                             
         BAS   RE,BLDCLT                   BUILD CLIENT POSTING                 
         MVC   ACTKCULA+9(3),=C'000'       IF NO PROD - PAD W/ ZEROES           
         AP    PK1CCNT,=P'1'               ADD ANOTHER FOR CLIENT LEVEL         
         MVC   NAME(L'SJCPNAM),SJCPNAM     MOVE IN CLIENT OR PROD NAME          
*                                                                               
BLDPST20 MVC   ACTRLEN,=Y(ACTRFST-ACTRECD)                                      
         XC    ACTRSTA,ACTRSTA                                                  
         XC    ACTRFST(2),ACTRFST                                               
         BAS   RE,ELEM20           NAME ELEMENT                                 
         BAS   RE,ELEM30           STATUS ELEMENT                               
         BAS   RE,ELEM32           ACCOUNT BALANCE ELEMENT                      
         BAS   RE,ADDR             ADD IT                                       
         LA    R6,SJLNQ(R6)                                                     
         BCT   R0,BLDPST05                                                      
*                                                                               
BLDPSTX  B     EXIT                                                             
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD CLIENT POSTING ACCOUNT                                        *         
*  THIS ROUTINE BUILDS A POSTING ACCOUNT AT THE CLIENT LEVEL          *         
*       R3 = A(IO2) - BUILD RECORD                                    *         
*       R6 = A(TABLE) - CURRENT POSITION IN TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SJRECD,R6                                                        
         USING ACTRECD,R3                                                       
BLDCLT   NTR1                                                                   
         MVC   ACTKCULA+9(3),=C'   '       IF NO PROD - PAD W/ ZEROES           
         MVC   NAME(L'SJCPNAM),SJCPNAM     MOVE IN CLIENT OR PROD NAME          
*                                                                               
         MVC   ACTRLEN,=Y(ACTRFST-ACTRECD)                                      
         XC    ACTRSTA,ACTRSTA                                                  
         XC    ACTRFST(2),ACTRFST                                               
         BAS   RE,ELEM20           NAME ELEMENT                                 
         BAS   RE,ELEM30           STATUS ELEMENT                               
         BAS   RE,ADDR             ADD IT                                       
*                                                                               
BLDCLTX  B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT POSTING ACCOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
PRTPST   NTR1                                                                   
         XC    LSTUL,LSTUL                                                      
         USING BIND,R2                                                          
         L     R2,ASJTAB           R2 = A(SJ TABLE)                             
         USING PLINED,R4                                                        
         LA    R4,XP               R4 = A(PRINT LINE)                           
         USING SJRECD,R6                                                        
PRTPST10 LA    R6,BINTAB           R6 = A(TABLE)                                
         SR    R0,R0                                                            
         ICM   R0,15,BININ         R0 = NUMBER IN TABLE                         
         BZ    PRTPSTX             NOTHING TO PROCESS                           
*                                                                               
         OC    LSTUL,LSTUL                                                      
         BNZ   *+10                                                             
         MVC   LSTUL,1(R6)         MOVE IN UNIT/LEDGER                          
*                                                                               
         MVC   HEADNM,SPACES       CLEAR HEADER                                 
         CLC   LSTUL,=C'SR'        IS THIS RUN 'SR'                             
         BNE   *+14                  IF NOT - MUST BE '1C'                      
         MVC   HEADNM(16),=C'SR - RECEIVABLES'                                  
         B     *+10                                                             
         MVC   HEADNM(12),=C'1C - COSTING'                                      
*                                                                               
PRTPST20 CLC   LSTUL,1(R6)         TWO TABLE ENTRIES 'SR' & '1C'                
         BNE   PRTPST40                                                         
         MVC   P4UL,SJUL           UNIT/LEDGER                                  
         MVC   P4TYPE,SJTYPE       TYPE                                         
         MVC   P4OFF,SJOFF         OFFICE                                       
         CLC   LSTUL,=C'SR'        IF 'SR' NO CLI/PROD                          
         BNE   PRTPST30                                                         
         MVC   P4CLT,SJCLI         MOVE IN ZEROES                               
         MVC   P4PRD,SJPRD         MOVE IN ZEROES                               
         MVC   P4ANAM,SJANAM       AGENCY NAME                                  
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         B     PRTPST40            SKIP 1C CODE                                 
*                                                                               
PRTPST30 DS    0H                                                               
         MVC   P4CLT,SJCLI         CLIENT CODE                                  
         OC    SJPRD,SJPRD         IS THERE A PRODUCT CODE???                   
         BNZ   *+14                                                             
         MVC   P4PRD,=C'000'         IF NOT - MOVE IN ZEROES                    
         B     *+10                                                             
         MVC   P4PRD,SJPRD           ELSE - MOVE IN PROD CODE                   
         MVC   P4CPNAM,SJCPNAM     CLI OR PROD NAME (WHICH EVER LEVEL)          
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTPST40 DS    0H                                                               
         LA    R6,SJLNQ(R6)                                                     
         BCT   R0,PRTPST20                                                      
         CLC   LSTUL,=C'1C'        '1C' IS LAST UNIT/LEDGER                     
         BE    PRTPSTX                                                          
         MVC   LSTUL,=C'1C'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     PRTPST10                                                         
*                                                                               
PRTPSTX  B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD CTFILE RECORDS                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING ZENRECD,R2                                                       
         USING PLINED,R4                                                        
         USING BIND,R5                                                          
         USING AGND,R6                                                          
BLDCTF   NTR1                                                                   
         MVC   HEADNM,SPACES                                                    
         MVC   HEADNM(27),=C'CONTROL FILE RECORDS'                              
         LA    R4,XP                                                            
         MVC   XP,XSPACES          CLEAR PRINT LINE                             
         L     R5,AAGNTAB                                                       
         ICM   R0,15,BININ         R0=NUMBER IN TABLE                           
         BZ    BLDCTFX             NOTHING TO PROCESS                           
         LA    R6,BINTAB                                                        
         L     R2,AIO1                                                          
         XC    LSTCLI,LSTCLI                                                    
*                                                                               
BLDCTF10 L     RE,AIO1                                                          
         LA    RF,MXRLNQ                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         CLC   LSTCLI,AGNSCLI                                                   
         BE    BLDCTF20                                                         
         MVI   ZENKSYS,ZENKCODQ    X'05' - ZENITH RECORDS                       
         MVI   ZENKTYP,ZENCLTQ     X'09' - CLIENT RECORD TYPE                   
         MVC   ZENKAGY,=C'TH'      ZENITH AGENCY CODE                           
         MVC   ZENKCLT,AGNSCLI     SORT CLIENT CODE                             
         MVC   ZENRLEN,=Y(ZENFIRST-ZENRKEY)                                     
         XC    ZENRSTAT,ZENRSTAT                                                
         AP    PKCTCNT,=P'1'                                                    
*                                                                               
         BAS   RE,ELEM01           BUILD '01' ELEMENT                           
         BAS   RE,ADDCTF           ADD IT                                       
*                                                                               
         MVC   P5CLI,AGNSCLI       CLIENT CODE                                  
         MVC   P5CNAM,AGNCNAM      CLIENT NAME                                  
         MVC   P5AGY,AGNAGY        ORIGINATING AGENCY                           
         GOTO1 ACREPORT            PRINT IT                                     
         CLC   LINE,MAXLINES       ARE WE AT END OF PAGE?                       
         BNL   BLDCTF20                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
BLDCTF20 MVC   LSTCLI,AGNSCLI      UPDATE SAVED FIELD                           
         LA    R6,AGNLNQ(R6)                                                    
         BCT   R0,BLDCTF10                                                      
*                                                                               
BLDCTFX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD ZENITH ELEMENT                                                  *         
***********************************************************************         
         SPACE 1                                                                
ELEM01   NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         USING ZENELEM,R4                                                       
         LA    R4,ELEMENT          R4 = A(ELEMENT WORK AREA)                    
         MVI   ZENELCD,ZENELCDQ    X'01' - ZENITH ELEMENT                       
         MVI   ZENELLN,ZENELENE    ELEMENT LENGTH X'50'                         
         MVC   ZENCAGOF,AGNAGY     ORIGINAL AGENCY CODE                         
         MVC   ZENCNAME,AGNCNAM    CLIENT NAME                                  
         BAS   RE,ADD01                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ZENITH RECORD TO CTFILE                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING ZENRECD,R2                                                       
ADDCTF   NTR1                                                                   
         L     R2,AIO1                                                          
         SR    R5,R5                                                            
         ICM   R5,3,ZENRLEN        SET LENGTH FOR TAPE                          
         LA    R5,4(R5)            INCREASE LENGTH BY 4                         
         STH   R5,RLN1             SET LENGTH                                   
         LA    R5,RLN1                                                          
*                                                                               
         TM    OPTN,OPTDUMP        DUMP OUTPUT                                  
         BNO   ADDCTF10                                                         
         MVC   MSG,=CL10'ACCT  REC'                                             
         SR    R4,R4                                                            
         LH    R4,RLN1                                                          
         GOTO1 DUMP,DMCB,(RC),RLN1,(R4)                                         
*                                                                               
ADDCTF10 CLI   QOPT7,C'T'          TEST TAPE OUTPUT                             
         BNE   ADDCTFX                                                          
         L     R3,ATOUT                                                         
         PUT   (R3),(R5)                                                        
ADDCTFX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT                                                         *         
*     R4 = A(ELEMENT)                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADD01    NTR1                                                                   
         L     R2,AIO1                                                          
         GOTO1 HELLO,DMCB,(C'P',CTFILE),(R2),(R4)                               
         LR    RE,R0                                                            
         B     EXIT                                                             
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT TOTAL ROUTINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTTOT   NTR1                                                                   
         LA    R4,XP               R4 = A(PRINT LINE)                           
         USING PLINED,R4                                                        
         MVC   HEADNM,SPACES                                                    
         MVC   HEADNM(12),=C'GRAND TOTALS'                                      
*                                                                               
         EDIT  (P8,PKSJCNT),(10,P6SJCNT),ZERO=NOBLANK                           
         EDIT  (P8,PKSRCNT),(10,P6SRCNT),ZERO=NOBLANK                           
         EDIT  (P8,PK1CCNT),(10,P61CCNT),ZERO=NOBLANK                           
         EDIT  (P8,PKCTCNT),(10,P6CTCNT),ZERO=NOBLANK                           
         AP    PKGDTOT,PKSJCNT     ADD SJ COUNT TO TOTAL                        
         AP    PKGDTOT,PKSRCNT     ADD SR COUNT TO TOTAL                        
         AP    PKGDTOT,PK1CCNT     ADD 1C COUNT TO TOTAL                        
         AP    PKGDTOT,PKCTCNT     ADD CT COUNT TO TOTAL                        
         EDIT  (P8,PKGDTOT),(10,P6GDTOT)                                        
         GOTO1 ACREPORT                                                         
         CLC   LINE,MAXLINES       ARE WE AT END OF PAGE?                       
         BNL   PRTTOTX                                                          
         GOTO1 ACREPORT                                                         
*                                                                               
PRTTOTX  B     EXIT                                                             
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         ICM   R6,1,BINFST         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         LR    R5,R4               A(RECORD FOUND)                              
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
         CLI   SVSYSID,C'A'        COMPARE IF ACC                               
         BNE   *+8                   IF NOT SKIP TO NEXT COMPARE                
         LA    R5,AGNAMLNQ(R5)       ELSE BUMP TO ACC  MEDIA LIST               
         CLI   SVSYSID,C'S'        COMPARE IF SPOT                              
         BNE   *+8                   IF NOT SKIP TO NEXT COMPARE                
         LA    R5,AGNSMLNQ(R5)       ELSE BUMP TO SPOT MEDIA LIST               
         CLI   SVSYSID,C'N'        COMPARE IF NET                               
         BNE   *+8                   IF NOT SKIP TO NEXT COMPARE                
         LA    R5,AGNNMLNQ(R5)       ELSE BUMP TO NET  MEDIA LIST               
         CLI   SVSYSID,C'P'        COMPARE IF PRINT                             
         BNE   *+8                   IF NOT SKIP LA                             
         LA    R5,AGNPMLNQ(R5)       ELSE BUMP TO PRINT MEDIA LIST              
BIN10    OC    0(2,R5),0(R5)       CHECK IF ANYTHING IN LIST                    
         BZ    BIN20                                                            
         CLC   0(2,R5),SVMEDLS     COMPARE IF NEW MEDIA IS IN LIST              
         BE    BINXIT                IF IT IS DONT ADD - OVERWRITE              
         LA    R5,2(R5)            BUMP TO NEXT ENTRY                           
         B     BIN10                                                            
BIN20    DS    0H                                                               
         MVC   MSG,=CL10'BIN TAB'                                               
         GOTO1 DUMP,DMCB,(RC),(R4),AGNLNQ                                       
*                                                                               
         MVC   MSG,=CL10'BIN REC'                                               
         GOTO1 DUMP,DMCB,(RC),(R3),AGNLNQ                                       
*                                                                               
         MVC   0(2,R5),SVMEDLS     UPDATE MEDIA LIST                            
         AP    0(8,R4),0(8,R3)     ADD TO COUNTER                               
*                                                                               
BINXIT   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R2                                                       
DMHIADIR NTR1                      DMRDHI FOR ACCT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCDIR',DKEY,DIR,0                
         LA    R2,DIR                                                           
         MVC   DA,ACCKDA           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSEADIR NTR1                      DMRSEQ FOR ACCT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'ACCDIR',DKEY,DIR,0                
         LA    R2,DIR                                                           
         MVC   DA,ACCKDA           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETACC NTR1                      GETREC FOR ACCT FILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'ACCMST',DA,AIO1,DMWORK            
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMHISDIR NTR1                      DMRDHI FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'SPTDIR',DKEY,DIR,0                
         MVC   DA,DIR+14           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSESDIR NTR1                      DMRSEQ FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'SPTDIR',DKEY,DIR,0                
         MVC   DA,DIR+14           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETSPT NTR1                      GETREC FOR SPOT FILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'SPTFILE',DA,AIO1,DMWORK           
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMHIPDIR NTR1                      DMRDHI FOR PRINT DIR                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',DKEY,DIR,0                
         MVC   DA,DIR+27           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSEPDIR NTR1                      DMRSEQ FOR PRINT DIR                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PRTDIR',DKEY,DIR,0                
         MVC   DA,DIR+27           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETPRT NTR1                      GETREC FOR PRINT FILE                        
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PRTFILE',DA,AIO1,DMWORK           
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMCHK    NTR1                      CHECK RETURNS ON DATAMGR CALLS               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
         CLI   QOPT4,C'Y'                                                       
         BNE   DUMPX                                                            
*        CP    MAXDMP,DMPTOT       CHECK IF MAXIMUM WAS REACHED                 
*        BNH   DUMPX                                                            
*        AP    DMPTOT,=P'1'                                                     
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 DUMP,DMCB,(RC),(R2),(R6)                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BNE   BXHK10                                                           
*                                                                               
         TM    OPTN,OPTSYS                                                      
         BNO   *+10                                                             
         MVC   XHEAD2+69(L'SYSNAME),SYSNAME        SYSTEM NAME                  
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(PZENPRD-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PCLT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PPRD-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PCLTNAM-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PAGY-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PAGYNAM-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PSYSNAM-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PRECV-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(PCOST-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(PDPL-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXHK10   DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   BXHK50                                                           
*                                                                               
         LA    RE,HEADTAB              PRINT HEADINGS                           
BXHK20   CLI   0(RE),X'FF'             END OF TABLE                             
         BE    BXHK40                                                           
         CLC   BYTE,0(RE)                                                       
         BE    *+12                                                             
         LA    RE,L'HEADTAB(RE)                                                 
         B     BXHK20                                                           
*                                                                               
         LA    RE,1(RE)            BUMP PAST ID NUMBER                          
         LA    RF,XHEAD7+42                                                     
BXHK30   CLI   0(RE),C'*'              END OF LIST?                             
         BE    BXHK40                                                           
         MVC   0(L'ALPHAID,RF),0(RE)   MOVE IN ALPHAID                          
         LA    RE,L'ALPHAID(RE)                                                 
         LA    RF,12(RF)                                                        
         B     BXHK30                                                           
*                                                                               
BXHK40   MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(PZCLT-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(PACTNAM-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PBSCNT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PBTCNT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PDWCNT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PDFCNT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PDTCNT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PSFCNT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PDPL2-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+PRLNQ2,C'R'                                              
*                                                                               
BXHK45   MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXHK50   DS    0H                                                               
         CLI   RCSUBPRG,2                                                       
         BNE   BXHK60                                                           
*                                                                               
         MVC   XHEAD2+69(L'HEADNM),HEADNM          SYSTEM NAME                  
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(P3ZCLT-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(P3ACTNAM-PRTLNE-1),C'C'                                 
         MVI   BOXCOLS+(P3AGY-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(P3ACMD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(P3SPMD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(P3PTMD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(P3NTMD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(P3DPL-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+PRLNQ3,C'R'                                              
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXHK60   DS    0H                                                               
         CLI   RCSUBPRG,3                                                       
         BNE   BXHK70                                                           
*                                                                               
         MVC   XHEAD2+69(L'HEADNM),HEADNM          SYSTEM NAME                  
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(P4TYPE-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(P4OFF-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(P4CLT-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(P4PRD-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(P4ANAM-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+PRLNQ4,C'R'                                              
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXHK70   DS    0H                                                               
         CLI   RCSUBPRG,4                                                       
         BNE   BXHK80                                                           
*                                                                               
         MVC   XHEAD2+69(L'HEADNM),HEADNM          SYSTEM NAME                  
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(P5CNAM-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(P5AGY-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+PRLNQ5,C'R'                                              
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXHK80   DS    0H                                                               
         CLI   RCSUBPRG,5                                                       
         BNE   BXXIT                                                            
*                                                                               
         MVC   XHEAD2+69(L'HEADNM),HEADNM          SYSTEM NAME                  
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+5,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(P6SRCNT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(P61CCNT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(P6CTCNT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(P6GDTOT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+PRLNQ6,C'R'                                              
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
UTL      DS    0D                                                               
         DC    F'0',X'02'                                                       
         DC    XL3'00'                                                          
*                                                                               
MXRLNQ   EQU   2000                MAX RECORD SIZE                              
*                                                                               
OPTN     DC    X'00'               RUN OPTIONS                                  
OPTTAPE  EQU   X'80'               TAPE IS OPEN                                 
OPTCLT   EQU   X'40'               RUN ON CLIENT LEVEL ONLY                     
OPTDUMP  EQU   X'20'               DUMP OUTPUT RECORDS                          
OPTAGY   EQU   X'10'               PRINT SELECTED AGENCY ONLY                   
OPTSYS   EQU   X'08'               RUN ON SELECTED FILES ONLY (N,P,S)           
*                                                                               
OPTNPRT  DC    X'00'               PRINT OPTIONS                                
OPTPRT1  EQU   X'80'               PRINT ROUTINE 1 - TOTALS BY AGENCY           
OPTPRT2  EQU   X'40'               PRINT ROUTINE 2 - GROUPS BY AGENCY           
OPTPRT3  EQU   X'20'               PRINT ROUTINE 3 - REGULAR PRINTOUT           
OPTPRT4  EQU   X'10'               PRINT ROUTINE 3 - REGULAR PRINTOUT           
TURNOFF  EQU   X'FF'                                                            
*                                                                               
DMPTOT   DC    PL4'0'              DUMP COUNT                                   
MAXDMP   DC    PL4'15'             MAXIMUM DUMPS                                
*                                                                               
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
CLUNPK   DC    V(CLUNPK)                                                        
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
*                                                                               
AIO1     DC    A(IO1)                                                           
ACLITAB  DC    A(CLITAB)                                                        
AAGNTAB  DC    A(AGNTAB)                                                        
ASJTAB   DC    A(SJTAB)                                                         
AOUTFIL  DC    A(OUTFIL)                                                        
ATOUT    DC    A(TOUT)                                                          
         DC    X'FF'                                                            
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=116 '                                     
SORTCARD DC    C'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1  '                        
*                                                                               
SPFLIST  DS    0H                  SPOT FILE LIST                               
         DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    C'X'                                                             
*                                                                               
PRFLIST  DS    0H                  PRINT FILE LIST                              
         DC    CL8'NPRTDIR'                                                     
         DC    CL8'NPRTFILE'                                                    
         DC    C'X'                                                             
*                                                                               
ACFLIST  DS    0H                  ACCOUNTING FILE LIST                         
         DC    CL8'NACCDIR'                                                     
         DC    CL8'NACCMST'                                                     
         DC    CL8'NACCARC'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
MEDTAB   DS    0C                  TABLE OF MEDIA CODES FOR PRINT               
         DC    C'I'                                                             
         DC    C'M'                                                             
         DC    C'N'                                                             
         DC    C'O'                                                             
         DC    C'S'                                                             
         DC    C'T'                                                             
         DC    X'FF'                                                            
*                                                                               
SYSTAB   DS    0F                  SYSTEM FILES                                 
         DC    X'80',X'03',C'DT',C'SD',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'40',X'03',C'SF',C'SD',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'B0',X'07',C'DF',C'SD',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'20',X'22',C'BS',C'SB',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'70',X'F4',C'BD',C'SC',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'70',X'15',C'NE',C'SC',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'20',X'23',C'BT',C'NB',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'50',X'25',C'DF',C'ND',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'40',X'25',C'BD',C'NC',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'90',X'23',C'NE',C'NC',C'SPOT    ',AL4(SPFLIST,SPTRD)           
         DC    X'C4',X'04',C'DF',C'PD',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'C5',X'04',C'DT',C'PD',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'E2',X'04',C'SF',C'PD',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'C9',X'54',C'BS',C'PB',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'C4',X'54',C'BD',C'PC',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'D7',X'24',C'NE',C'PC',C'PRINT   ',AL4(PRFLIST,PRTRD)           
         DC    X'6F',X'18',C'DW',C'AD',C'ACCOUNT ',AL4(ACFLIST,ACCRD)           
         DC    X'6E',X'96',C'BS',C'AB',C'ACCOUNT ',AL4(ACFLIST,ACCRD)           
         DC    X'F5',X'A6',C'BD',C'AC',C'ACCOUNT ',AL4(ACFLIST,ACCRD)           
         DC    X'E8',X'66',C'NE',C'AC',C'ACCOUNT ',AL4(ACFLIST,ACCRD)           
         DC    X'FF'                                                            
*                                                                               
IDTAB    DS    0CL38                                                            
         DC    C'BS',CL36'BATES USA'                                            
         DC    C'BT',CL36'BACKER SPIELVOGEL BATES INC.'                         
         DC    C'DF',CL36'SAATCHI AND SAATCHI ADVERTISING INC.'                 
         DC    C'DT',CL36'SAATCHI AND SAATCHI DFS INC.'                         
         DC    C'DW',CL36'SAATCHI AND SAATCHI NORTH AMERICA INC'                
         DC    C'SF',CL36'SAATCHI AND SAATCHI NORTH AMERICA INC'                
         DC    C'BD',CL36'BBDO'                                                 
         DC    C'NE',CL36'DDB WORLDWIDE COMM, INC'                              
         DC    X'FF'                                                            
*                                                                               
SPTTAB   DS    0CL2                                                             
         DC    X'01',C'T'          TELEVISION                                   
         DC    X'02',C'R'          RADIO                                        
         DC    X'03',C'N'          NATIONAL TELEVISION                          
         DC    X'04',C'X'          NATIONAL RADIO                               
         DC    X'08',C'C'                                                       
         DC    X'FF'                                                            
*                                                                               
HEADTAB  DS    0CL14                                                            
         DC    X'01',CL12'BSBTDFDTDWSF',C'*'  BATES/SAATCHI CONVERSION          
         DC    X'02',CL12'BDNE',C'*'          BD/NE CONVERSION                  
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DCBS                                                                *         
***********************************************************************         
         SPACE 1                                                                
OUTFIL   DCB   DDNAME=OUTFIL,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,LRECL=2048,BLKSIZE=8032,BUFNO=2                         
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
         DC    C'**IO1***'                                                      
RLN1     DC    F'0'                      IOAREA #1                              
IO1      DC    (MXRLNQ)X'00'                                                    
*                                                                               
*        BINTABLE CONSTANTS FOR TABLE 1                                         
*                                                                               
CLITAB   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(CLILNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(CLIKLNQ)        KEY LENGTH                                   
         DC    AL4(CLIMAX)         MAX IN TABLE                                 
         DC    AL1(0)              NUMBER OF BUCKETS - NO BUCKETS               
         DC    AL1(0)              DISPLACEMENT TO FIRST BUCKET                 
         DS    (CLIMAX*CLILNQ)XL1  TABLE                                        
*                                                                               
*        BINTABLE CONSTANTS FOR TABLE 2                                         
*                                                                               
AGNTAB   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(AGNLNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(AGNKLNQ)        KEY LENGTH                                   
         DC    AL4(AGNMAX)         MAX IN TABLE                                 
         DC    AL1(AGNBKCT)        NUMBER OF BUCKETS                            
         DC    AL1(AGNBKT-AGND)    DISPLACEMENT TO FIRST BUCKET                 
         DS    (AGNMAX*AGNLNQ)XL1  TABLE                                        
*                                                                               
*        BINTABLE CONSTANTS FOR TABLE 3                                         
*                                                                               
SJTAB    DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(SJLNQ)          LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(SJKYLNQ)        KEY LENGTH                                   
         DC    AL4(SJMAX)          MAX IN TABLE                                 
         DC    AL1(0)              NUMBER OF BUCKETS - NO BUCKETS               
         DC    AL1(0)              DISPLACEMENT TO FIRST BUCKET                 
         DS    (SJMAX*SJLNQ)XL1    TABLE                                        
*                                                                               
CLIMAX   EQU   5000                                                             
AGNMAX   EQU   8000                                                             
SJMAX    EQU   20000                                                            
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
ACZGD    DSECT                                                                  
AUTL     DS    A                                                                
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
SAVEKEY  DS    CL(L'ACCKEY)        SAVED AREA FOR KEY                           
PRTCKEY  DS    CL7                 STORAGE FOR PRINT KEY FOR CLIENT             
PRTPKEY  DS    CL10                STORAGE FOR PRINT KEY FOR PRODUCT            
SESAVE   DS    XL1                 SE NUMBER SAVE AREA                          
SVSYSID  DS    CL1                 SAVED AREA FOR SYSTEM ID                     
SVAGY    DS    CL1                 SAVED AREA FOR AGENCY ID                     
SVZEN    DS    CL3                 SAVED AREA FOR ZENITH CODE                   
*                                                                               
SVAGZN   DS    0CL6                SAVED AREA FOR AGENCY/ZENITH CODES           
SVAGCLT  DS    CL3                   AGENCY CLIENT CODE                         
SVZNCLT  DS    CL3                   ZENITH CLIENT CODE                         
*                                                                               
SYSSAVE  DS    CL1                 SAVED AREA FOR SYSTEM RUN REQUEST ID         
SYSNAME  DS    CL7                 SAVED AREA FOR SYSTEM RUN REQUEST            
HEADNM   DS    CL36                SAVED AREA FOR AGENCY RUN REQUEST            
SVSENUM  DS    XL1                 SAVE AREA FOR SYSTEM SE NUMBER               
SAVEID   DS    CL2                 SAVE AREA FOR COMPANY ID                     
SVMEDLS  DS    CL2                 SAVED AREA FOR MEDIA LIST                    
SPTMED   DS    XL1                 SAVED AREA FOR SPOT/NET MEDIA                
SAVEAGN  DS    XL1                 SAVED AREA FOR AGENCY/MEDIA                  
*                                                                               
PKFLDS   DS    0PL8                START OF PACKED FIELDS                       
PKDPCNT  DS    PL8                 PACKED FIELD FOR DUPLICATE COUNT             
PKSJCNT  DS    PL8                 TOTAL SJ RECORDS CREATED                     
PKSRCNT  DS    PL8                 TOTAL SR RECORDS CREATED                     
PK1CCNT  DS    PL8                 TOTAL 1C RECORDS CREATED                     
PKCTCNT  DS    PL8                 TOTAL CT RECORDS CREATED                     
PKGDTOT  DS    PL8                 GRAND TOTAL OF ALL RECORDS CREATED           
PKNUMQ   EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
FLAG     DS    XL1                                                              
FLGCTF   EQU   X'80'               FLAG FOR CTFILE XSORT                        
*                                                                               
DUPFLG   DS    XL1                 FLAG FOR DUPLICATES                          
DUPFLGB  EQU   X'80'               DUP FLAG FOR BATES                           
DUPFLGT  EQU   X'40'               DUP FLAG FOR ZENITH                          
DUPFLGD  EQU   X'20'               DUP FLAG FOR SAATCHI                         
DUPFLGO  EQU   X'10'               DUP FLAG FOR BD/NE                           
*                                                                               
SRTBUFF  DS    CL(SRTLNQ)          BUFFER FOR SORTER                            
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
*                                                                               
NAME     DS    CL36                                                             
*                                                                               
COMMAND  DS    CL8                 USED IN DATAMGR IO                           
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
*                                                                               
LSTCPA   DS    0CL8                SAVED FOR LAST CLIENT/PROD/AGENCY            
LSTCLI   DS    CL3                 SAVED AREA FOR LAST CLIENT                   
LSTPRD   DS    CL3                 SAVED AREA FOR LAST PRODUCT                  
LSTAGY   DS    CL2                 SAVED AREA FOR LAST AGENCY                   
*                                                                               
LSTAGID  DS    CL1                 SAVED AREA FOR AGENCY ID                     
LSTUL    DS    CL2                 SAVED AREA FOR UNIT/LEDGER                   
*                                                                               
MSG      DS    CL10                MESSAGE FOR DUMP                             
*                                                                               
CLIWRK   DS    CL(CLILNQ)          WORK AREA FOR CLIENT ENTRY                   
AGNWRK   DS    CL(AGNLNQ)          WORK AREA FOR AGENCY ENTRY                   
SJWRK    DS    CL(SJLNQ)           WORK AREA FOR SJ RECORD ENTRY                
*                                                                               
ELEMENT  DS    XL255                                                            
         EJECT                                                                  
***********************************************************************         
* SYSTEM TABLE FILE DSECT                                             *         
***********************************************************************         
         SPACE 1                                                                
SYSTBLD  DSECT                                                                  
SYSTMED  DS    0XL1                COMPANY AGENCY/MEDIA                         
SYSTCPY  DS    XL1                 COMPANY ID (ACC SIDE ONLY)                   
SYSTSE   DS    XL1                 AGENCY SE NUMBER                             
SYSTCODE DS    CL2                 AGENCY CHARACTER CODE                        
SYSTID   DS    CL1                 ONE BYTE SYSTEM IDENTIFICATION               
SYSTAGY  DS    CL1                 ONE BYTE AGENCY IDENTIFICATION               
SYSTNAM  DS    CL8                 SYSTEM NAME (SPOT, NET, PRINT)               
ASYSFIL  DS    AL4                 A(SYSTEM FILES TO BE OPEN)                   
ASYSRD   DS    AL4                 A(SYSTEM READ ROUTINES)                      
SYSTLNQ  EQU   *-SYSTBLD           LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN CLIENT/PRODUCT TABLE 1                           *         
***********************************************************************         
         SPACE 1                                                                
CLID     DSECT                                                                  
CLICLI   DS    CL3             CLIENT CODE                                      
CLIPRD   DS    CL3             PRODUCT CODE                                     
CLIKLNQ  EQU   *-CLID          LENGTH OF KEY                                    
CLIAGY   DS    CL2             SOURCE AGENCY CODE(FOR PRINT ONLY)               
CLIMED   DS    CL1             SOURCE AGY/MED CODE(ONLY MEDIA - PRINT)          
CLISYS   DS    XL1             X'80' - SPOT/X'40' - ACC/X'00' - PRINT           
CLISE    DS    XL1             SE NUMBER FOR SYSTEM                             
CLIID    DS    CL1             ONE BYTE SYSTEM ID (N,P,S)                       
CLIAGNID DS    CL1             ONE BYTE AGENCY ID (B,C,D)                       
CLINAM   DS    CL36            CLIENT NAME(ONLY FOR ACC)                        
CLILNQ   EQU   *-CLID          LENGTH                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN CLIENT/PRODUCT TABLE 2                           *         
***********************************************************************         
         SPACE 1                                                                
AGND     DSECT                                                                  
AGNACLI  DS    CL3             AGENCY CLIENT CODE                               
AGNZCLI  DS    CL3             ZENITH CLIENT CODE                               
AGNAGY   DS    CL2             SOURCE AGENCY CODE                               
AGNKLNQ  EQU   *-AGND          LENGTH OF KEY                                    
AGNBKT   DS    PL8                 BUCKET                                       
AGNBKLN  EQU   *-AGNBKT            BUCKET LENGTH                                
AGNBKCT  EQU   (*-AGNBKT)/AGNBKLN  NUMBER OF BUCKETS                            
AGNXL1Q  EQU   *-AGND              DISPLACEMENT TO ID FOR XSORT                 
AGNSYSID DS    CL1                 AGENCY ID(B-BATES,D-SAATCHI)                 
AGNXL2Q  EQU   *-AGND              DISPLACEMENT TO SORT CLI FOR XSORT           
AGNSCLI  DS    CL3             SORT CLIENT CODE EITHER AGENCY/ZENITH            
AGNPCLI  DS    CL2             AGENCY CLIENT CODE PACKED                        
AGNCNAM  DS    CL20            CLIENT NAME                                      
AGNMED   DS    CL1             SOURCE AGY/MED CODE(ONLY MEDIA - PRINT)          
AGNSYS   DS    XL1             SYSTEM ID                                        
AGNSPT   EQU   X'80'           SPOT/NET   SYSTEM                                
AGNACC   EQU   X'40'           ACCOUNTING SYSTEM                                
AGNPRT   EQU   X'00'           PRINT      SYSTEM                                
AGNRAN   EQU   X'10'           ALREADY RUN                                      
AGNSPSE  DS    XL1             SE NUMBER FOR SPOT  SYSTEM                       
AGNNTSE  DS    XL1             SE NUMBER FOR NET   SYSTEM                       
AGNPTSE  DS    XL1             SE NUMBER FOR PRINT SYSTEM                       
AGNID    DS    CL1             ONE BYTE SYSTEM ID (N,P,S)                       
AGNAMLNQ EQU   *-AGND          DISPLACEMENT TO ACC   MEDIA LIST                 
AGNACMD  DS    CL12            ACC   MEDIA LIST                                 
AGNSMLNQ EQU   *-AGND          DISPLACEMENT TO SPOT  MEDIA LIST                 
AGNSPMD  DS    CL12            SPOT  MEDIA LIST                                 
AGNPMLNQ EQU   *-AGND          DISPLACEMENT TO PRINT MEDIA LIST                 
AGNPTMD  DS    CL12            PRINT MEDIA LIST                                 
AGNNMLNQ EQU   *-AGND          DISPLACEMENT TO NET   MEDIA LIST                 
AGNNTMD  DS    CL12            NET   MEDIA LIST                                 
AGNDUP   DS    CL9             DUPLICATE FIELD                                  
AGNLNQ   EQU   *-AGND          LENGTH                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN SJ RECORD TABLE - BINADD TABLE 3                 *         
***********************************************************************         
         SPACE 1                                                                
SJRECD   DSECT                                                                  
SJCDE    DS    XL1             SJ CLIENT CODE                                   
SJUL     DS    CL2             SJ UNIT/LEDGER(SR OR 1C)                         
SJTYPE   DS    CL1             TYPE(SR - A     1C - B,C OR S)                   
SJOFF    DS    CL2             OFFICE(SR - BS OR SA   1C - 01)                  
SJCLI    DS    CL3             CLIENT CODE(ONLY FOR 1C)                         
SJPRD    DS    CL3             PRODUCT CODE(ONLY FOR 1C)                        
SJKYLNQ  EQU   *-SJRECD        LENGTH OF KEY                                    
SJANAM   DS    CL36            AGENCY NAME(ONLY FOR SR RECORDS)                 
SJCPNAM  DS    CL20            CLIENT OR PRODUCT NAME(1C RECORDS)               
SJLNQ    EQU   *-SJRECD        LENGTH                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* PRINT DESCT                                                         *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                  PRINT LINE                                   
         DS    CL2                                                              
PZENCLT  DS    CL3                 ZENITH CLIENT CODE                           
         DS    CL2                                                              
PZENPRD  DS    CL3                 ZENITH PRODUCT CODE                          
         DS    CL2                                                              
PCLT     DS    CL3                 CLIENT CODE                                  
         DS    CL2                                                              
PPRD     DS    CL3                 PRODUCT CODE                                 
         DS    CL2                                                              
PCLTNAM  DS    CL20                CLIENT NAME                                  
         DS    CL5                                                              
         ORG   PCLTNAM             CLIENT/PRODUCT NAME IN SAME POSITION         
PPRDNAM  DS    CL20                PRODUCT NAME                                 
         DS    CL5                                                              
PAGY     DS    CL2                 AGENCY CODE                                  
         DS    CL3                                                              
PAGYNAM  DS    CL36                COMPANY NAME                                 
         DS    CL2                                                              
PSYSNAM  DS    CL3                 SYSTEM  NAME                                 
         DS    CL2                                                              
PRECV    DS    CL12                'SR' - RECEIVABLE ACCOUNT                    
         DS    CL2                                                              
PCOST    DS    CL12                '1C' - COSTING ACCOUNT                       
         DS    CL2                                                              
PDPL     DS    CL9                 DUPLICATE OR NOT?????                        
PRLNQ    EQU   *-PLINED            LENGTH                                       
         ORG   PRTLNE                                                           
         DS    CL2                 PRINT OUT #1 - # OF OCCURANCES               
PACLT    DS    CL3                 AGENCY CLIENT CODE                           
         DS    CL2                                                              
PZCLT    DS    CL3                 ZENITH CLIENT CODE                           
         DS    CL2                                                              
PACTNAM  DS    CL20                AGENCY CLIENT NAME                           
         DS    CL5                                                              
PBSCNT   DS    0CL10               BS/BD COUNT                                  
PBDCNT   DS    CL10                                                             
         DS    CL2                                                              
PBTCNT   DS    0CL10               BT/NE COUNT                                  
PNECNT   DS    CL10                                                             
         DS    CL2                                                              
PDWCNT   DS    CL10                DW COUNT                                     
         DS    CL2                                                              
PDFCNT   DS    CL10                DF COUNT                                     
         DS    CL2                                                              
PDTCNT   DS    CL10                DT COUNT                                     
         DS    CL2                                                              
PSFCNT   DS    CL10                SF COUNT                                     
         DS    CL2                                                              
PDPL2    DS    CL9                 DUPLICATE OR NOT?????                        
PRLNQ2   EQU   *-PLINED            LENGTH                                       
         ORG   PRTLNE                                                           
         DS    CL2                 PRINT OUT #3 - WHERE FOUND                   
P3ACLT   DS    CL3                 AGENCY CLIENT CODE                           
         DS    CL2                                                              
P3ZCLT   DS    CL3                 ZENITH CLIENT CODE                           
         DS    CL2                                                              
P3ACTNAM DS    CL20                AGENCY CLIENT NAME                           
         DS    CL5                                                              
P3AGY    DS    CL2                 AGENCY CODE                                  
         DS    CL3                                                              
P3ACMD   DS    CL18                ACC   MEDIA LIST                             
         DS    CL2                                                              
P3SPMD   DS    CL18                SPOT  MEDIA LIST                             
         DS    CL2                                                              
P3NTMD   DS    CL18                NET   MEDIA LIST                             
         DS    CL2                                                              
P3PTMD   DS    CL18                PRINT MEDIA LIST                             
         DS    CL2                                                              
P3DPL    DS    CL9                 DUPLICATE OR NOT?????                        
PRLNQ3   EQU   *-PLINED            LENGTH                                       
         ORG   PRTLNE                                                           
         DS    CL2                 PRINT OUT #4 - RECEIVABLES/COSTING           
P4UL     DS    CL2                 UNIT/LEDGER                                  
         DS    CL5                                                              
P4TYPE   DS    CL1                 TYPE                                         
         DS    CL5                                                              
P4OFF    DS    CL2                 OFFICE                                       
         DS    CL5                                                              
P4CLT    DS    CL3                 CLIENT CODE (SR - 000)                       
         DS    CL5                                                              
P4PRD    DS    CL3                 PRODUCT CODE (SR - 000)                      
         DS    CL7                                                              
P4ANAM   DS    CL36                AGENCY NAME (SR ONLY)                        
         ORG   P4ANAM                                                           
P4CPNAM  DS    CL20                CLI/PROD NAME (1C ONLY) - LEVEL              
         DS    CL16                SPARE                                        
PRLNQ4   EQU   *-PLINED            LENGTH                                       
         ORG   PRTLNE                                                           
         DS    CL2                 PRINT OUT #5 - CTFILE RECORDS                
P5CLI    DS    CL3                 CLIENT CODE                                  
         DS    CL5                                                              
P5CNAM   DS    CL20                CLIENT NAME                                  
         DS    CL5                                                              
P5AGY    DS    CL2                 ORIGINATING AGENCY                           
         DS    CL5                                                              
PRLNQ5   EQU   *-PLINED            LENGTH                                       
         ORG   PRTLNE                                                           
         DS    CL2                 PRINT OUT #6 - TOTAL OF SJ RECORDS           
P6SJCNT  DS    CL10                NUMBER OF SJ RECORDS ADDED                   
         DS    CL5                                                              
P6SRCNT  DS    CL10                NUMBER OF SR RECORDS ADDED                   
         DS    CL5                                                              
P61CCNT  DS    CL10                NUMBER OF 1C RECORDS ADDED                   
         DS    CL5                                                              
P6CTCNT  DS    CL10                NUMBER OF CT RECORDS ADDED                   
         DS    CL5                                                              
P6GDTOT  DS    CL10                TOTAL NUMBER OF RECORDS ADDED                
         DS    CL5                                                              
PRLNQ6   EQU   *-PLINED            LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* SORT DSECT 1                                                        *         
***********************************************************************         
         SPACE 1                                                                
SORTD    DSECT                                                                  
SRTCLT   DS    CL3                 CLIENT CODE                                  
SRTPRD   DS    CL3                 PRODUCT CODE                                 
SRTAGY   DS    CL2                 AGENCY CODE                                  
SRTACLT  DS    CL3                 ORIG CLIENT CODE(IF OVERRIDE EXISTS)         
SRTAGNID DS    CL1                 AGENCY ID(B-BATE,T-ZENITH,D-SAATCHI)         
SRTCNAM  DS    CL20                CLIENT NAME                                  
SRT1CPRD DS    CL3                 PRODUCT CODE FOR 1C ACCOUNT                  
SRTPNAM  DS    CL20                PRODUCT NAME                                 
SRTANAM  DS    CL36                COMPANY NAME                                 
SRTSYS   DS    CL1                 ONE BYTE SYSTEM ID (N,P,S)                   
SRTSRRC  DS    CL12                SJ RECORD                                    
SRT1CRC  DS    CL12                1C RECORD                                    
SRTLNQ   EQU   *-SORTD             LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* SPOT/NET FILE ++INCLUDES AND DSECTS                                 *         
***********************************************************************         
         SPACE 1                                                                
*        SPOT CLIENT DSECT                                                      
*                                                                               
SPCLID   DSECT                                                                  
*          DATA SET SPGENCLT   AT LEVEL 054 AS OF 12/09/94                      
CLTHDR   DS    0C                                                               
CKEY     DS    0CL13     V         KEY                                          
CKEYTYPE DS    CL1       B         RECORD TYPE X'00'                            
CKEYAM   DS    CL1       A/M       AGENCY/MEDIA                                 
CKEYCLT  DS    CL2       CLT       CLIENT CODE                                  
CKEYZRO  DS    CL9       B         BINARY ZEROS                                 
         SPACE 2                                                                
CLEN     DS    CL2       B         RECORD LENGTH (1000)                         
CCNTRL   DS    CL1       B         CONTROL BYTE                                 
CLINKS   DS    CL4       B         LINK FIELDS                                  
         DS    CL2       B         SPARE                                        
         DS    CL2       CLT       CLIENT INTERFACE                             
         SPACE 2                                                                
CNAME    DS    CL20      A         CLIENT NAME                                  
COFFICE  DS    CL1       N         OFFICE NUMBER                                
CPROF    DS    CL15      A/N       CLIENT PROFILE (SEE MANUAL)                  
CLIST    DS    880C      V         PRODUCT CODE LIST                            
*                        A         4 BYTE FIELDS  1-3=PRODUCT MNEMONIC          
*                        B                          4=PRODUCT NUMBER            
CCLTIFC  DS    CL8       A/N       NEW CLIENT INTERFACE CODE                    
CACCOFC  DS    CL2       A/N       2 CHAR ACC OFFICE CODE                       
*                                                                               
CGRP1    DS    CL3                 CLTGRP ASSGN                                 
CGRP2    DS    CL3                 CLTGRP ASSGN                                 
CGRP3    DS    CL3                 CLTGRP ASSGN                                 
CGRP4    DS    CL3                 CLTGRP ASSGN                                 
CGRP5    DS    CL3                 CLTGRP ASSGN                                 
*                                                                               
CLOCK    DS    CL1                 CLIENT LOCK                                  
CMCLTCOD DS    XL2                 MASTER TRAFFIC CLIENT CODE                   
CMCLTUNQ DS    XL1                 MASTER TRAFFIC CLIENT UNIQUE SEQNUM          
CMCLTPRD DS    XL1                 MASTER TRAFFIC CLIENT PRODUCT CODE           
*                                                                               
CACCAGY  DS    CL2                 ACC AGENCY OVERRIDE                          
CPOLONLY DS    CL1                 POL BUYING ONLY                              
*                                                                               
CCLTINTR DS    CL2       CLT       CLIENT INTERFACE                             
CEXTRA   DS    CL15      A/N       EXTRA PROFILE                                
CTITLE   DS    CL10                ID TITLE                                     
*                                                                               
CPU1     DS    CL20                PRODUCT USER FIELD DESC 1                    
CPU1TYPE DS    CL1                 PRODUCT USER TYPE (A/C/N)                    
CPU1LEN  DS    XL1                 PRODUCT USER LENGTH (MAX32)                  
CPU1FLG1 DS    XL1                                                              
CFLGREQQ EQU   X'80'               X'80' = REQUIRED                             
CFLGA2Q  EQU   X'40'               X'40' = SHOW ON A2                           
CFLGNIBQ EQU   X'20'               X'20' = (NET) INTEG BILLS                    
CFLGSPQ  EQU   X'10'               X'10' = (SPOT) SHOW ON BILLS                 
CFLGNTBQ EQU   X'10'               X'10' = (NET) TIME BILLS                     
CFLGMXQ  EQU   X'08'               X'08' = TRANSFER ON MX                       
CFLGNSBQ EQU   X'04'               X'04' = (NET) SPEC CHARGE BILLS              
*                                                                               
CPU1FLG2 DS    XL1                                                              
CUSERLNQ EQU   *-CPU1                                                           
*                                                                               
CPU2     DS    CL20                PRODUCT USER FIELD DESC 2                    
CPU2TYPE DS    CL1                 PRODUCT USER TYPE (A/C/N)                    
CPU2LEN  DS    XL1                 PRODUCT USER LENGTH (MAX16)                  
CPU2FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CPU2FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
*                                                                               
CEU1     DS    CL20                ESTIMATE USER FIELD DESC 1                   
CEU1TYPE DS    CL1                 ESTIMATE USER TYPE (A/C/N)                   
CEU1LEN  DS    XL1                 ESTIMATE USER LENGTH (MAX32)                 
CEU1FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CEU1FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
*                                                                               
CEU2     DS    CL20                ESTIMATE USER FIELD DESC 2                   
CEU2TYPE DS    CL1                 ESTIMATE USER TYPE (A/C/N)                   
CEU2LEN  DS    XL1                 ESTIMATE USER LENGTH (MAX16)                 
CEU2FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CEU2FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
CULNQ    EQU   *-CPU1                                                           
*                                                                               
CMEDNAME DS    CL10                MEDIA NAME OVERRIDE                          
CNETID   DS    CL4                 NETWORK ID                                   
COPT1    DS    XL1                                                              
COP1COSQ EQU   X'80'               SECOND COST REQUIRED                         
COP1INFQ EQU   X'40'               INFOMERCIAL                                  
COP1DBLQ EQU   X'20'               DO NOT TEST DOUBLE-BOOKING                   
COP1MGRQ EQU   X'10'               REQUIRE MGREQ REC IF ID=MKTGRP               
COP1NMG  EQU   X'08'               CLIENT USES NEW MAKEGOODS                    
COP1CTAQ EQU   X'04'               CONTRACT ANALYSIS (CTA) CLIENT               
COPT2    DS    XL1                                                              
         DS    CL2                 SPARE                                        
*                                                                               
CPST     DS    CL10                PST CODES                                    
CDAILY   DS    CL1                 ESTIMATES WILL BE DAILY                      
CPWPCT   DS    XL3                 PROFIT WITHIN PERCENTAGE                     
CZENCLT  DS    CL3                 ZENITH CLIENT CODE                           
         DS    149C                ** NEW SPARE **                              
CLTHDRL  EQU   *-CLTHDR                                                         
         SPACE 1                                                                
* CONTENTS OF CPROF:                CONTENTS OF CEXTRA:                         
*   1 - BRAND/POL TRNDS               1 - CANADIAN DEMO OPTION                  
*   2 - LOCK BOX NUMBER               2 - CANADIAN NETWORK TAX                  
*   3 - MKT/STA TRNDS                 3 - BUY ID REQUIRED                       
*   4 - RATING SERVICE                4 - ESTIMATE FILTERS REQ                  
*   5 - BILL FORMULA CNTRL            5 - CAMPAIGNS                             
*   6 - BILL ESTIMATE CNTRL           6 - U.S. SPILL                            
*   7 - PRINT CLT CODE AS AAN         7 - 'EST=NO' EST NAME                     
*   8 - PRINT EST SERIES NM           8 - MKGDS IN MISSED MTH                   
*   9 - GOALS CPP OVERRIDE            9 - GOAL REQD FOR BUY                     
*   10- PROGRAM ADJ. CNTRL            10- COUNTRY                               
*   11- POL TIMESHEET DEMOS           11- OUT-OF-WEEK CLIENT                    
*   12- FORCE EST SERIES REQ          12- GST CODE                              
*   13- PRD REQ FOR TRUE POL          13- SPECIAL DEMO ADJ.                     
*   14- EXCLUSION GROUP CODE          14- PRD REQD FOR ADDS SEND                
*   15- CLIENT RATE CNTRL             15- RATE COVERAGE CONTROL (NET)           
*                                                                               
*        SPOT PRODUCT DSECT                                                     
*                                                                               
SPPRDD   DSECT                                                                  
*          DATA SET SPGENPRD   AT LEVEL 014 AS OF 05/11/93                      
*              PRODUCT HEADER RECORD                                            
         SPACE 1                                                                
PRDHDR   DS    0C                                                               
PKEY     DS    0CL13     V         KEY                                          
PKEYTYPE DS    CL1       B         RECORD TYPE X'00'                            
PKEYAM   DS    CL1       A/M       AGENCY/MEDIA                                 
PKEYCLT  DS    CL2       CLT       CLIENT CODE                                  
PKEYPRD  DS    CL3       A         PRODUCT CODE                                 
PKEYZRO  DS    CL6       B         BINARY ZEROS                                 
         SPACE 2                                                                
PLEN     DS    CL2       B         RECORD LENGTH (240)                          
PCNTRL   DS    CL1       B         CONTROL BYTE                                 
PLINKS   DS    CL4       B         LINK FIELDS                                  
         DS    CL4       B         SPARE                                        
         SPACE 2                                                                
PACCT    DS    CL4       A/N       ACCOUNT NUMBER                               
PNAME    DS    CL20      A         PRODUCT NAME                                 
PCODE    DS    CL2       B         PRODUCT CODE                                 
PADDR1   DS    CL30      A/N       BILL ADDRESS LINE 1                          
PADDR2   DS    CL30      A/N       BILL ADDRESS LINE 2                          
PADDR3   DS    CL30      A/N       BILL ADDRESS LINE 3                          
PADDR4   DS    CL30      A/N       BILL ADDRESS LINE 4                          
PDIV     DS    CL3       A/N       DIVISION CODE                                
PBILLDT  DS    CL2       B         EFFECTIVE Y/M OF SERVICE                     
PBILLBAS DS    CL1       B         2 4-BIT FIELDS - BILL BASE/COMM BASE         
*                                  B'0000' = GROSS, B'0001' = NET               
PBILLCOM DS    CL4       B         SIGNED COMMISSION (99.9999)                  
PAGYFEE  DS    CL2       P         OTHER AGENCY FEE (2 IMPLIED DEC)             
PPROF    DS    CL30      A/N       PROFILE                                      
PGRP1    DS    CL3                 PRDGRP ASSGN                                 
PGRP2    DS    CL3                 PRDGRP ASSGN                                 
PGRP3    DS    CL3                 PRDGRP ASSGN                                 
PCLASS   DS    CL1                 PRODUCT CLASS                                
PGRP4    DS    CL3                 PRDGRP ASSGN                                 
PGRP5    DS    CL3                 PRDGRP ASSGN                                 
PLOCK    DS    CL1                 PRD LOCKED                                   
PLKDAT   DS    CL2                 PRD LOCK ACTV DATE  (COMPRESSD)              
PGSTCODE DS    CL1                 GOODS AND SERVICE TAX                        
         DS    CL8                 SPARE                                        
PUSER1   DS    XL32                USER FIELD 1                                 
PUSER2   DS    XL16                USER FIELD 2                                 
PPST     DS    CL10                PST CODES                                    
PTALAGY  DS    CL6                 TALENT AGENCY                                
         DS    CL32                SPARE                                        
PRDHDRL  EQU   *-PRDHDR                                                         
*                                                                               
*        SPOT ESTIMATE DSECT                                                    
*                                                                               
SPESTD   DSECT                                                                  
*          DATA SET SPGENEST   AT LEVEL 004 AS OF 05/19/94                      
*              ESTIMATE HEADER RECORD                                           
         SPACE 2                                                                
ESTHDR   DS    0C                                                               
EKEY     DS    0CL13     V         KEY                                          
EKEYTYPE DS    CL1       B         RECORD TYPE X'00'                            
EKEYAM   DS    CL1       A/M       AGENCY/MEDIA                                 
EKEYCLT  DS    CL2       CLT       CLIENT CODE                                  
EKEYPRD  DS    CL3       A         PRODUCT CODE                                 
EKEYEST  DS    CL1       B         ESTIMATE NUMBER                              
         DS    CL5       B                                                      
*                                                                               
ELEN     DS    CL2       B         RECORD LENGTH (500)                          
ECNTRL   DS    CL1       B         CONTROL BYTE                                 
ELINKS   DS    CL4       B         LINK FIELD                                   
ECURPDN  DS    CL4                 NET PAID TODAY BUCKET                        
*                                                                               
EDESC    DS    CL20      A         ESTIMATE DESCRIPTION                         
EPRDCD   DS    CL2                 PRODUCT CODE                                 
ENEWNET  EQU   X'80'               IN EPRDCD FOR NEW NETPAK EST                 
ERECCTR  DS    CL2                 RECORD COUNTER                               
EDAYMENU DS    CL1       N         DAYPART MENU NUMBER                          
EMSTRIND DS    CL1       A         M=MASTER EST  S=SUB-EST, ELSE 0              
EMSTREST DS    CL1       B         FOR SUB-EST, MASTER ESTIMATE NUMBER          
EHUTADJ  DS    CL1       B         BITS 0-3=FIRST MONTH AVERAGE                 
*                                       4-7=LAST (0=USE 1ST ONLY)               
*                                       ALL ZERO=ADJUST ON BUY PERIOD           
ESTART   DS    CL6       N         START DATE (YYMMDD)                          
EEND     DS    CL6       N         END DATE (YYMMDD)                            
ERATECST DS    CL1                 COST FIELDS AFFECTED BY RATE T=ACT           
EDAILY   DS    CL1                 DAILY EST INIDICATOR                         
         DS    CL30                SPARE                                        
EORDN    DS    CL104     B         13 ORDERED YTD, 13 ORDERED CURRENT           
EPAIDN   DS    CL104     B         13 PAID YTD, 13 PAID CURRENT                 
*                                                                               
EBOOK    DS    CL2       B         BOOK FOR DEFAULT (YEAR-MONTH)                
EDEMOS   DS    CL126     V         14 9-BYTE DEMOGRAPHIC FIELDS                 
*        DS    CL1       B         DEMO NUMBER                                  
*        DS    CL1       B         WEIGHTING FACTOR                             
*        DC    CL7       A/N       DEMO DESCRIPTION                             
*                                                                               
* FIELDS BELOW ARE FOR NEW DEMO LOOK-UP CONVERSION                              
*                                                                               
         ORG   EDEMOS                                                           
EDEMLST  DS    XL60                (20 DEMOS X 3 BYTES=SPARE/TYPE/NUM)          
EWGTLST  DS    XL20                (20 X 1 BYTE = WEIGHTS)                      
EUSRNMS  DS    CL28                (4 X 7 BYTES = USER DEMO NAMES)              
EWGTNM   DS    CL7                 WEIGHTED DEMO NAME                           
ETRGLST  DS    XL6                 DEMO TARGETS FOR ALLOCATION                  
ETDMIN   DS    XL2                 TARGET DEMO MINIMUM VALUE - 1 DEC.           
ECONTROL DS    XL1                 ESTIMATE CONTROL BYTE                        
EBILESTQ EQU   X'80'                 BILL PERIOD = ESTIMATE PERIOD              
ENSEPCMQ EQU   X'40'                 NO SEPARATE COMM BILLING FOR THIS          
*                                    EST (OVERRIDES B2B PROFILE)                
EOWSDAY  DS    XL1                 OUT OF WEEK START DAY                        
ERATE    DS    XL1                 RATE TYPE (AS IN CPROF+14)                   
*                                  * = NO RATE TYPE (OVERRIDES CLIENT)          
*                                                                               
ENET     DS    CL48      B                                                      
ECOPY    DS    CL1       A         FILM COPY ALPHA CODE                         
EREP     DS    CL2       B         SPECIAL REP CODE                             
EPROF    DS    0CL8      A/N       PROFILE                                      
         DS    CL3       A/N       FILTERS                                      
ECPPCLT  DS    CL2        B        CPP EST CLT                                  
ECPPEST  DS    CL1        B        CPP EST                                      
EREQLO   DS    CL1                 REQUEST RANGE - LOW                          
EREQHI   DS    CL1                 REQUEST RANGE - HIGH                         
*                                  ABOVE FIELDS MAY BE C'NO' OR 2X'00'          
EBILLBAS DS    CL1       B         2 4-BIT FIELDS - BILL BASE/COMM BASE         
*                                  B'0000'=GROSS  B'0001'=NET                   
EBILLCOM DS    XL4       B         SIGNED COMMISSION RATE (99,9999)             
ECONVIND DS    0CL1      B         X'FF'=JWT CONVERSION (FEB4/74)               
ETYPE    DS    CL1        A/N      EST TYPE C=CUT OFF                           
*                                  1=W$,2=WPCT,3=M$,4=MPCT,5=Q$                 
ETACOPY  DS    CL1       B         X'FF'=DUPLICATE TURN-AROUNDS                 
EMGDTE   DS    CL2       B         LATEST MAKEGOOD DATE                         
EAUTHN   DS    CL52      B         13 MONTHLY AUTHORIZED COUNTERS               
ERTLSCHM DS    CL2       C         RETAIL SCHEME CODE                           
EUSER1   DS    XL32                USER FIELD 1                                 
EUSER2   DS    XL16                USER FIELD 2                                 
EPWPCT   DS    XL3                 PROFIT WITHIN PERCENT (WESTERN)              
EFLAG1   DS    XL1                                                              
EF1REQ   EQU   X'80'               SF JWT REQ=Y FLAG                            
EF1NMG   EQU   X'40'               NEW MAKEGOODS                                
EGRANT   DS    CL34                SPACE -- YES !!!!                            
ESTHDRLN EQU   *-ESTHDR                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT FILE ++INCLUDES AND DSECTS                                    *         
***********************************************************************         
         SPACE 1                                                                
*        PRINT CLIENT DSECT                                                     
*                                                                               
PRCLID   DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
*                                                                               
*        PRINT PRODUCT DSECT                                                    
*                                                                               
PRPRDD   DSECT                                                                  
*          DATA SET PPRDREC    AT LEVEL 024 AS OF 06/01/93                      
PPRDREC  DS    0C .                *** - PRODUCT RECORD ***                     
***                                                                             
***      WARNING DO NOT LET RECORD LENGTH EXCEED 330 BYTES                      
***      PPNEWFILE CURRENTLY ALLOWS FOR 330 BYTES                               
***      CURRENT MAXIMUM RECORD LENGTH COULD BE 300 BYTES                       
***      WHEN ALL OPTIONAL ELEMENTS ARE PRESENT                                 
***                                                                             
PPRDKEY  DS    0CL25                                                            
PPRDKAGY DS    CL2 .     A         AGENCY CODE                                  
PPRDKMED DS    CL1 .     A         PRINT MEDIA CODE                             
PPRDKRCD DS    X'06' .   B         RECORD CODE                                  
PPRDKCLT DS    CL3 .     A         CLIENT CODE                                  
PPRDKPRD DS    CL3 .     A         PRODUCT CODE                                 
         DS    15X'00'                                                          
*                                                                               
PPRDLEN  DS    CL2 .     B         RECORD LENGTH                                
*                                                                               
PPRDCNTL DS    CL2 .     B         CONTROL BYTES                                
         DS    CL4 .     B         DISK ADDRESS FOR LINKED RECORDS              
*                                                                               
PPRDELEM DS    0CL200                                                           
         DS    X'06' .   B         ELEMENT CODE                                 
         DS    X'C8' .   B         ELEMENT LENGTH                               
PPRDNAME DS    CL20 .    AN        PRODUCT NAME                                 
PPRDBILL DS    CL20 .    AN        BILL RECEIPT NAME                            
PPRDLIN1 DS    CL30 .    AN        ADDRESS - LINE 1                             
PPRDLIN2 DS    CL30 .    AN        ADDRESS - LINE 2                             
PPRDATTN DS    CL24 .    AN        ATTENTION                                    
PPRDDIV  DS    CL3 .     N         DIVISION CODE                                
PPRDACCT DS    CL4 .     AN        ACCOUNT NUMBER                               
*                        IF FIRST POS = X'FF' THEN NEXT 3 ARE BINARY            
*                        FOR JWT PRODUCT INTERFACE NUMBER                       
PPRDBILP DS    CL37 .    AN        BILLING PROFILE                              
PPRDEXCL DS    CL3 .     A         EXCLUSION CODE                               
PPRDOAN  DS    CL2 .     A         OAN (OTHER AGENCY NAME) AGENCY CODE          
PPRDBIL2 DS    CL20 .    A/N       BILL RECEIPT NAME  - LINE 2                  
PPRDGST  DS    CL1       A         CANADIAN GST TAX CODE                        
*                                  X'00' OR C'S',C'X',C'Z'                      
         DS    CL04 .              SPARE                                        
*                                                                               
PPRDUDEF DS    0CL50                                                            
         DS    X'08'     B         ELEMENT CODE                                 
         DS    X'32'     B         ELEMENT LENGTH                               
PPUSER1  DS    XL32      A         USER DESCRIPTION 1                           
PPUSER2  DS    XL16      A         USER DESCRIPTION 2                           
*                                                                               
PPRDPST  DS    0CL12                                                            
         DS    X'25'     B         ELEMENT CODE                                 
         DS    X'0C'     B         ELEMENT LENGTH                               
PPRDPSTC DS    CL10      A         PST CODES                                    
*                                                                               
*        PRINT ESTIMATE DSECT                                                   
*                                                                               
PRESTD   DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
         SPACE 1                                                                
* DDCNTRL                                                                       
*                                                                               
       ++INCLUDE DDCNTRL                                                        
*                                                                               
* DMWRKRK                                                                       
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACREPZG02A10/18/00'                                      
         END                                                                    
