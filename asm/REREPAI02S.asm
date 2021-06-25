*          DATA SET REREPAI02S AT LEVEL 109 AS OF 12/09/98                      
*PHASE REAI02A,*                                                                
*INCLUDE RECUP                                                                  
         TITLE 'REREPAI02 - ALTERNATE CALENDAR INITIALIZER'                     
*********************************************************************           
*                                                                   *           
*        REREPAI02 --- ALTERNATE CALENDAR INITIALIZER               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* DEC02/97 (JRD) --- INITIAL ENTRY:                                 *           
*                                                                   *           
* FEB06/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* MAR13/98 (JRD) --- ADD OPTION TO BLOW AWAY OLD 53 ELEMENTS        *           
*                     QOPTION1 = Y                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
REAI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REAI02,R7,R8,R9,RR=RE                                        
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
         EJECT                                                                  
* ----------------------------------------------------------------- *           
*        CHECK AND PROCESS MODE SETTINGS                                        
* ----------------------------------------------------------------- *           
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
* ----------------------------------------------------------------- *           
*  COMMON ROUTINES                                                              
* ----------------------------------------------------------------- *           
EXITOK   DS    0H                                                               
         CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   DS    0H                                                               
         LTR   RB,RB                                                            
         B     EXIT                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* ----------------------------------------------------------------- *           
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
* ----------------------------------------------------------------- *           
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)  REQUEST A CONTRACT                    
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(PROCCONT),AL3(POST)    POST ROUTINE                          
         DC    AL1(REQLAST),AL3(RPTDONE)  END OF REPORT                         
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
***********************************************************************         
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
***********************************************************************         
INITIAL  NTR1                                                                   
         ZAP   PROCCTR,=P'0'                                                    
         ZAP   READCTR,=P'0'                                                    
         MVI   MISCFLG1,0                                                       
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTO1 LOADER,DMCB,=CL8'T00AAC',0                                       
         MVC   VREPFACS,4(R1)                                                   
*                                                                               
         L     RE,ADCONLST                                                      
         L     RE,VCOMFACS-ADCONSD(RE)                                          
         ST    RE,ACOMFACS                                                      
*                                                                               
         L     RE,=V(RECUP)                                                     
         A     RE,RELO                                                          
         ST    RE,VRECUP                                                        
         B     EXITOK                                                           
***********************************************************************         
*   POST: READ EACH BUY RECORD AND GENERATE ALTERNATE CALENDAR                  
* ESITMATE BUCKETS AS REQUIRED USING THE MONDAY DATE OF THE BUY                 
* CREATION DATE AS THE BUCKET ACTICITY DATE.                                    
***********************************************************************         
POST     NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         AP    READCTR,=P'1'                                                    
*                                                                               
         TM    MISCFLG1,MF1STACH   STATION READ?                                
         BNZ   POST020             YES                                          
         OI    MISCFLG1,MF1STACH                                                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,RCONKREP                                              
         MVC   K.RSTAKSTA,RCONKSTA                                              
         CLI   K.RSTAKSTA+4,C'T'                                                
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                REQUIRED RECORD                              
*                                                                               
         LA    RE,RBUYREC                                                       
         ST    RE,AIOAREA                                                       
         GOTO1 GREC                                                             
*                                                                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'08'        EXTENDED DESCRIPTION ELEMENT?                
         BAS   RE,GETEL                                                         
         BNE   POST010                                                          
*                                                                               
         TM    RSTAOPTA-RSTAXXEL(R6),X'20'       USES ALT CALENDAR?             
         BNZ   POST020                           YES                            
*                                                                               
POST010  DS    0H                  STATION NOT FLAGGED FOR ALT. CAL.            
         MVC   P(L'XSTANALT),XSTANALT                                           
         MVC   P+8(5),RCONKSTA                                                  
         GOTO1 REPORT                                                           
         B     POSTX                                                            
*                                                                               
POST020  DS    0H                                                               
         TM    RCONMODR+1,X'20'    MON DATA ADDED?                              
         BZ    POST0022                                                         
*                                  YES - COPY BUCKETS THEN CONTINUE             
         MVC   P(L'XCONMON),XCONMON                                             
         GOTO1 HEXOUT,DMCB,RCONKCON,P+9,4                                       
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 COPYBUCS,DMCB,(X'03',RCONREC)                                    
         GOTO1 COPYBUCS,DMCB,(X'04',RCONREC)                                    
*                                                                               
         ZAP   BUYCTR,=P'0'                                                     
         B     POST102                                                          
*                                                                               
POST0022 DS    0H                                                               
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,RCONREC),ACOMFACS                    
         MVC   BUCKFLGS,0(R1)                                                   
         TM    BUCKFLGS,X'40'+X'20'      PREVIOUS ALT BUCKETS?                  
         BZ    POST040                   NO                                     
*                                                                               
         CLI   QOPTION1,C'Y'             REQUEST TO RESET ALL BUCKETS?          
         BNE   POST030                   NO                                     
*                                                                               
         NI    BUCKFLGS,X'FF'-(X'40'+X'20')                                     
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A4'        ALTERNATE CALENDAR ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),(R6)                                  
*                                                                               
         LA    R6,RCONELEM                                                      
         MVI   ELCODE,X'53'        ALTERNATE CALENDAR BUCKET                    
POST024  DS    0H                        PRINT CONTRACT ERROR                   
         BAS   RE,FIRSTEL                                                       
         BNE   POST028                                                          
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),(R6)                                  
         B     POST024                                                          
*                                                                               
POST028  DS    0H                        PRINT CONTRACT ERROR                   
         B     POST040                                                          
*                                                                               
POST030  DS    0H                        PRINT CONTRACT ERROR                   
         MVC   P(L'XCONBUC),XCONBUC                                             
         GOTO1 HEXOUT,DMCB,RCONKCON,P+9,4                                       
         GOTO1 REPORT                                                           
         B     POSTX                                                            
*                                                                               
POST040  DS    0H                                                               
         XC    KEY,KEY             READ BUY RECORDS                             
K        USING RBUYKEY,KEY                                                      
         MVI   K.RBUYKTYP,X'0B'                                                 
         MVC   K.RBUYKREP,RCONKREP                                              
         GOTOX (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(3,K.RBUYKCON)             
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         ZAP   BUYCTR,=P'0'                                                     
*                                                                               
POST050  DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   POST100             NO MORE BUYS ON THIS CONTRACT                
*                                                                               
         AP    BUYCTR,=P'1'                                                     
         LA    RE,RBUYREC                                                       
         ST    RE,AIOAREA                                                       
         GOTO1 GREC                                                             
*                                  DO BUCKET UPDATE                             
         BAS   RE,BUCKUP                                                        
         BE    POST060                                                          
*                                  PRINT BUY ERROR                              
         MVC   P(L'XBUYOUT),XBUYOUT                                             
         EDIT  RBUYKLIN,(3,P+8),ZERO=NOBLANK                                    
         GOTO1 HEXOUT,DMCB,RCONKCON,P+12,4                                      
         GOTO1 REPORT                                                           
         B     POSTX                                                            
*                                                                               
POST060  DS    0H                                                               
         MVC   KEY(L'RBUYKEY),RBUYREC                                           
         GOTO1 HIGH                RE-ESTABLISH BUY SEQUENCE                    
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                HUH?                                         
*                                                                               
         GOTO1 SEQ                 NEXT BUY                                     
         B     POST050                                                          
*                                                                               
POST100  DS    0H                                                               
         CP    BUYCTR,=P'0'                                                     
         BH    POST102                                                          
*                                                                               
         MVC   P(L'XCONNBUY),XCONNBUY                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+9,4                                       
         GOTO1 REPORT                                                           
         B     POSTX                                                            
*                                                                               
POST102  DS    0H                                                               
         AP    PROCCTR,=P'1'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RCONKEY),RCONKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                HUH?                                         
*                                                                               
         LA    RE,RBUYREC          READ SOMEWHERE                               
         ST    RE,AIOAREA                                                       
         GOTO1 GREC                                                             
*                                                                               
         LA    RE,RCONREC          WRITE NEW RECORD OUT                         
         ST    RE,AIOAREA                                                       
*                                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    POST110             YES                                          
*                                                                               
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   POST110             NO                                           
*                                                                               
         GOTO1 PREC                                                             
*                                                                               
POST110  DS    0H                                                               
         CLC   =C'$$XTRACT',QUESTOR                                             
         BNE   POST112                                                          
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(L'PRECONM),PRECONM                                             
         GOTO1 HEXOUT,DMCB,RCONKCON,P+9,4                                       
         GOTO1 REPORT                                                           
         GOTO1 PRNTREC,RBUYREC                                                  
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P(L'POSTCONM),POSTCONM                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+9,4                                       
         EDIT  BUYCTR,(3,P+35),ZERO=NOBLANK                                     
         GOTO1 REPORT                                                           
         GOTO1 PRNTREC,RCONREC                                                  
POST112  DS    0H                                                               
*                                                                               
POSTX    DS    0H                                                               
         XC    KEY,KEY             RESTORE CONTRACT SEQUENCE                    
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY,SAVEKEY                                                      
         BE    *+6                                                              
         DC    H'0'                HUH?                                         
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REQUEST COMPLETED PRINT SOME DETAILS                                          
***********************************************************************         
RPTDONE  NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(22),=CL22'CONTRACTS READ:'                                     
         EDIT  READCTR,(17,P+24),ZERO=NOBLANK                                   
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(22),=CL22'CONTRACTS PROCESSED:'                                
         EDIT  PROCCTR,(17,P+24),ZERO=NOBLANK                                   
         GOTO1 REPORT                                                           
*                                                                               
         B     EXITOK                                                           
***********************************************************************         
* BUCKUP:                                                                       
*                                                                               
*    OUTPUT:  CC EQUAL & UPDATED CONTRACT & BUY OR PLAN RECORDS                 
*                            OR                                                 
*             CC NOT EQUAL                                                      
*                                                                               
***********************************************************************         
BUCKUP   NTR1                                                                   
         LA    R2,RBUYREC          A(BUYREC)                                    
         MVC   WORK(4),GETBROAD                          A(GETBROAD)            
         MVC   WORK+4(4),GETDAY                          A(GETDAY)              
         MVC   WORK+8(4),ADDAY                           A(ADDAY)               
         MVC   WORK+12(4),DATCON                         A(DATCON)              
         MVC   WORK+16(4),DATAMGR                        A(DATAMGR)             
         MVC   WORK+20(4),VRECUP                         A(RECUP)               
*                                                                               
* BUILD BUCKETS ALTERNATE EST BUCKETS                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,RCONKREP                                                    
         GOTOX (RFGENBUC,VREPFACS),DMCB,(R2),(X'FF',BUCKETS),WORK,     +        
               (R0),RCONKSTA                                                    
         BNE   EXITNE                                                           
*                                                                               
* ADD ALTERNATE CALENDAR ELEMENT TO CONREC                                      
*                                                                               
         XC    WORK2(RCONAXLQ),WORK2                                            
E        USING RCONAXEL,WORK2                                                   
         MVI   E.RCONAXCO,X'A4'                                                 
         MVI   E.RCONAXLN,RCONAXLQ                                              
         TM    BUCKETS,X'80'       STATION RECORD USED?                         
         BNO   *+12                NO                                           
         OI    E.RCONAXFL,X'40'                                                 
         B     *+8                                                              
         OI    E.RCONAXFL,X'80'    THEN ITS REP LEVEL                           
*                                                                               
         OI    E.RCONAXFL,X'01'    SET WAS INITIALIZED                          
         GOTO1 DATCON,DMCB,(5,0),(19,E.RCONAXID)                                
         DROP  E                                                                
*                                                                               
         LA    R5,RCONELEM                                                      
BUCU014  DS    0H                                                               
         CLI   0(R5),0             END OF RECORD?                               
         BE    BUCU016             YES - ADD ELEMENT                            
         CLI   0(R5),X'A4'         ALT CALENDAR ELEM?                           
         BE    BUCU018             YES - NOTHING TO ADD                         
         BH    BUCU016             ADD ELEMENT HERE                             
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     BUCU014                                                          
*                                                                               
BUCU016  DS    0H                                                               
         GOTO1 VRECUP,DMCB,(C'R',RCONREC),WORK2,(R5)                            
*                                                                               
* ADD BUCKETS TO CONREC                                                         
*                                                                               
BUCU018  DS    0H                                                               
         NI    BUCKETS,X'FF'-X'80'  TURN OFF STATION FLAG                       
         CLC   BUCKETS(2),=H'2'     NONE?                                       
         BE    BUCU022                                                          
*                                                                               
         MVC   HALF,BUCKETS                                                     
         LH    R5,HALF             LEN OF BUCKETS                               
         LA    R5,BUCKETS-1(R5)                                                 
         LA    R3,BUCKETS+2         1ST BUCKET                                  
*                                                                               
* GET MONDAY DATE OF OF BUY CREATION                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYCREA),(0,DUB)                                 
         OC    RBUYCREA,RBUYCREA   IF NO DATE USE CONDATE                       
         BNZ   BUCU019                                                          
         GOTO1 DATCON,DMCB,(3,RCONCREA),(0,DUB)                                 
*                                                                               
BUCU019  GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),=3C' '      FIND DAY OF WEEK                             
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R6,R6                                                            
         IC    R6,DMCB             DAY OF WEEK                                  
         BCTR  R6,R0                                                            
         LNR   R6,R6               BACK UP TO MONDAY                            
*                                  GET CURRENT MONDAY                           
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R6)                                      
*                                  GET 2-BYTE CURRENT MONDAY-WEEK DATE          
         GOTO1 DATCON,DMCB,DMCB+12,(2,DATE)                                     
*                                                                               
* ADD BUCKET TO CONREC (OR SUBTRACT)                                            
*     USE MONDAY DATE OF BUY CREATION AS WEEK COMMENCING DATE                   
*                                                                               
BUCU020  MVI   1(R3),10            K BUCKET LENGTH                              
         MVC   HALF,RCONBKWK-RCONBKEL(R3)                                       
         MVC   RCONBKWK-RCONBKEL(2,R3),DATE                                     
         GOTOX (RFADDBUC,VREPFACS),DMCB,RCONREC,(R3),VRECUP                     
         MVI   1(R3),14            RESTORE LENGTH                               
         MVC   RCONBKWK-RCONBKEL(2,R3),HALF                                     
         ZIC   R4,1(R3)                                                         
         BXLE  R3,R4,BUCU020       NEXT BUCKET                                  
*                                                                               
BUCU022  DS    0H                                                               
         B     EXITOK                                                           
         DROP  R7                                                               
***********************************************************************         
* COPY BUCKETS TO ALTERNATE FORM                                                
*                                                                               
* INPUT:   P1 BYTE 1 - BUCKET TYPE TO COPY                                      
*                          X'03' WILL BECOME X'53'                              
*                          X'04' WILL BECOME X'54'                              
*             BYTE 2-4 A(CONTRACT)                                              
*                                                                               
***********************************************************************         
COPYBUCS NTR1                                                                   
         MVC   ELCODE,0(R1)                                                     
         SR    R4,R4                                                            
         ICM   R4,7,1(R1)                                                       
*                                                                               
         MVI   ELCODE2,X'53'                                                    
         CLI   ELCODE,X'03'                                                     
         BE    CPYBUC02                                                         
*                                                                               
         MVI   ELCODE2,X'54'                                                    
         CLI   ELCODE,X'04'                                                     
         BE    CPYBUC02                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
CPYBUC02 DS    0H                                                               
         LA    R6,RCONELEM-RCONREC(R4)                                          
         LR    R5,R6                                                            
         BAS   RE,FRSTEL2          ALREADY HAS ALTERNATES?                      
         BE    CPYBUCX             YES - NOTHING TO DO                          
*                                                                               
         LR    R5,R6               RESET TO BEGINING OF CONREC                  
         BAS   RE,FIRSTEL          BUCKETS?                                     
         BNE   CPYBUCX             NO - NOTHING TO DO                           
*                                                                               
CPYBUC04 DS    0H                  FIND WHERE TO ADD ALT. ELEMENTS              
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         CLC   ELCODE2,0(R5)                                                    
         BL    CPYBUC10                                                         
         CLI   0(R5),0                                                          
         BNE   CPYBUC04                                                         
*                                                                               
CPYBUC10 DS    0H                                                               
         XC    WORK2,WORK2                                                      
         ZIC   RE,1(R6)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),0(R6)      COPY BUCKET                                  
         MVC   WORK2(1),ELCODE2    RESET ELEMENT CODE                           
*                                                                               
         GOTO1 VRECUP,DMCB,(C'R',(R4)),WORK2,(R5)                               
         ZIC   RE,1(R5)            BUMP PAST NEW ELEMENT                        
         AR    R5,RE                                                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    CPYBUC10                                                         
*                                                                               
CPYBUCX  DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* PRINT A REP RECORD FOR DEBUGGING                                              
***********************************************************************         
PRNTREC  NTR1                                                                   
         LR    R6,R1                                                            
         MVC   P(12),=C'RECORD DUMP:'                                           
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R6),P+2,34                                         
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,34(R6)           FIRST ELEMENT                                
PRNTR10  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    PRNTR20                                                          
*                                                                               
         ZIC   R2,1(R6)                                                         
         LA    R4,0(R6)                                                         
         LA    R0,P+5                                                           
PRNTR12  DS    0H                                                               
         LR    R3,R2                                                            
         CH    R3,=H'60'                                                        
         BNH   *+8                                                              
         LA    R3,60                                                            
*                                                                               
         SR    R2,R3                                                            
         GOTO1 HEXOUT,DMCB,(R4),(R0),(R3)                                       
         GOTO1 REPORT                                                           
*                                                                               
         AR    R4,R3                                                            
         LA    R0,P+10                                                          
         LTR   R2,R2                                                            
         BNZ   PRNTR12                                                          
*                                                                               
         ZIC   R2,1(R6)                                                         
         LA    R6,0(R2,R6)                                                      
         B     PRNTR10                                                          
*                                                                               
PRNTR20  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         GETEL  R6,=Y(RCONELEM-RCONREC),ELCODE                                  
         GETELN R5,=Y(RCONELEM-RCONREC),ELCODE2,2                               
         EJECT                                                                  
*********************************************************************           
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*********************************************************************           
XCONMON  DC    C'CONTRACT XXXXXXXX HAS MON DATA ADDED'                          
XCONBUC  DC    C'CONTRACT XXXXXXXX ALREADY HAS ALTERNATE BUCKETS'               
XCONNBUY DC    C'CONTRACT XXXXXXXX HAS NO BUYS'                                 
XSTANALT DC    C'STATION XXXXX IS NOT SET TO USE ALTERNATE CALENDARS'           
XBUYOUT  DC    C'BUYLINE XXX/         IS NOT COVERED BY CALENDARS'              
PRECONM  DC    C'CONTRACT XXXXXXXX PRE RECORD'                                  
POSTCONM DC    C'CONTRACT XXXXXXXX POST RECORD WITH XXX BUYS'                   
*********************************************************************           
* NON RE-ENTRENT WORKING STORAGE                                                
*********************************************************************           
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
AIOAREA  DS    A                                                                
VREPFACS DS    A                                                                
ACOMFACS DS    A                                                                
VRECUP   DS    A                                                                
MTRACDM8 DS    C                                                                
DATE     DS    CL2                                                              
BUCKFLGS DS    X                                                                
*                                                                               
ELCODE   DS    X                                                                
ELCODE2  DS    X                                                                
*                                                                               
MISCFLG1 DS    X                                                                
MF1STACH EQU   X'80'                - STATION FLAG WAS CHECKED                  
*                                                                               
READCTR  DC    PL6'0'              CONTRACTS READ      CTR                      
PROCCTR  DC    PL6'0'              CONTRACTS PROCESSED CTR                      
BUYCTR   DC    PL6'0'              BUYS ON A CONTRACT                           
COMMAND  DS    CL8                                                              
SAVEKEY  DS    XL(L'KEY)                                                        
WORK2    DS    XL256                                                            
BUCKETS  DS    CL400                                                            
FILLER   DS    XL6000                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* REGENALL1                                                                     
* REREPWORKD                                                                    
* REREPMODES                                                                    
* REXADDRD                                                                      
* COMFACSD                                                                      
* REPFACSQ                                                                      
*****    PRINT OFF                                                              
       ++INCLUDE REGENALL1A                                                     
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REPFACSQ                                                       
         PRINT ON                                                               
***********************************************************************         
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                                                              
BRDEND   DS    XL3                                                              
BRDWEEKS DS    XL1                                                              
BRDLEN   EQU   *-BRDSTART                                                       
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109REREPAI02S12/09/98'                                      
         END                                                                    
