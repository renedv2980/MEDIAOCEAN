*          DATA SET ACLFM24    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T60324A,+0                                                               
         TITLE 'ADJUST HOURS'                                                   
*INCLUDE PERVERT                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE ACSLRY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
T60324   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (LWSX-LWS),*LFM24**,R8,RR=R2,CLEAR=YES                           
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
*                                                                               
         ST    R2,RELO                                                          
         EJECT                                                                  
*                                                                               
*        CONTROL MODE SETTINGS                                                  
*                                                                               
         CLI   MODE,BUILDKEY                                                    
         BE    BLDKEY                                                           
         CLI   MODE,DSPLYREC                                                    
         BE    DSPREC                                                           
         CLI   MODE,BUILDREC                                                    
         BE    BLDREC                                                           
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              BUILD KEY ROUTINE                                                
BLDKEY   LA    R2,MARACCTH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   KEY(15),SPACES              MOVE COMPANY CODES TO KEY.           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),WORK                                                   
         TM    4(R2),X'20'                 HAS DATA PREVIOUSLY BEEN             
         BO    BLDK7                       VALIDATED - IF SO BRANCH.            
*                                                                               
         MVC   MARACNM,SPACES              CLEAR ACCOUNT                        
         OI    MARACNMH+6,X'80'            AND                                  
         MVC   MARCTNM,SPACES              CONTRA ACCOUNT LABELS.               
         OI    MARCTNMH+6,X'80'                                                 
*                                                                               
         NI    MARCONTH+4,X'DF'            SET VALIDITY BIT IN CONTRA           
         NI    MARPERDH+4,X'DF'            AND PERIOD OFF.                      
*                                                                               
         MVI   ANYKEY,C'Y'                 SET ANY KEY CHANGES TO YES.          
*                                                                               
*                                                                               
         GOTO1 READ                        READ ACCOUNT NAME.                   
         MVC   ACCNT,KEY                   SAVE ACCOUNT CODE                    
         MVI   ERROR,NOTVLREC              SET ERROR MSG.                       
*                                                                               
         LA    R4,IO                       TEST                                 
         USING ACKEYD,R4                   TO                                   
         LA    R5,ACRECORD                 SEE                                  
BLDK3    CLI   0(R5),X'00'                 IF                                   
         BE    XIT                         RECORD                               
         CLI   0(R5),X'32'                 HAS                                  
         BE    BLDK5                       32                                   
         ZIC   R0,1(R5)                    ELEMENT.                             
         AR    R5,R0                                                            
         B     BLDK3                                                            
*                                                                               
BLDK5    GOTO1 NAMOUT                      TRANSMIT ACCOUNT NAME.               
         OI    4(R2),X'20'                 TURN ON VALIDITY BIT.                
         SPACE 1                                                                
*              VALIDATE CONTRA ACCOUNT                                          
BLDK7    LA    R2,MARCONTH                                                      
         GOTO1 ANY                         ANY DATA ENTERED?                    
         GOTO1 MOVE                                                             
         TM    4(R2),X'20'                 HAS CONTRA BEEN PREVIOUSLY           
         BO    BLDK13                      VALIDATED - IF SO BRANCH.            
*                                                                               
         MVC   MARCTNM,SPACES              CLEAR CONTRA LABEL.                  
         OI    MARCTNMH+6,X'80'                                                 
*                                                                               
         MVI   ANYKEY,C'Y'                 SET ANY KEY CHANGES TO YES.          
         NI    MARPERDH+4,X'DF'            TURN OFF VALIDITY BIT IN             
*                                          PERIOD.                              
         MVC   KEY(15),SPACES                                                   
         MVC   KEY(1),COMPANY              MOVE COMPANY CODES TO KEY.           
         MVC   KEY+1(14),WORK                                                   
*                                                                               
         GOTO1 READ                        READ CONTRA NAME AND                 
         MVC   CNTRA,KEY                   SAVE CONTRA                          
         MVI   ERROR,NOTVLREC              SET ERROR MSG.                       
*                                                                               
         LA    R4,IO                       TEST                                 
         USING ACKEYD,R4                   TO                                   
         LA    R5,ACRECORD                 CHECK                                
BLDK9    CLI   0(R5),X'00'                 IF                                   
         BE    XIT                         RECORD                               
         CLI   0(R5),X'32'                 CONTAINS                             
         BE    BLDK11                      32                                   
         ZIC   R0,1(R5)                    ELEMENT.                             
         AR    R5,R0                                                            
         B     BLDK9                                                            
*                                                                               
BLDK11   GOTO1 NAMOUT                      TRANSMIT CONTRA NAME.                
         OI    4(R2),X'20'                 TURN ON VALIDITY BIT.                
         SPACE 1                                                                
*        VALIDATE PERIOD                                                        
BLDK13   LA    R2,MARPERDH                                                      
         TM    4(R2),X'20'                 HAS PERIOD BEEN PREVIOUSLY           
         BO    BLDK20                      VALIDATED - IF SO BRANCH.            
*                                                                               
         GOTO1 ANY                         ANY DATA ENTERED?                    
         GOTO1 MOVE                                                             
*                                                                               
         MVI   ANYKEY,C'Y'                 SET ANY KEY CHANGES TO YES.          
*                                                                               
         MVI   ERROR,DATERR                                                     
         GOTO1 =V(SCANNER),DMCB,MARPERDH,(2,BLOCK),C',=-=',RR=RELO              
*                                                                               
*                                        SEPARATES PERIOD TO 2 BLOCKS.          
*                                                                               
         ZIC   R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BNZ   BLDK14                    BAD DATE.                              
         BAS   RE,CLRSCN CLEAR SCREEN.                                          
         B     XIT                                                              
         SPACE 1                                                                
BLDK14   GOTO1 DATCON,DMCB,(5,0),(1,TODAY)         GET TODAYS DATE.             
         LA    R5,BLOCK                                                         
         LA    R3,START                                                         
         XC    END,END                                                          
*                                                                               
BLDK15   CLI   1(R5),X'00'                 IF NOT ZERO - ERROR.                 
         BE    BLDK17                                                           
         BAS   RE,CLRSCN                                                        
         B     XIT                                                              
BLDK17   CLI   0(R5),6                  IF DATE LENGTH IS ZERO - ERROR.         
         BNH   BLDK18                                                           
         BAS   RE,CLRSCN                                                        
         B     XIT                                                              
*                                                                               
BLDK18   GOTO1 DATVAL,DMCB,(0,12(R5)),WORK     IF INPUT IS YY/MM/DD             
         OC    0(4,R1),0(R1)                   ERROR IN INPUT.                  
         BZ    BLDK19                                                           
         BAS   RE,CLRSCN                                                        
         B     XIT                                                              
*                                                                               
BLDK19   GOTO1 DATVAL,DMCB,(2,12(R5)),WORK     ONLY FORMAT IS MMM/YY            
         OC    0(4,R1),0(R1)                                                    
         BNZ   BLDK21                                                           
         BAS   RE,CLRSCN                                                        
         B     XIT                                                              
*                                                                               
BLDK21   GOTO1 DATCON,DMCB,(0,WORK),(1,0(R3))                                   
         CLC   0(2,R3),TODAY              DATE INPUT CAN'T BE                   
         BNH   BLDK22                     HIGHER THAN TODAY'S.                  
         BAS   RE,CLRSCN                  CLEAR SCREEN.                         
         B     XIT                                                              
*                                                                               
*                                                                               
BLDK22   LA    R3,3(R3)                   BUMP TO NEXT INPUT DATE.              
         LA    R5,32(R5)                  BUMP TO NEXT DATE IN BLOCK.           
         BCT   R6,BLDK15                                                        
*                                                                               
         OC    END,END                                                          
         BNZ   *+10                                                             
         MVC   END,START           IF NO END USE START                          
         CLC   END,START           START MUST NOT BE LESS THAN END.             
         BNL   BLDK23                                                           
         BAS   RE,CLRSCN                                                        
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
*                                                                               
BLDK23   GOTO1 DATCON,DMCB,(1,START),(0,WORK)                                   
         GOTO1 DATCON,DMCB,(1,END),(0,WORK+6)                                   
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6,RR=RELO                             
*                                          DETERMINE NUMBER OF MONTHS           
*                                          BETWEEN DATES.                       
         MVC   NUMMNTS,DMCB+14             SAVE NUMBER OF MONTHS.               
         CLC   NUMMNTS,=H'12'              IS MONTHS BETWEEN > 12               
         BNH   BLDK20                      IF SO - ERROR.                       
         BAS   RE,CLRSCN                                                        
         B     XIT                                                              
*                                                                               
         SPACE 1                                                                
BLDK20   OI    4(R2),X'20'                 TURN ON VALIDITY BIT.                
         CLI   ANYKEY,C'Y'                                                      
         BNE   XIT                                                              
         BAS   RE,INITAB           INITIALIZE TABLE.                            
         BAS   RE,CLRSCN           CLEAR SCREEN.                                
         BAS   RE,FILLUP           FILL UP TABLE.                               
         MVI   ERROR,X'FF'         EVERYTHING LOOKS OK.                         
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 1                                                                
DSPREC   DC    0H'0'                                                            
         BAS   RE,CLRSCN                     CLEAR SCREEN.                      
         BAS   RE,DISPLAY                    DISPLAY TABLE.                     
         LA    R2,MARACCTH                                                      
         CLC   LOGACT(3),=C'INQ'             IS ACTION INQUIRY.                 
         BE    XIT                           IF YES BRANCH TO EXIT.             
         LA    R5,MARDAT1H                                                      
         USING DATLIN,R5                                                        
         LA    R2,MARHRSH                    BRING CURSOR TO FIRST              
         MVI   ERROR,X'FF'                                                      
         B     XIT                           HOURS FIELD.                       
         SPACE 2                                                                
*              BUILD RECORD                                                     
         SPACE 1                                                                
BLDREC   DC    0H'0'                                                            
         BAS   RE,BLDR                         VALIDATE INPUT.                  
         CLI   ERROR,X'FF'                     NO ERRORS?                       
         BNE   XIT                             IF ERRORS BRANCH.                
         BAS   RE,UPREC                        UPDATE THE  RECORD.              
         BAS   RE,CLRSCN                       CLEAR THE SCREEN.                
         BAS   RE,DISPLAY                      DISPLAY THE TABLE.               
         LA    R2,MARACCTH                     MOVE CURSOR TO ACCOUNT           
         MVI   ERROR,X'FF'                                     FIELD.           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILL IN THE TABLE                                     
         SPACE 1                                                                
         USING ACKEYD,R5                                                        
FILLUP   NTR1                                                                   
         LA    R5,IO                                                            
         MVC   ACKEYACC(42),SPACES     GET PREVIOUS ADJUSTMENT RECORD.          
         MVC   ACKEYACC,ACCNT                                                   
         MVC   ACKEYCON,CNTRA                                                   
         MVI   ACKEYREF,C'A'                                                    
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO,IO                 
         CLI   DMCB+8,0                IF ZERO, 'A' RECORD FOUND.               
         BNE   FILLUP10                ELSE BRANCH TO READ HOURS.               
         MVI   READSW,C'N'                                                      
         LH    R0,NUMMNTS                                                       
         LA    R4,TABC                                                          
         USING FILLD,R4                                                         
FILLUP3  GOTO1 GETEL,DMCB,(X'45',(R5)),(2,FILMNTH)                              
         CLI   ELERR,0                IF ZERO, ELEMENT FOUND.                   
         BNE   FILLUP5                ELSE BRANCH TO READ HOURS.                
         L     R3,ELADDR                                                        
         USING TRHISTD,R3                                                       
         ZAP   FILACHR,TRHSDR         DEBITS ARE TOTAL HOURS.                   
         ZAP   FILCNHR,TRHSCR         CREDITS ARE CLIENT HOURS.                 
         OI    FILSTAT,X'80'          TURN ON ADJUST ELEMENT FOUND.             
         CP    FILCNHR,=P'0'          IS CLIENT HRS FROM ADJUST ZERO.           
         BNE   FILLUP4                                                          
         ZAP   FILCST,=P'0'           IF SO COST AND                            
         ZAP   FILPCT,=P'0'           % ARE ALSO ZERO.                          
         B     FILLUP6                                                          
FILLUP4  ZAP   PK13,FILCNHR                                                     
         MP    PK13,=P'100000'                                                  
         DP    PK13,FILACHR           CLIENT HRS/TOTAL HRS                      
         SRP   PK13(9),63,5                                                     
         ZAP   FILPCT,PK13(9)         EQUALS PERCENT.                           
         ZAP   PK13,FILSAL                                                      
         MP    PK13,FILPCT            SALARY X PERCENT                          
         SRP   PK13,60,5              EQUALS                                    
         ZAP   FILCST,PK13            COST.                                     
         B     FILLUP6                                                          
         SPACE 1                                                                
FILLUP5  MVI   READSW,C'Y'            IF A 45 ELEMENT NOT FOUND.                
FILLUP6  LA    R4,FILEN(R4)           NEXT ITEM IN TABLE.                       
         BCT   R0,FILLUP3                                                       
         CLI   READSW,C'N'                                                      
         BE    XIT                    IF ALL 45 ELEMENTS FOUND.                 
         SPACE 1                                                                
*              NOW READ HOURS RECORDS                                           
FILLUP10 MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC,ACCNT                                                   
FILLUP11 MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO,IO                 
         B     FILLUP13                                                         
FILLUP12 MVC   COMMAND,=C'DMRSEQ'                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO,IO                 
FILLUP13 CLC   ACKEYACC,ACCNT                                                   
         BNE   FILLUP20                 END OF ACCOUNT.                         
         SPACE 1                                                                
         GOTO1 GETEL,DMCB,(X'45',(R5)),0                                        
         CLI   ELERR,0                                                          
         BNE   FILLUP16                                                         
         CLI   ACKEYREF,C'H'                                                    
         BNE   FILLUP12                                                         
         CLC   ACKEYCON+1(2),=C'1C'     FROM UNIT 1 LEDGER C.                   
         BNE   FILLUP12                                                         
         BAS   RE,POSTEM                POST HISTORIES.                         
         B     FILLUP12                                                         
FILLUP16 CLI   ACRECORD,X'44'           IS THIS TRANSACTION REC.                
         BNE   FILLUP12                                                         
         MVC   ACKEYDTE,=X'FFFFFF'      SKIP TRANSACTIONS.                      
         B     FILLUP11                                                         
         SPACE 1                                                                
*              GET PERCENT FOR THIS CLIENT                                      
         USING FILLD,R4                                                         
FILLUP20 LH    R0,NUMMNTS                                                       
         LA    R4,TABC                                                          
         SPACE 1                                                                
FILLUP22 CP    FILACHR,=P'0'       IF ACCT HRS IS ZERO, CLIENT HRS              
         BE    FILLUP26            IS ALSO ZERO.                                
         TM    FILSTAT,X'80'       WAS % FOUND IN ADJUST?                       
         BO    FILLUP26            IF YES NEXT TABLE ENTRY.                     
         ZAP   PK13,FILCNHR        THIS CLIENT HOURS.                           
         MP    PK13,=P'10000'                                                   
         DP    PK13,FILACHR        CLIENT HOURS/ACCOUNT HOURS                   
         ZAP   FILPCT,PK13(9)      EQUALS PERCENT.                              
FILLUP23 ZAP   PK13,FILSAL                                                      
         MP    PK13,FILPCT         SALARY X PERCENT                             
         SRP   PK13,60,5           EQUALS                                       
         ZAP   FILCST,PK13         COST.                                        
FILLUP26 LA    R4,FILEN(R4)        NEXT ITEM IN TABLE.                          
         BCT   R0,FILLUP22                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST 45'S TO TABC                                     
         SPACE 1                                                                
POSTEM   NTR1                                                                   
         LH    R0,NUMMNTS                                                       
         LA    R4,TABC                                                          
         USING FILLD,R4                                                         
POSTEM3  TM    FILSTAT,X'80'       IF % NOT EQUAL TO ZERO, ADJUSTMENT           
         BO    POSTEM5             RECORD FOUND.                                
         GOTO1 GETEL,DMCB,(X'45',(R5)),(2,FILMNTH)                              
         CLI   ELERR,0             IF ZERO, ELEMENT FOUND.                      
         BNE   POSTEM5             ELSE BRANCH.                                 
         L     R3,ELADDR                                                        
         USING TRHISTD,R3                                                       
         AP    FILACHR,TRHSCR      ACCUMULATE HRS.WORKED ON ANY CLIENT.         
         SP    FILACHR,TRHSDR      SUBTRACT ANY DEBITS.                         
         LA    R5,IO                                                            
         USING ACKEYD,R5                                                        
         CLC   ACKEYCON,CNTRA      IS CONTRA ACCOUNT IS ONE SPECIFIED           
         BNE   POSTEM5             IF NOT BRANCH AROUND                         
         AP    FILCNHR,TRHSCR      ELSE ADD HOURS TO CLIENT HOURS.              
         SP    FILCNHR,TRHSDR      SUBTRACT ANY DEBITS.                         
         SPACE 1                                                                
POSTEM5  LA    R4,FILEN(R4)        NEXT ITEM IN TABLE.                          
         BCT   R0,POSTEM3                                                       
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              ROUTINE TO INITIALIZE TABLE                                      
         SPACE 1                                                                
         USING FILLD,R4                                                         
INITAB   NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY,ACCNT                                                        
         GOTO1 READ                                                             
         LA    R4,TABC                                                          
         LH    R0,NUMMNTS                                                       
         GOTO1 DATCON,DMCB,(1,START),(0,CDATE)                                  
INITAB2  ZAP   FILSAL,=P'0'                    INITIALIZE                       
         ZAP   FILACHR,=P'0'                   TABLE                            
         ZAP   FILCNHR,=P'0'                   VALUES                           
         ZAP   FILPCT,=P'0'                    TO                               
         ZAP   FILCST,=P'0'                    ZERO.                            
         MVI   FILSTAT,X'00'                                                    
         GOTO1 DATCON,DMCB,(0,CDATE),(1,WORK)                                   
         MVC   FILMNTH,WORK                    PUT DATE TO TABLE.               
         GOTO1 =V(ADDAY),DMCB,CDATE,WORK,32,RR=RELO    UP TO NEXT               
         MVC   CDATE(4),WORK                           MONTH.                   
         MVC   CDATE+4(2),=C'01'                                                
         SPACE 1                                                                
         MVC   STEND(2),FILMNTH           USE SAME START AND END DATE           
         MVC   STEND+2(2),FILMNTH         WHEN GOING TO ACSLRY.                 
*                                                                               
         LA    R5,IO                                                            
*                                                                               
         L     RF,=V(ACSLRY)                                                    
         GOTO1 (RF),DMCB,(X'80',(R5)),STEND,WRKAREA,COMFACS,RR=RELO             
*                                                                               
         LA    R3,WRKAREA                                                       
         USING SLRD,R3                                                          
         ZAP   FILSAL,SLRTOT              MOVE SALARY TO TABLE.                 
         LA    R4,FILEN(R4)               UP TO NEXT TABLE ENTRY.               
         BCT   R0,INITAB2                                                       
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              ROUTINE TO CLEAR SCREEN                                          
         USING DATLIN,R5                                                        
CLRSCN   NTR1                                                                   
         LA    R0,12                                                            
         LA    R5,MARDAT1H                                                      
CLRSCN1  MVC   MARDAT,SPACES               LOOP TO CLEAR OUT 12                 
         OI    MARDATH+6,X'80'                  DATE                            
         MVC   MARSAL,SPACES                    SALARY                          
         OI    MARSALH+6,X'80'                                                  
         MVC   MARHRS,SPACES                    HOURS                           
         OI    MARHRSH+6,X'80'                                                  
         MVC   MARPCT,SPACES                    PERCENT                         
         OI    MARPCTH+6,X'80'                                                  
         MVC   MARCST,SPACES                    COST FIELDS.                    
         OI    MARCSTH+6,X'80'                                                  
*                                                                               
         LA    R5,DLLEN(R5)                BUMP TO NEXT LINE ON SCREEN.         
         BCT   R0,CLRSCN1                                                       
*                                                                               
         MVC   MARTSAL,SPACES              CLEAR SALARY TOTAL FIELD             
         OI    MARTSALH+6,X'80'                                                 
         MVC   MARTHRS,SPACES                    HOURS  TOTAL FIELD             
         OI    MARTHRSH+6,X'80'                                                 
         MVC   MARTCST,SPACES                    COST   TOTAL FIELD.            
         OI    MARTCSTH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY TABLE ENTRIES                                 
         SPACE 1                                                                
         USING DATLIN,R5                                                        
         USING FILLD,R4                                                         
DISPLAY  NTR1                                                                   
         LH    R3,NUMMNTS                                                       
         LA    R5,MARDAT1H                                                      
         LA    R4,TABC                                                          
         ZAP   TOTSAL,=P'0'                     INITIALIZE TOTAL FIELDS         
         ZAP   TOTHRS,=P'0'                     TO ZERO.                        
         ZAP   TOTCST,=P'0'                                                     
TABOUT   MVC   WORK(2),FILMNTH                                                  
         MVI   WORK+2,1                           PUT TO SCREEN:                
         GOTO1 DATCON,DMCB,(1,WORK),(9,MARDAT)             DATE.                
         EDIT  FILSAL,(10,MARSAL),2,COMMAS=YES             SALARY.              
         EDIT  FILCNHR,(7,MARHRS),2,ALIGN=LEFT,MINUS=YES   CLIENT HRS.          
         OI    MARHRSH+4,X'20'                                                  
         EDIT  FILPCT,(6,MARPCT),2,ALIGN=LEFT,MINUS=YES    PERCENT.             
         OI    MARPCTH+4,X'20'                                                  
         EDIT  FILCST,(10,MARCST),2,COMMAS=YES,MINUS=YES   COST.                
         AP    TOTSAL,FILSAL                         ACCUMULATE SALARY          
         AP    TOTHRS,FILCNHR                        CLIENT HOURS               
         AP    TOTCST,FILCST                         COST.                      
         LA    R5,DLLEN(R5)                                                     
         LA    R4,FILEN(R4)                                                     
         BCT   R3,TABOUT                                                        
         EDIT  TOTSAL,(13,MARTSAL),2,COMMAS=YES,FLOAT=$,MINUS=YES               
         EDIT  TOTHRS,(9,MARTHRS),2,ALIGN=LEFT,MINUS=YES                        
         EDIT  TOTCST,(13,MARTCST),2,COMMAS=YES,FLOAT=$,MINUS=YES               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE INPUT FIELDS                                 
         USING DATLIN,R5                                                        
         USING FILLD,R4                                                         
BLDR     NTR1                                                                   
         LA    R5,MARDAT1H                                                      
         LA    R4,TABC                                                          
         LH    R0,NUMMNTS                                                       
*                                                                               
BLD1     TM    MARPCTH+4,X'20'             HAS PERCENT BEEN AMENDED.            
         BO    BLD8                                                             
         LA    R2,MARPCTH                  IF SO VALIDATE.                      
         GOTO1 VALICASH                                                         
         ZAP   DUB1,DUB                                                         
*                                                                               
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(34),=C'PERCENT CANNOT BE GREATER THAN 100'               
         MVI   ERROR,X'FE'                                                      
         CP    DUB1,=P'10000'              IF NEW PERCENT GREATER THAN          
         BH    XIT                         100 NO CALCULATIONS.                 
         CP    DUB1,=P'0'                  IS PERCENT ZERO                      
         BNE   BLD3                        IF SO SET TO ZERO                    
         ZAP   FILPCT,=P'0'                PERCENT                              
         ZAP   FILCST,=P'0'                COST                                 
         ZAP   FILCNHR,=P'0'               CLIENT HOURS.                        
         B     BLD13                                                            
*                                                                               
BLD3     CP    FILPCT,=P'0'                IF OLD % = 0, OLD CLIENT             
         BNE   BLD5                        HRS = 0.                             
         CP    FILACHR,=P'0'               IF ACCT HRS = 0                      
         BNE   BLD5                                                             
         LA    R2,MARHRSH                  NEED ENTRY IN CLIENT HRS             
         GOTO1 VALICASH                                                         
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(30),=C'MUST ENTER CORRESPONDING HOURS'                   
         MVI   ERROR,X'FE'                                                      
         CP    DUB,=P'0'                   DOES AMT ON SCREEN = 0               
         BE    XIT                                                              
         B     BLD5                                                             
*                                                                               
BLD5     ZAP   FILPCT,DUB1                 MOVE NEW PERCENT TO TABLE.           
         ZAP   PK13,FILSAL                                                      
         MP    PK13,FILPCT                 SALARY X NEW PERCENT                 
         SRP   PK13,60,5                   EQUALS                               
         ZAP   FILCST,PK13                 NEW COST.                            
*                                                                               
         TM    MARHRSH+4,X'20'             HAS CLIENT HOURS ALSO BEEN           
         BO    BLD7                        AMENDED.                             
         LA    R2,MARHRSH                  IF YES VALIDATE.                     
         GOTO1 VALICASH                                                         
         CP    DUB,=P'0'                                                        
         BE    BLD9                                                             
         ZAP   FILCNHR,DUB                 NEW CLIENT HOURS TO TABLE.           
         ZAP   PK13,FILCNHR                                                     
         MP    PK13,=P'10000'                                                   
         DP    PK13,FILPCT                 CLIENT HOURS/PERCENT                 
         ZAP   FILACHR,PK13(10)            EQUAL NEW TOTAL HOURS.               
         B     BLD13                       BRANCH TO NEXT TABLE ENTRY.          
BLD7     ZAP   PK13,FILACHR                IF HOURS HAS NOT BEEN AMNDED         
         MP    PK13,FILPCT                 TOTAL HOURS X PERCENT                
         SRP   PK13,60,5                   EQUALS                               
         ZAP   FILCNHR,PK13                NEW CLIENT HOURS.                    
         B     BLD13                                                            
*                                                                               
BLD8     TM    MARHRSH+4,X'20'             CLIENT HOURS BEEN AMENDED.           
         BO    BLD13                       IF NOT BRANCH.                       
         LA    R2,MARHRSH                  IF SO VALIDATE.                      
         GOTO1 VALICASH                                                         
         ZAP   DUB1,DUB                                                         
*                                                                               
         CP    DUB1,=P'0'                  IF CLIENT HRS ENTERED =              
         BNE   BLD10                       ZERO SET TO ZERO                     
BLD9     ZAP   FILCNHR,=P'0'               CLIENT HOURS                         
         ZAP   FILPCT,=P'0'                PERCENT                              
         ZAP   FILCST,=P'0'                COST.                                
         B     BLD13                                                            
*                                                                               
BLD10    CP    FILCNHR,=P'0'               IF OLD CLIENT HRS = 0,               
         BNE   BLD11                       OLD % = 0.                           
         CP    FILACHR,=P'0'               IF ACCT HRS = 0                      
         BNE   BLD11                                                            
         LA    R2,MARPCTH                  NEED ENTRY IN %.                     
         GOTO1 VALICASH                                                         
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(32),=C'MUST ENTER CORRESPONDING PERCENT'                 
         MVI   ERROR,X'FE'                                                      
         CP    DUB,=P'0'                   DOES % ON SCREEN = 0.                
         BE    XIT                                                              
         B     BLD11                                                            
*                                                                               
BLD11    MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(30),=C'HOURS GREATER THAN TOTAL HOURS'                   
         MVI   ERROR,X'FE'                                                      
         CP    FILACHR,DUB1                IF CLIENT HRS > ACCT HRS             
         BL    XIT                         ERROR.                               
         ZAP   FILCNHR,DUB1                CLIENT HOURS TO TABLE.               
         ZAP   PK13,FILCNHR                                                     
         MP    PK13,=P'10000'                                                   
         DP    PK13,FILACHR                CLIENT HRS/ACCT HRS                  
         ZAP   FILPCT,PK13(9)              EQUALS PERCENT.                      
         ZAP   PK13,FILSAL                                                      
         MP    PK13,FILPCT                 SALARY X NEW PERCENT                 
         SRP   PK13,60,5                   = COST.                              
         ZAP   FILCST,PK13                                                      
BLD13    LA    R5,DLLEN(R5)                UP TO NEXT TABLE ENTRY.              
         LA    R4,FILEN(R4)                                                     
         BCT   R0,BLD1                                                          
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO UPDATE RECORDS                                        
         USING TRHISTD,R3                                                       
         USING FILLD,R4                                                         
         USING ACKEYD,R5                                                        
UPREC    NTR1                                                                   
         MVI   READSW,C'W'                  SET READSWITCH TO WRITE.            
         LH    R0,NUMMNTS                                                       
         LA    R3,ELEMENT                                                       
         LA    R4,TABC                                                          
         LA    R5,IO                                                            
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC,ACCNT                                                   
         MVC   ACKEYCON,CNTRA                                                   
         MVI   ACKEYREF,C'A'                                                    
         MVC   COMMAND,=C'DMREAD'          READ TO FIND ADJUSTMENT REC.         
         OI    DMINBTS,X'80'                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO,IO                 
         CLI   DMCB+8,0                    IF ZERO, ADJ. REC. FOUND.            
         BE    UPREC5                      BRANCH AROUND.                       
         MVI   READSW,C'A'                 ELSE SET READSWITCH TO ADD.          
         MVC   ACKEYACC(42),SPACES              SET KEY.                        
         MVC   ACKEYACC,ACCNT                                                   
         MVC   ACKEYCON,CNTRA                                                   
         MVI   ACKEYREF,C'A'                                                    
         XC    ACLENGTH(10),ACLENGTH                                            
UPREC5   XC    ELEMENT,ELEMENT             BUILD ELEMENT LIST:                  
         MVI   TRHSEL,X'45'                        CODE                         
         MVI   TRHSLEN,X'10'                       LENGTH                       
         MVC   TRHSYEAR(2),FILMNTH                 DATE FROM TABLE              
         ZAP   TRHSDR,FILACHR                      DEBITS = TOT HRS             
         ZAP   TRHSCR,FILCNHR                      CREDITS =CLIENT HRS          
         CLI   READSW,C'A'                    IF READSW IS ADD RECORD           
         BE    UPREC8                         BRANCH AROUND DELETE EL.          
         GOTO1 DELEL,DMCB,(X'45',(R5)),(2,FILMNTH)   DELETE OLD ELEMENT         
UPREC8   GOTO1 ADDEL,DMCB,(R5),(R3)                  ADD NEW ELEMENT            
         LA    R4,FILEN(R4)                    UP TO NEXT TABLE ENTRY.          
         BCT   R0,UPREC5                                                        
         GOTO1 DELEL,DMCB,(X'43',(R5)),0       DELETE OLD 43 ELEMENT            
         SPACE 1                                                                
         LA    R3,ELEMENT                                                       
         USING TRSUBHD,R3                                                       
         XC    ELEMENT,ELEMENT                 BUILD 43 ELEMENT                 
         MVC   TRSBEL(2),=X'4300'                                               
         LA    RE,IO                                                            
         MVC   TRSBACNT,ACKEYCON-ACKEYD(RE)                                     
         OC    TRSBACNT,SPACES                                                  
         OC    MARCTNM,SPACES                                                   
         MVC   TRSBNAME,SPACES                                                  
         MVI   TRSBLEN,18                                                       
         CLC   TRSBACNT,SPACES                                                  
         BE    UPREC9                                                           
         LA    R1,MARCTNM+L'MARCTNM-1                                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,MARCTNM                                                       
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRSBNAME(0),MARCTNM                                              
         AH    R1,=H'18'                                                        
         STC   R1,TRSBLEN                                                       
UPREC9   GOTO1 ADDEL,DMCB,(R5),(R3)                                             
         MVC   COMMAND,=C'DMADD'           IF READSW IS ADD, ADD                
         CLI   READSW,C'A'                 RECORD, OTHERWISE WRITE              
         BE    *+10                        OVER OLD RECORD.                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',IO,IO                 
         NI    DMINBTS,X'7F'                                                    
         B     XIT                                                              
         SPACE 2                                                                
         MVI   ERROR,X'FF'                                                      
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 =V(HELLO),ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),        *        
               ((R5),(R3)),RR=RELO                                              
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 =V(HELLO),ELIST,(C'D',=C'ACCOUNT '),((R4),(R2)),        *        
               ((R5),(R3)),RR=RELO                                              
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 =V(HELLO),ELIST,(C'P',=C'ACCOUNT '),(R2),(R3),RR=RELO            
         CLI   ELIST+12,0                                                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
RELO     DS    F                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
*              DSECT FOR LOCAL WORKING STORAGE                                  
         SPACE 1                                                                
LWSD     DSECT                                                                  
LWS      DS    0C                                                               
STEND    DS    CL4                 YYMM-YYMM.                                   
WRKAREA  DS    CL200               WORK AREA FOR ACSLRY.                        
TODAY    DS    CL3                 TODAY'S DATE FROM COMPUTER.                  
PDATE    DS    CL3                 YYMM(PWOS)                                   
CDATE    DS    CL6                 YYMMDD                                       
PK13     DS    PL13                                                             
DUB1     DS    D                                                                
         SPACE 1                                                                
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
         SPACE 1                                                                
LWSX     EQU   *                                                                
*                                                                               
DATLIN   DSECT                             DSECT TO COVER MARDAT1.              
MARDATH  DS    CL8                         (DATE,SALARY,HOURS,                  
MARDAT   DS    CL6                          PERCENT,COST)                       
MARSALH  DS    CL8                                                              
MARSAL   DS    CL10                                                             
MARHRSH  DS    CL8                                                              
MARHRS   DS    CL7                                                              
MARPCTH  DS    CL8                                                              
MARPCT   DS    CL6                                                              
MARCSTH  DS    CL8                                                              
MARCST   DS    CL10                                                             
DLLEN    EQU   *-DATLIN                                                         
*                                                                               
*                                                                               
FILLD    DSECT                        DSECT TO SAVE 12 ENTRIES OF               
FILMNTH  DS    PL2                    MONTH                                     
FILSAL   DS    PL4                    SALARY                                    
FILACHR  DS    PL4                    ALL HOURS                                 
FILCNHR  DS    PL4                    CLIENT HOURS                              
FILPCT   DS    PL3                    PERCENT                                   
FILCST   DS    PL4                    COST                                      
FILSTAT  DS    CL1                    STATUS (ADJUST OR HISTORY)                
FILEN    EQU   *-FILLD                                                          
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMDFD                                                       
*                                                                               
START    DS    CL3                                                              
END      DS    CL3                                                              
NUMMNTS  DS    H                                                                
READSW   DS    CL1                                                              
TOTSAL   DS    PL6                                                              
TOTHRS   DS    PL6                                                              
TOTCST   DS    PL6                                                              
ACCNT    DS    CL15                ACCOUNT                                      
CNTRA    DS    CL15                CONTRA ACCOUNT                               
TABC     DS    (12*FILEN)C                                                      
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE DDSLRD                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACLFM24   05/01/02'                                      
         END                                                                    
