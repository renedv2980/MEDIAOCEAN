*          DATA SET REREPSP02  AT LEVEL 070 AS OF 11/01/05                      
*PHASE RESP02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREPSP02 (RESP02) --- REP SPOT S/P CONVERSION'                 
*                                                                               
***********************************************************************         
*                                                                     *         
*        REREPSP02K--- REP SPOT S/P     CONVERTED TO GENDIR/GENFIL    *         
*                      RECORDS.                                       *         
*                                                                     *         
*     QOPTION1  =  Y   GENERAL TRACE                                  *         
*     QOPTION2  =  Y   GENERAL TRACE WITH RECORD DUMPS                *         
*     QOFFICE   =  XX  SELECT B3 AND MS ONLY                          *         
*     QOFFICE   =  YY  SELECT LS ONLY                                 *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
* NOV12/02 (BU ) --- POINT PERSONS WITH NO OFFICE TO BE SKIPPED       *         
*                                                                     *         
* DEC02/02 (BU ) --- LUNA TEST AND MASTER TEST                        *         
*                                                                     *         
* OCT20/05 (BU ) --- ADD OFFICES TO REP RECORDS                       *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         PRINT NOGEN                                                            
RESP02   CSECT                                                                  
         NMOD1 MYWORKX-MYWORKD,**RESP**,R9,CLEAR=YES                            
         USING MYWORKD,RC                                                       
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         LA    RF,IOAREA           SET A(IOAREA)                                
         ST    RF,AIOAREA                                                       
*                                                                               
         CLI   MODE,REQFRST        RIGHT MODE?                                  
         BNE   MAINEXIT            NO - SO JUST EXIT                            
*                                                                               
         BAS   RE,INIT             DO THE INITIALIZATION STUFF                  
         CLI   QOPTION1,C'Y'       TRACE MODE                                   
         JNE   MAIN0100            NO                                           
*                                                                               
         MVC   P(14),=C'BACK FROM INIT'                                         
         GOTO1 REPORT                                                           
*                                                                               
MAIN0100 EQU   *                                                                
*                                                                               
         BAS   RE,READREPS         PROCESS REP RECORDS                          
*                                                                               
         BAS   RE,READSALS         READ THE S/P RECORDS                         
*                                                                               
         BAS   RE,READPPS          READ THE P/P RECORDS                         
*                                                                               
         MVC   P(18),=C'TOTAL REPS  READ: '                                     
         EDIT  TOTREPS,(5,P+32),ALIGN=LEFT                                      
         GOTO1 REPORT                                                           
         MVC   P(23),=C'TOTAL REP RECS WRITTEN:'                                
         EDIT  TOTRECS,(5,P+32),ALIGN=LEFT                                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(18),=C'TOTAL S/P   READ: '                                     
         EDIT  TOTSALS,(5,P+32),ALIGN=LEFT                                      
         GOTO1 REPORT                                                           
         MVC   P(23),=C'TOTAL S/P RECS SKIPPED:'                                
         EDIT  TOTSALL,(5,P+32),ALIGN=LEFT                                      
         GOTO1 REPORT                                                           
         MVC   P(23),=C'TOTAL S/P RECS WRITTEN:'                                
         EDIT  TOTSPS,(5,P+32),ALIGN=LEFT                                       
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(18),=C'TOTAL P/P   READ: '                                     
         EDIT  TOTPPS,(5,P+32),ALIGN=LEFT                                       
         GOTO1 REPORT                                                           
         MVC   P(23),=C'TOTAL P/P RECS SKIPPED:'                                
         EDIT  TOTPPL,(5,P+32),ALIGN=LEFT                                       
         GOTO1 REPORT                                                           
         MVC   P(23),=C'TOTAL P/P RECS WRITTEN:'                                
         EDIT  TOTPPWS,(5,P+32),ALIGN=LEFT                                      
         GOTO1 REPORT                                                           
*                                                                               
         CLOSE FILOUTA                                                          
*                                                                               
MAINEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
INIT     NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,WORK),(0,TODAY) GET TODAY'S DATE                  
*                                                                               
         CLC   =C'YY',QOFFICE      LIMIT PROCESSING?                            
         BNE   INIT0060            NO                                           
         MVC   P+1(24),=C'LUNA EXTRACT IN PROGRESS'                             
         GOTO1 REPORT                                                           
INIT0060 EQU   *                                                                
         OPEN  (FILOUTA,(OUTPUT))                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* THIS ROUTINE PROCESSES ALL REP RECORDS.  IF MASTER, SUB REP RECS              
*        ARE GENERATED.  IF NOT MASTER, RECORD IS SKIPPED.                      
*                                                                               
READREPS NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,1               SET KEY TYPE                                 
         GOTO1 HIGH                READ FIRST KEY                               
         B     RREP0040                                                         
RREP0020 EQU   *                                                                
         GOTO1 SEQ                 READ SEQUENTIAL                              
RREP0040 EQU   *                                                                
         CLI   KEY,1               REP RECORD?                                  
         BH    RREP0800            NO  - FINISHED                               
         CLC   =C'XX',QOFFICE      LIMIT PROCESSING?                            
         BNE   RREP0050            NO                                           
         CLC   =C'B3',KEY+25       YES - EJOR?                                  
         BE    RREP0060            YES - ACCEPT IT                              
         CLC   =C'MS',KEY+25       NO  - MASTER?                                
         BNE   RREP0020            NO  - SKIP IT                                
RREP0050 EQU   *                                                                
         CLC   =C'YY',QOFFICE      LIMIT PROCESSING?                            
         BNE   RREP0060            NO                                           
         CLC   =C'LS',KEY+25       YES - LUNA?                                  
         BNE   RREP0020            NO  - SKIP IT                                
RREP0060 EQU   *                                                                
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   RREP0080            NO - SO CONTINUE                             
*                                                                               
         MVC   P(16),=C'REP RECORD READ:'                                       
         MVC   P+16(27),KEY                                                     
         GOTO1 REPORT                                                           
*                                                                               
RREP0080 EQU   *                                                                
         GOTO1 GREC                RETRIEVE THE RECORD                          
         CLC   =C'NU',RREPKREP     CLEAR CHANNEL?                               
         BE    RREP0100            YES - SPECIAL PROCESSING                     
         CLC   =X'FFFF',RREPMAST   MASTER REP?                                  
         BNE   RREP0020            NO  - SKIP IT                                
*                                                                               
RREP0100 EQU   *                                                                
*                                                                               
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTREPS          GET TOTAL REP RECORDS READ                   
         LA    R5,1(R5)            INC COUNT OF REP RECORDS READ                
         STH   R5,TOTREPS          AND STORE IT                                 
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   RREP0120            NO - SO CONTINUE                             
*                                                                               
         MVC   P(13),=C'MASTER FOUND:'                                          
         MVC   P+13(27),RREPREC                                                 
         GOTO1 REPORT                                                           
*                                                                               
RREP0120 EQU   *                                                                
         BAS   RE,WRITEREP         GENERATE REP RECORDS                         
         B     RREP0020            GO BACK FOR NEXT                             
*                                                                               
RREP0800 EQU   *                                                                
         SR    R0,R0               FINISHED                                     
*                                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
* THIS ROUTINE GENERATES ONE RECORD FOR EACH SUBSIDIARY FOUND IN THE            
* MASTER REP RECORD.                                                            
*                                                                               
WRITEREP NTR1                                                                   
*                                                                               
         XCEF  REC,1000            CLEAR REC                                    
         LA    R7,REC+4                                                         
         USING GSPLRECD,R7                                                      
*                                                                               
*   THIS RECORD IS NOW A VARIABLE LENGTH RECORD, SO THESE                       
*        INITIAL SETTINGS DON'T HOLD UP.  WHERE NECESSARY,                      
*        THEY ARE RESET IN THE SUBSEQUENT CODE.                                 
*                                                                               
         LA    RF,GSPLSUBL         SET LENGTH OF DATA ONLY                      
         STCM  RF,3,GSPLFLEN       STORE LOW ORDER TWO BYTES                    
         LH    R6,GSPLFLEN         L(RECORD)                                    
         LA    R6,GSPLFRST(R6)     L(KEY+CNTL+DATA)                             
         STH   R6,GSPLFLEN         STORE L(RECORD)                              
         LA    R6,4(R6)            L(ENTIRE TAPE RECORD)                        
         STH   R6,REC              STORE IT                                     
*                                                                               
* BUILD THE KEY                                                                 
*                                                                               
         MVI   GSPLKTYP,X'71'      RECORD TYPE                                  
         MVI   GSPLKSTP,X'00'      RECORD SUB TYPE                              
         MVI   GSPLSBCD,1          SET ELEMENT CODE TYPE                        
         MVI   GSPLSBLN,GSPLSUBL   SET ELEMENT LENGTH                           
*                                                                               
         CLC   =C'NU',RREPKREP     CLEAR CHANNEL?                               
         BNE   WREP0010            NO                                           
         MVC   GSPLKREP,RREPKREP   YES - SPECIAL PROCESSING                     
         MVC   GSPLMREP,=C'K3'     FORCE KATZ RADIO GROUP AS MASTER             
         MVC   GSPLSBMR,RREPSHRT   INSERT SHORT REP MASTER NAME                 
         B     WREP0030                                                         
WREP0010 EQU   *                                                                
         LA    R3,RREPELEM         SET A(01 DESCRIP ELEMENT)                    
         ZIC   RF,1(R3)                                                         
         AR    R3,RF               BUMP TO X'02' ELEMENT                        
         CLI   0(R3),2             FOR MASTER, X'02' MUST BE PRESENT            
         USING RREPSUB,R3                                                       
         ZIC   R8,RREPSCNT         LOOP: NUMBER OF SUBSIDIARIES                 
         LA    R4,RREPSCOD         SET A(1ST SUB REP CODE)                      
WREP0020 EQU   *                                                                
         MVC   GSPLKREP,0(R4)      INSERT SUB REP INTO KEY                      
         MVC   GSPLMREP,RREPKREP   INSERT MASTER REP INTO KEY                   
         MVC   SAVEREP,0(R4)       SAVE VALUE FOR OFFICE WORK                   
         MVC   GSPLSBMR,RREPSHRT   INSERT SHORT REP MASTER NAME                 
*                                                                               
         GOTO1 =A(SETOFFCS)        SET OFFICES IN SUB                           
*                                                                               
         DROP  R7                                                               
WREP0030 EQU   *                                                                
*                                                                               
* NOW DUMP IT OUT JUST TO MAKE SURE WE'VE GOT IT WORKING                        
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WREP0040            NO - SO CONTINUE                             
*                                                                               
         MVC   P(15),=C'GOING TO PRNTBL'                                        
         GOTO1 REPORT                                                           
*                                                                               
WREP0040 EQU   *                                                                
*                                                                               
         CLI   QOPTION2,C'Y'       DUMP OUT MODE?                               
         JNE   WREP0060            NO - SO CONINTUE                             
*                                                                               
         ZICM  R6,REC,2            EXTRACT LENGTH OF RECORD                     
         LA    R5,REC                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',(R6),=C'1D'                
*                                                                               
WREP0060 EQU   *                                                                
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WREP0080            NO - SO CONTINUE                             
*                                                                               
         MVC   P(12),=C'GOING TO PUT'                                           
         GOTO1 REPORT                                                           
*                                                                               
WREP0080 EQU   *                                                                
*                                                                               
         PUT   FILOUTA,REC         WRITE IT                                     
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WREP0100            NO - SO CONTINUE                             
*                                                                               
         MVC   P(13),=C'BACK FROM PUT'                                          
         GOTO1 REPORT                                                           
*                                                                               
WREP0100 EQU   *                                                                
*                                                                               
         SR    RF,RF               CLEAR R5                                     
         LH    RF,TOTRECS          GET TOTAL # RECORDS WRITTEN                  
         LA    RF,1(RF)            INC COUNT OF RECORDS WRITTEN                 
         STH   RF,TOTRECS          AND STORE IT                                 
*                                                                               
         CLC   =C'NU',RREPKREP     CLEAR CHANNEL?                               
         BE    WREP0110            YES - NO LOOP                                
*                                                                               
         LA    R4,2(R4)            BUMP TO NEXT SUBREP CODE                     
*                                                                               
***      MVC   P(03),=C'R0='                                                    
***      STCM  R0,15,P+4                                                        
***      GOTO1 REPORT                                                           
*                                                                               
         BCT   R8,WREP0020         GO BACK FOR NEXT O/P RECORD                  
WREP0110 EQU   *                                                                
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WREP0120            NO - SO CONTINUE                             
*                                                                               
         MVC   P(19),=C'ALL RECORDS WRITTEN'                                    
         GOTO1 REPORT                                                           
*                                                                               
WREP0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
SETOFFCS NTR1                                                                   
         MVC   REPKEYSV,KEY        SAVE KEY FOR RESTART                         
         LA    R7,REC+4                                                         
         USING GSPLRECD,R7                                                      
*                                                                               
         MVI   GSPLOFFL,2          SET ELEMENT CODE                             
         SR    R3,R3               CLEAR COUNTER                                
         LA    R4,GSPLOFFN         SET A(FIRST OFFICE IN ELEMENT)               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,4               SET KEY FOR OFFICE READ                      
         MVC   KEY+23(2),SAVEREP   INSERT THIS REP INTO KEY                     
         GOTO1 HIGH                READ FIRST OFFICE FOR REP                    
         B     SOFF0040                                                         
SOFF0020 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT OFFICE FOR REP                     
SOFF0040 EQU   *                                                                
*                                                                               
*   TEST                                                                        
         MVC   P+1(08),=C'OFFC KEY'                                             
         MVC   P+12(27),KEY                                                     
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   KEY,4               OFFICE RECORD?                               
         BNE   SOFF0200            NO  - RECORDS FINISHED                       
         CLC   KEY+23(2),SAVEREP   SAME REP?                                    
         BNE   SOFF0200            NO  - RECORDS FINISHED                       
         LA    R3,1(R3)            INCREMENT COUNTER                            
         MVC   0(2,R4),KEY+25      INSERT OFFICE INTO ELEMENT                   
         LA    R4,2(R4)            BUMP TO NEXT SLOT IN ELEMENT                 
         B     SOFF0020            GO BACK FOR NEXT RECORD                      
SOFF0200 EQU   *                                                                
*                                                                               
*   SET OFFICE COUNT IN ELEMENT                                                 
*                                                                               
         STC   R3,GSPLOFNM         INSERT COUNT                                 
         SLL   R3,1                DOUBLE COUNT TO CALCULATE SIZE               
         LA    R3,3(R3)            ADD IN ELT CODE, LEN, COUNT                  
         STC   R3,GSPLOFLN         MOVE TO ELEMENT LENGTH                       
*                                                                               
*   RESET THE BASE LENGTH VALUES FOR THIS RECORD                                
*                                                                               
         LA    RF,GSPLSUBL         SET LENGTH OF DATA ONLY                      
         STCM  RF,3,GSPLFLEN       STORE LOW ORDER TWO BYTES                    
         LH    R6,GSPLFLEN         L(RECORD)                                    
         LA    R6,GSPLFRST(R6)     L(KEY+CNTL+DATA)                             
         STH   R6,GSPLFLEN         STORE L(RECORD)                              
         LA    R6,4(R6)            L(ENTIRE TAPE RECORD)                        
         STH   R6,REC              STORE IT                                     
*                                                                               
*   NOW ADD IN THE ADDITIONAL LENGTH OF THE ADDITIONAL ELEMENT                  
*                                                                               
         ZICM  RF,GSPLFLEN,2       GET RECORD LENGTH                            
         AR    RF,R3               ADD LENGTH NEW ELEMENT                       
         STH   RF,GSPLFLEN         RESET RECORD LENGTH                          
         ZICM  RF,REC,2            GET OVERALL RECORD LENGTH                    
         AR    RF,R3               ADD LENGTH NEW ELEMENT                       
         STH   RF,REC              RESET OVERALL RECORD LENGTH                  
*&&DO                                                                           
*   TEST                                                                        
         LA    R6,192              PHONY LENGTH                                 
         LA    R5,REC                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',(R6),=C'1D'                
*   TEST END                                                                    
*&&                                                                             
*                                                                               
*   NOW RETRIEVE THE ORIGINAL RECORD                                            
*                                                                               
         MVC   KEY,REPKEYSV        RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY MUST BE FOUND                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GREC                RETRIEVE THE PHYSICAL RECORD                 
*                                                                               
         XIT1                                                                   
         DROP  R7                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THIS ROUTINE PROCESSES ALL REP RECORDS.  IF MASTER, SUB REP RECS              
*        ARE GENERATED.  IF NOT MASTER, RECORD IS SKIPPED.                      
*                                                                               
READSALS NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,6               SET KEY TYPE                                 
         GOTO1 HIGH                READ FIRST KEY: PROCESS ALL                  
*                                     S/P RECS WITHIN THIS FILE                 
         B     RSAL0040                                                         
RSAL0020 EQU   *                                                                
         GOTO1 SEQ                 READ SEQUENTIAL                              
RSAL0040 EQU   *                                                                
         CLI   KEY,6               S/P RECORD?                                  
         BH    RSAL0800            NO  - FINISHED                               
         CLC   =C'XX',QOFFICE      LIMIT PROCESSING?                            
         BNE   RSAL0050            NO                                           
         CLC   =C'B3',KEY+22       YES - EJOR?                                  
         BE    RSAL0050            YES - ACCEPT IT                              
         CLC   =C'MS',KEY+22       NO  - MASTER?                                
         BNE   RSAL0020            NO  - SKIP IT                                
RSAL0050 EQU   *                                                                
         CLC   =C'YY',QOFFICE      LIMIT PROCESSING?                            
         BNE   RSAL0055            NO                                           
         CLC   =C'LS',KEY+22       YES - LUNA?                                  
         BNE   RSAL0020            NO  - SKIP IT                                
RSAL0055 EQU   *                                                                
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   RSAL0060            NO - SO CONTINUE                             
*                                                                               
         MVC   P(16),=C'S/P RECORD READ:'                                       
         MVC   P+16(27),KEY                                                     
         GOTO1 REPORT                                                           
*                                                                               
RSAL0060 EQU   *                                                                
         GOTO1 GREC                RETRIEVE THE RECORD                          
*                                                                               
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTSALS          GET TOTAL S/P RECORDS READ                   
         LA    R5,1(R5)            INC COUNT OF S/P RECORDS READ                
         STH   R5,TOTSALS          AND STORE IT                                 
*                                                                               
         TM    RSALFLG,X'20'       USE FOR EDI?                                 
         BO    RSAL0070            NO  - BLOCKED OUT                            
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    RSAL0080            NO  - KEEP IT                                
*                                  YES - COUNT, THEN SKIP                       
RSAL0070 EQU   *                                                                
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTSALL          GET TOTAL S/P RECORDS SKIPPED                
         LA    R5,1(R5)            INC COUNT OF S/P RECORDS SKIPPED             
         STH   R5,TOTSALL          AND STORE IT                                 
         B     RSAL0020            GO BACK FOR NEXT                             
RSAL0080 EQU   *                                                                
*                                                                               
         BAS   RE,WRITESAL         GENERATE SAL RECORDS                         
         B     RSAL0020            GO BACK FOR NEXT                             
*                                                                               
RSAL0800 EQU   *                                                                
         SR    R0,R0               FINISHED                                     
*                                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
* THIS ROUTINE GENERATES ONE RECORD FOR EACH S/P RECORD FOUND.                  
*                                                                               
WRITESAL NTR1                                                                   
*                                                                               
         XCEF  REC,1000            CLEAR REC                                    
         LA    R7,REC+4                                                         
         USING GSPLRECD,R7                                                      
*                                                                               
         LA    RF,GSPLSALL         SET LENGTH OF DATA ONLY                      
         STCM  RF,3,GSPLFLEN       STORE LOW ORDER TWO BYTES                    
         LH    R6,GSPLFLEN         L(RECORD)                                    
         LA    R6,GSPLFRST(R6)     L(KEY+CNTL+DATA)                             
         STH   R6,GSPLFLEN         STORE L(RECORD)                              
         LA    R6,4(R6)            L(ENTIRE TAPE RECORD)                        
         STH   R6,REC              STORE IT                                     
*                                                                               
* BUILD THE KEY                                                                 
*                                                                               
         MVI   GSPLKTYP,X'71'      RECORD TYPE                                  
         MVI   GSPLKSTP,X'01'      RECORD SUB TYPE                              
         MVI   GSPLSBCD,1          SET ELEMENT CODE TYPE                        
         MVI   GSPLSBLN,GSPLSALL   SET ELEMENT LENGTH                           
*                                                                               
         MVC   GSPLKSRP,RSALKREP   INSERT SUB REP INTO KEY                      
         MVC   GSPLKSAL,RSALKSAL   INSERT SALESPERSON INTO KEY                  
         MVC   GSPLSPNM,RSALNAME   INSERT S/P NAME                              
         MVC   GSPLSPOF,RSALOFF    INSERT S/P OFFICE                            
         MVC   GSPLSPTM,RSALTEAM   INSERT S/P TEAM                              
         MVC   GSPLSPFX,RSALFAX    INSERT FAX NUMBER AS ENTERED                 
         OC    GSPLSPEM,SPACES     CLEAR EMAIL ADDR TO SPACES                   
         CLC   RSALPOWR,=C'00'     ANY POWER CODE ENTERED?                      
         BE    WSAL0020            NO                                           
         CLC   RSALPOWR,SPACES     ANY POWER CODE ENTERED?                      
         BNH   WSAL0020            NO                                           
         MVC   GSPLSPPC,RSALPOWR   YES - INSERT POWER CODE                      
*                                  ONLY CHECK 'MINUS' IF POWERCODE              
*                                     IS PRESENT                                
         TM    RSALPFLG,X'80'      POWER CODE IS 'MINUS'?                       
         BNO   WSAL0020            NO                                           
         OI    GSPLSPFL,X'10'      YES - SET FLAG TO 'MINUS'                    
WSAL0020 EQU   *                                                                
         TM    RSALFLG,X'04'       E-MAIL PREFERENCE SET?                       
         BNO   WSAL0040            NO                                           
         OI    GSPLSPFL,X'40'      YES - SET FLAG                               
WSAL0040 EQU   *                                                                
         TM    RSALFLG,X'02'       FAX    PREFERENCE SET?                       
         BNO   WSAL0060            NO                                           
         OI    GSPLSPFL,X'20'      YES - SET FLAG                               
WSAL0060 EQU   *                                                                
*                                                                               
*   EMAIL ADDRESS FOR S/P IS IN S/P2 RECORD.  NEED TO READ THAT                 
*        RECORD (WHICH MAY NOT EXIST), EXTRACT ADDRESS, IF FOUND,               
*        THEN RESTORE KEY SEQUENCE TO S/P1 RECORD                               
*                                                                               
         MVC   KEY2SAVE,KEY        SAVE OFF KEY                                 
         MVI   KEY,X'46'           SET TO READ S/P KEY2                         
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   WSAL0120            NO  - CAN'T ADD EMAIL INFO                   
*                                                                               
         GOTO1 GREC                RETRIEVE THE RECORD                          
         LA    RF,RSA2ELEM                                                      
WSAL0080 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD                                
         BE    WSAL0120            YES - CAN'T ADD EMAIL INFO                   
         CLI   0(RF),X'20'         EMAIL ADDR ELEMENT?                          
         BE    WSAL0100            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     WSAL0080            GO BACK FOR NEXT                             
WSAL0100 EQU   *                                                                
         ZIC   RE,1(RF)            GET ELEMENT LENGTH                           
         SH    RE,=H'3'            SUBTRACT CONTROL +1 FOR EX                   
         EX    RE,WSAL0800         MOVE EMAIL ADDR BY LENGTH                    
*                                                                               
WSAL0120 EQU   *                                                                
         MVC   KEY(27),KEY2SAVE    RESET ORIGINAL KEY                           
         GOTO1 HIGH                RESET KEY SEQUENCE                           
*                                                                               
* NOW DUMP IT OUT JUST TO MAKE SURE WE'VE GOT IT WORKING                        
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WSAL0140            NO - SO CONTINUE                             
*                                                                               
         MVC   P(15),=C'GOING TO PRNTBL'                                        
         GOTO1 REPORT                                                           
*                                                                               
WSAL0140 EQU   *                                                                
*                                                                               
         CLI   QOPTION2,C'Y'       DUMP OUT MODE?                               
         JNE   WSAL0160            NO - SO CONINTUE                             
*                                                                               
         ZICM  R6,REC,2            EXTRACT LENGTH OF RECORD                     
         LA    R5,REC                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',(R6),=C'1D'                
*                                                                               
WSAL0160 EQU   *                                                                
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WSAL0180            NO - SO CONTINUE                             
*                                                                               
         MVC   P(12),=C'GOING TO PUT'                                           
         GOTO1 REPORT                                                           
*                                                                               
WSAL0180 EQU   *                                                                
*                                                                               
         PUT   FILOUTA,REC         WRITE IT                                     
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WSAL0200            NO - SO CONTINUE                             
*                                                                               
         MVC   P(13),=C'BACK FROM PUT'                                          
         GOTO1 REPORT                                                           
*                                                                               
WSAL0200 EQU   *                                                                
*                                                                               
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTSPS           GET TOTAL # RECORDS WRITTEN                  
         LA    R5,1(R5)            INC COUNT OF RECORDS WRITTEN                 
         STH   R5,TOTSPS           AND STORE IT                                 
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WSAL0220            NO - SO CONTINUE                             
*                                                                               
         MVC   P(19),=C'ALL S/P     WRITTEN'                                    
         GOTO1 REPORT                                                           
*                                                                               
WSAL0220 EQU   *                                                                
         XIT1                                                                   
WSAL0800 MVC   GSPLSPEM(0),2(RF)   MOVE EMAIL ADDR FOR LENGTH                   
         DROP  R7                                                               
KEY2SAVE DS    CL27                                                             
         DS    0H                                                               
         EJECT                                                                  
***>>>>                                                                         
*                                                                               
* THIS ROUTINE PROCESSES ALL REP RECORDS.  IF MASTER, SUB REP RECS              
*        ARE GENERATED.  IF NOT MASTER, RECORD IS SKIPPED.                      
*                                                                               
READPPS  NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'31'           SET KEY TYPE                                 
         GOTO1 HIGH                READ FIRST KEY: PROCESS ALL                  
*                                     P/P RECS WITHIN THIS FILE                 
         B     RPPS0040                                                         
RPPS0020 EQU   *                                                                
         GOTO1 SEQ                 READ SEQUENTIAL                              
RPPS0040 EQU   *                                                                
         CLI   KEY,X'31'           P/P RECORD?                                  
         BH    RPPS0800            NO  - FINISHED                               
         CLC   =C'XX',QOFFICE      LIMIT PROCESSING?                            
         BNE   RPPS0050            NO                                           
         CLC   =C'B3',KEY+22       YES - EJOR?                                  
         BE    RPPS0050            YES - ACCEPT IT                              
         CLC   =C'MS',KEY+22       NO  - MASTER?                                
         BNE   RPPS0020            NO  - SKIP IT                                
RPPS0050 EQU   *                                                                
         CLC   =C'YY',QOFFICE      LIMIT PROCESSING?                            
         BNE   RPPS0055            NO                                           
         CLC   =C'LS',KEY+22       YES - LUNA?                                  
         BNE   RPPS0020            NO  - SKIP IT                                
RPPS0055 EQU   *                                                                
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   RPPS0060            NO - SO CONTINUE                             
*                                                                               
         MVC   P(16),=C'P/P RECORD READ:'                                       
         MVC   P+16(27),KEY                                                     
         GOTO1 REPORT                                                           
*                                                                               
RPPS0060 EQU   *                                                                
         GOTO1 GREC                RETRIEVE THE RECORD                          
*                                                                               
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTPPS           GET TOTAL P/P RECORDS READ                   
         LA    R5,1(R5)            INC COUNT OF P/P RECORDS READ                
         STH   R5,TOTPPS           AND STORE IT                                 
*                                                                               
         TM    RPTPFLG,X'20'       USE FOR EDI?                                 
         BO    RPPS0070            NO  - BLOCKED OUT                            
*                                                                               
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BNZ   RPPS0070            YES - COUNT, THEN SKIP                       
*                                                                               
         OC    RPTPOFF,RPTPOFF     ANY OFFICE?                                  
         BNZ   RPPS0080            YES - KEEP IT                                
*                                  NO  - COUNT, THEN SKIP                       
RPPS0070 EQU   *                                                                
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTPPL           GET TOTAL P/P RECORDS SKIPPED                
         LA    R5,1(R5)            INC COUNT OF P/P RECORDS SKIPPED             
         STH   R5,TOTPPL           AND STORE IT                                 
         B     RPPS0020            GO BACK FOR NEXT                             
RPPS0080 EQU   *                                                                
*                                                                               
         BAS   RE,WRITEPPS         GENERATE PP  RECORDS                         
         B     RPPS0020            GO BACK FOR NEXT                             
*                                                                               
RPPS0800 EQU   *                                                                
         SR    R0,R0               FINISHED                                     
*                                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
* THIS ROUTINE GENERATES ONE RECORD FOR EACH S/P RECORD FOUND.                  
*                                                                               
WRITEPPS NTR1                                                                   
*                                                                               
         XCEF  REC,1000            CLEAR REC                                    
         LA    R7,REC+4                                                         
         USING GSPLRECD,R7                                                      
*                                                                               
         LA    RF,GSPLPPL          SET LENGTH OF DATA ONLY                      
         STCM  RF,3,GSPLFLEN       STORE LOW ORDER TWO BYTES                    
         LH    R6,GSPLFLEN         L(RECORD)                                    
         LA    R6,GSPLFRST(R6)     L(KEY+CNTL+DATA)                             
         STH   R6,GSPLFLEN         STORE L(RECORD)                              
         LA    R6,4(R6)            L(ENTIRE TAPE RECORD)                        
         STH   R6,REC              STORE IT                                     
*                                                                               
* BUILD THE KEY                                                                 
*                                                                               
         MVI   GSPLKTYP,X'71'      RECORD TYPE                                  
         MVI   GSPLKSTP,X'02'      RECORD SUB TYPE                              
         MVI   GSPLSBCD,1          SET ELEMENT CODE TYPE                        
         MVI   GSPLSBLN,GSPLPPL    SET ELEMENT LENGTH                           
*                                                                               
         MVC   GSPLKPRP,RPTPKREP   INSERT SUB REP INTO KEY                      
         MVC   GSPLKSAL,RPTPKREC   INSERT POINTPERSON INTO KEY                  
         MVC   GSPLSPNM,RPTPNAME   INSERT P/P NAME                              
         MVC   GSPLPPOF,RPTPOFF    INSERT S/P OFFICE                            
*                                                                               
         LA    R3,RPTPELEM         SET A(01 ELEMENT)                            
WPPS0020 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    WPPS0140            YES                                          
         CLI   0(R3),X'20'         S/P EMAIL ADDRESS ELEMENT                    
         BE    WPPS0060            YES -                                        
         CLI   0(R3),X'21'         FAX ELEMENT?                                 
         BE    WPPS0100            YES                                          
WPPS0040 EQU   *                                                                
         ZIC   RF,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,RF                                                            
         B     WPPS0020            GO BACK FOR NEXT                             
WPPS0060 EQU   *                                                                
         ZIC   RF,1(R3)            GET ELEMENT LENGTH                           
         SH    RF,=H'3'            SUB FOR CTL, LEN, AND EX                     
         EX    RF,WPPS0080         MOVE EMAIL ADDR BY LENGTH                    
         OC    GSPLPPEM(50),SPACES CLEAR OUT BIN ZERO IN ADDR                   
         B     WPPS0040                                                         
WPPS0080 MVC   GSPLPPEM(0),RPTPEMAL-RPTPEMEM(R3)                                
*                                  MOVE EMAIL ADDR BY LENGTH                    
WPPS0100 EQU   *                                                                
         USING RPTPFXEM,R3                                                      
         MVC   GSPLPPFX,RPTPFXFX   INSERT FAX NUMBER                            
         TM    RPTPFXFG,X'40'      EMAIL PREFERENCE?                            
         BNO   WPPS0120            NO                                           
         OI    GSPLPPFL,X'40'      YES - TURN ON SAME FLAG                      
WPPS0120 EQU   *                                                                
         TM    RPTPFXFG,X'20'      FAX   PREFERENCE?                            
         BNO   WPPS0140            NO                                           
         OI    GSPLPPFL,X'20'      YES - TURN ON SAME FLAG                      
WPPS0140 EQU   *                                                                
*                                                                               
         DROP  R3,R7                                                            
*                                                                               
* NOW DUMP IT OUT JUST TO MAKE SURE WE'VE GOT IT WORKING                        
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WPPS0160            NO - SO CONTINUE                             
*                                                                               
         MVC   P(15),=C'GOING TO PRNTBL'                                        
         GOTO1 REPORT                                                           
*                                                                               
WPPS0160 EQU   *                                                                
*                                                                               
         CLI   QOPTION2,C'Y'       DUMP OUT MODE?                               
         JNE   WPPS0180            NO - SO CONINTUE                             
*                                                                               
         ZICM  R6,REC,2            EXTRACT LENGTH OF RECORD                     
         LA    R5,REC                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',(R6),=C'1D'                
*                                                                               
WPPS0180 EQU   *                                                                
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WPPS0200            NO - SO CONTINUE                             
*                                                                               
         MVC   P(12),=C'GOING TO PUT'                                           
         GOTO1 REPORT                                                           
*                                                                               
WPPS0200 EQU   *                                                                
*                                                                               
         PUT   FILOUTA,REC         WRITE IT                                     
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WPPS0220            NO - SO CONTINUE                             
*                                                                               
         MVC   P(13),=C'BACK FROM PUT'                                          
         GOTO1 REPORT                                                           
*                                                                               
WPPS0220 EQU   *                                                                
*                                                                               
         SR    R5,R5               CLEAR R5                                     
         LH    R5,TOTPPWS          GET TOTAL # RECORDS WRITTEN                  
         LA    R5,1(R5)            INC COUNT OF RECORDS WRITTEN                 
         STH   R5,TOTPPWS          AND STORE IT                                 
*                                                                               
         CLI   QOPTION1,C'Y'       TRACE MODE?                                  
         JNE   WPPS0240            NO - SO CONTINUE                             
*                                                                               
         MVC   P(19),=C'ALL P/P     WRITTEN'                                    
         GOTO1 REPORT                                                           
*                                                                               
WPPS0240 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***>>>>                                                                         
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
         LTORG                                                                  
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=2048,BLKSIZE=8120,BUFNO=2                                  
*                                                                               
IOAREA   DS    CL1000              IO AREA                                      
         ORG   IOAREA                                                           
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENSAL2                                                      
         EJECT                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENPTP                                                       
         EJECT                                                                  
*                                                                               
* LITERALS, CONSTANTS, AND WORK AREA                                            
*                                                                               
*                                                                               
*    WORK SPACE, ETC.                                                           
*                                                                               
MYWORKD  DSECT                                                                  
RELO     DS    F                   RELOCATION FACTOR                            
AENTRY   DS    XL4                 CURRENT ENTRY IN SPEC LIST                   
ASPECS   DS    XL4                 A(REPORT SPEC)                               
LSPECS   DS    XL4                 L(REPORT SPEC)                               
SPECID   DS    CL8                 REPORT SPEC ID ('RERGXX  ')                  
TODAY    DS    CL6                 YYMDD DATE                                   
TOTREPS  DS    XL2                 TOTAL REP RECS READ                          
TOTRECS  DS    XL2                 TOTAL REP RECS WRITTEN                       
TOTSALS  DS    XL2                 TOTAL S/P RECS READ                          
TOTSALL  DS    XL2                 TOTAL S/P RECS SKIPPED                       
TOTSPS   DS    XL2                 TOTAL S/P RECS WRITTEN                       
TOTPPS   DS    XL2                 TOTAL S/P RECS READ                          
TOTPPL   DS    XL2                 TOTAL S/P RECS SKIPPED                       
TOTPPWS  DS    XL2                 TOTAL S/P RECS WRITTEN                       
COMMAND  DS    CL8                                                              
AIOAREA  DS    A                                                                
REPKEYSV DS    CL27                                                             
SAVEREP  DS    CL2                 SAVE REP FOR OFFICE WORK                     
         DS    0F                                                               
*                                                                               
REC      DS    XL1000              AREA FOR RECORD                              
         SPACE 2                                                                
MYWORKX  EQU   *                                                                
*                                                                               
       ++INCLUDE GEGENSPSAL        REPORT SPOT S/P RECORD                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070REREPSP02 11/01/05'                                      
         END                                                                    
