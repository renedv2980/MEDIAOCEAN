*          DATA SET REREP1S02  AT LEVEL 138 AS OF 05/01/02                      
*PHASE RE1S02A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
         TITLE 'REREP1S02 - STATION JOIN DATE/BILLING ANALYZER'                 
*********************************************************************           
*                                                                   *           
*        REREP1S02 --- STATION BILLING ANALYZER                     *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* APR29/98 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* JAN11/00 (BU ) --- FIX NON-MASTER PROCESSING                      *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   QUESTOR  =  PRINTSR   -   DISPLAY SORT RECORDS + VARIOUS        *           
*                                DATA COMPONENTS                    *           
*               PRINT     -   DISPLAYS VARIOUS DATA COMPONENTS      *           
*   QUESTOR+7=  #         -   PROCESSES FIRST N CONTRACTS ONLY      *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      R9  =  THIRD  BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE1S02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1S02,R7,R9,RR=RE                                           
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
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
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
MODEEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
*                                                                               
*   ALL PROCESSING IN THIS PROGRAM IS DONE IN THE INITIAL PASS                  
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
*                                                                               
INITIAL  NTR1                                                                   
         MVI   RCSUBPRG,0          CLEAR HEADING SWITCH                         
         CLI   QOPTION1,C'A'       ALL BILLING TO BE DISPLAYED?                 
         BE    INIT0005            YES                                          
         MVI   RCSUBPRG,1          NO  - SET ALTERNATE HEADING                  
INIT0005 EQU   *                                                                
         MVC   PAGE,=H'1'                                                       
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
*                                                                               
         BAS   RE,SETDATES                                                      
         XC    WKSPACE,WKSPACE     CLEAR COMPARISON TEST AREA                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           RETRIEVE THE REP RECORD                      
*                                                                               
*   TAKE REP CODE FROM REQUEST CARD - MASTER RUN REPLACES                       
*        FIELD 'RCREPFL' WITH EACH SUBSIDIARY IN TURN                           
*                                                                               
*                                                                               
         MVC   KEY+25(2),QREP      INSERT REP CODE                              
         GOTO1 HIGH                                                             
         GOTO1 GETREP              RETRIEVE THE REP RECORD                      
         LA    RF,SUBREPS                                                       
         ST    RF,ASUBREP          SAVE A(SUBREPS TABLE)                        
*                                                                               
         XC    SUBREPS,SUBREPS     CLEAR SUBREP TABLE                           
         MVC   SUBREPS(2),QREP INSERT REP CODE IN TABLE                         
         MVI   MSTRFLAG,C'Y'       SET 'MASTER REP'                             
         MVC   ALTREPFL,QREP                                                    
         CLC   =X'FFFF',RREPMAST   IS THIS A MASTER?                            
         BE    INIT0010            YES - USE QREP VALUE                         
         OC    RREPMAST,RREPMAST   IS THIS A NON-MASTER?                        
         BZ    INIT0008            NO                                           
         CLC   =C'  ',RREPMAST     IS THIS A NON-MASTER?                        
         BNE   INIT0010            YES - USE QREP VALUE                         
INIT0008 EQU   *                                                                
         MVC   ALTREPFL,RREPMAST   NO  - USE THE MASTER REP                     
         MVI   MSTRFLAG,C'N'       SET 'NOT MASTER REP'                         
         B     INIT0020                                                         
INIT0010 EQU   *                                                                
*                                  REP CODE IS MASTER: INSERT SUBREPS           
         BAS   RE,SETSUBS          SET UP SUBREPS                               
INIT0020 EQU   *                                                                
*                                                                               
*                                  RETRIEVE PROFILE ELEMENT:                    
*                                     NOT USED RIGHT NOW                        
*                                                                               
*   TEST                                                                        
*        MVC   P+1(08),=C'SUBREPS='                                             
*        MVC   P+9(36),SUBREPS                                                  
*        MVC   P+48(1),MSTRFLAG                                                 
*        MVC   P+52(25),RREPMAST                                                
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         LA    RF,RREPELEM         SET A(01 ELEMENT)                            
INIT0040 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    INIT0120            YES - NO PROFILE ELEMENT                     
         CLI   0(RF),4             PROFILE ELEMENT FOUND?                       
         BE    INIT0060            YES - LOOK FOR PROGRAM UNIT SET              
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     INIT0040                                                         
INIT0060 EQU   *                                                                
         ZIC   RE,2(RF)            SET # OF 10-BYTE PROGRAM UNITS               
         LA    RF,4(RF)            BUMP TO 1ST PROGRAM UNIT                     
INIT0080 EQU   *                                                                
         CLI   0(RF),RREPQLFM      PROGRAM UNIT = FILE PROG?                    
         BE    INIT0100            YES                                          
         LA    RF,10(RF)           NO  - BUMP TO NEXT UNIT                      
         BCT   RE,INIT0080         GO BACK FOR NEXT                             
         B     INIT0120            NO ELEMENT FOUND -                           
INIT0100 EQU   *                                                                
*                                                                               
*   AT THIS POINT, PROFILE FOR THE 'FILE' PROGRAM HAS BEEN FOUND.               
*        NOTHING HAS BEEN DONE WITH THE PROFILE.  THIS CODE                     
*        IS VESTIGIAL, AND MAY BE USED IN THE FUTURE.                           
*                                                                               
INIT0120 EQU   *                                                                
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         L     RF,ASUBREP                                                       
         MVC   SUBINUSE,0(RF)      LOAD SUBREP IN USE                           
         BAS   RE,REPNAME          RETRIEVE REP NAME                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'8E'           CYCLE THROUGH CONTRACT RECORDS               
         MVC   KEY+1(2),SUBINUSE   INSERT REP CODE IN PROGRESS                  
*                                                                               
*   TEST                                                                        
***      MVC   KEY+3(5),=C'WTTT '  FORCE HIGH START                             
*   TEST END                                                                    
*                                                                               
         GOTO1 HIGH                                                             
*        SR    R2,R2                                                            
         B     INIT0150                                                         
INIT0140 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT KEY IN SEQUENCE                    
INIT0150 EQU   *                                                                
         CLI   KEY,X'8E'           STILL CONTRACT?                              
         BNE   INIT0400            NO  - REP FINISHED - CHECK NEXT              
         CLC   KEY+1(2),SUBINUSE   SAME REP?                                    
         BNE   INIT0400            NO  - REP FINISHED - CHECK NEXT              
         CLI   KEY+16,1            TYPE 1 KEY?                                  
         BNE   INIT0140            NO  - SKIP IT                                
         CLI   QOPTION1,C'A'       ALL BILLING?                                 
         BNE   INIT0155            NO  - STARTDAT IS CUTOFF DATE                
         CLC   KEY+10(2),STARTDAT  CONTRACT ENDED BEFORE                        
*                                     REQUEST START DATE?                       
         BL    INIT0140            YES - SKIP IT                                
INIT0155 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'KEY USED ='                                           
*        MVC   P+11(34),KEY                                                     
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         OC    WKSPACE,WKSPACE     ANY ENTRY IN WKSPACE?                        
         BZ    INIT0160            NO  - FIRST TIME                             
         CLC   KEY+1(7),WKSPACE    YES - SAME REP/STATION IN PROGRESS?          
         BE    INIT0180            YES - PROCESS THE ORDER                      
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'KEY+1    ='                                           
*        MVC   P+11(34),KEY                                                     
*        MVC   P+50(16),WKSPACE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         BAS   RE,DISPSTAT         NO  - DISPLAY LAST STATION INFO              
INIT0160 EQU   *                                                                
         XC    WKSPACE,WKSPACE     CLEAR THE WKSPACE                            
         MVC   WKSPACE(7),KEY+1    INSERT REP/STATION INTO WKSPACE              
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'RESET    ='                                           
*        MVC   P+11(34),KEY                                                     
*        MVC   P+50(16),WKSPACE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
INIT0180 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'NEXT CON ='                                           
*        MVC   P+11(34),KEY                                                     
*        MVC   P+50(16),WKSPACE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         BAS   RE,GETCON           RETRIEVE CONTRACT RECORD                     
         TM    RCONCNTL,X'80'      RECORD DELETED?                              
         BNO   INIT0190            YES - SKIP IT                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'DELETED  ='                                           
*        MVC   P+11(34),KEY                                                     
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         B     INIT0140                                                         
INIT0190 EQU   *                                                                
         LA    R2,RCONELEM         RETRIEVE BUCKETS                             
INIT0200 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    INIT0140            YES - GO BACK FOR NEXT RECORD                
         CLI   0(R2),3             ESTIMATE BUCKET?                             
         BE    INIT0220            YES                                          
         CLI   0(R2),4             INVOICE BUCKET?                              
         BE    INIT0240            YES                                          
         B     INIT0280            NO                                           
INIT0220 EQU   *                                                                
         CLC   WKBOOK,2(R2)        DATE IN WKSPACE < BUCKET                     
*                                     MONTH OF SERVICE?                         
         BNL   INIT0280                                                         
         MVC   WKBOOK,2(R2)        PUT NEW DATE IN EST DATE                     
         B     INIT0280                                                         
INIT0240 EQU   *                                                                
         CLC   WKBILL,2(R2)        DATE IN WKSPACE < BUCKET                     
*                                     MONTH OF SERVICE?                         
         BNL   INIT0280                                                         
         MVC   WKBILL,2(R2)        PUT NEW DATE IN INV DATE                     
         B     INIT0280                                                         
INIT0280 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     INIT0200                                                         
INIT0400 EQU   *                                                                
*                                                                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'INIT0400 ='                                           
*        MVC   P+11(34),KEY                                                     
*        MVC   P+50(16),WKSPACE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         OC    WKSPACE,WKSPACE     ANY ENTRY IN WKSPACE?                        
         BZ    INIT0420            NO  -                                        
         BAS   RE,DISPSTAT         PROCESS LAST ENTRY WHICH                     
*                                     CAUSED BREAK                              
INIT0420 EQU   *                                                                
         XC    WKSPACE,WKSPACE     CLEAR THE WKSPACE                            
*                                                                               
         L     RF,ASUBREP          LOAD A(SUBREP IN USE)                        
         LA    RF,2(RF)            BUMP TO NEXT ENTRY                           
         ST    RF,ASUBREP                                                       
         OC    0(2,RF),0(RF)       ANY ENTRY IN SLOT?                           
         BNZ   INIT0120            YES - GO BACK AND PROCESS IT                 
         MVC   P+1(37),=C'** STATION/BILLING REPORT COMPLETE **'                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'**                          **'                       
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
         EJECT                                                                  
*                                                                               
*   REPNAME:  RETRIEVE SUBSIDIARY REP NAME, IF A MASTER RUN                     
*                                                                               
REPNAME  NTR1                                                                   
         CLI   MSTRFLAG,C'Y'       MASTER REP RUN?                              
         BNE   REPN0120            NO  - EXIT ROUTINE                           
         XC    KEY,KEY                                                          
         MVI   KEY,1               SET RECORD TYPE                              
         MVC   KEY+25(2),SUBINUSE  SET REP ID                                   
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - MUST BE ON FILE                        
         GOTO1 GETREP                                                           
         MVC   SVREPNAM,RREPNAME   SAVE REP NAME                                
REPN0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DISPSTAT:  DISPLAY STATION INFORMATION OF LAST STATION PROCESSED            
*                                                                               
DISPSTAT NTR1                                                                   
         CLI   QOPTION1,C'A'       ALL BILLING?                                 
         BE    DIST0010            YES                                          
*                                  NO  - CHECK FOR NO BILLING AFTER             
*                                     EFFECTIVE DATE                            
         CLC   WKBOOK,STDATBIN     BOOKING AFTER EFFECTIVE DATE                 
         BH    DIST0080            YES - DON'T SHOW STATION                     
***>>>   CLC   WKBILL,STDATBIN     BILLING AFTER EFFECTIVE DATE                 
***>>>   BH    DIST0080            YES - DON'T SHOW STATION                     
*                                                                               
*   TEST                                                                        
*        MVC   P+1(08),=C'DISPSTAT'                                             
*        MVC   P+10(16),WKSPACE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
DIST0010 EQU   *                                                                
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
         XC    KEY,KEY                                                          
         MVI   KEY,2               INSERT KEY TYPE                              
         MVC   KEY+20(7),WKREP     INSERT REP CODE/STATION CALLS                
         GOTO1 HIGH                READ STATION KEY                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO                                           
         GOTO1 GETSTA              RETRIEVE STATION RECORD                      
*                                                                               
*   TEST                                                                        
*        MVC   P+1(08),=C'LEAVE=  '                                             
*        MVC   P+10(05),RCONKSTA                                                
*        MVC   P+20(6),RSTASTRT                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         OC    RSTAEND,RSTAEND     STATION HAVE LEAVE DATE?                     
         BNZ   DIST0060            YES - SKIP ITS OUTPUT                        
**>>     MVC   P+2(2),WKREP                                                     
         MVC   P+10(5),WKSTA       DISPLAY STATION CALLS                        
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(5,P+20)                                
*                                  DISPLAY JOINDATE                             
         OC    WKBOOK,WKBOOK       ANY BOOK DATE?                               
         BZ    DIST0020            NO                                           
         XC    WORK,WORK                                                        
         MVC   WORK(2),WKBOOK                                                   
         GOTO1 DATCON,DMCB,(3,WORK),(5,P+34)                                    
*                                  DISPLAY LATEST BOOK DATE                     
DIST0020 EQU   *                                                                
         OC    WKBILL,WKBILL       ANY BILL DATE?                               
***>>>   BZ    DIST0040            NO                                           
         B     DIST0040            DON'T SHOW BILLING INFORMATION               
         XC    WORK,WORK                                                        
         MVC   WORK(2),WKBILL                                                   
         GOTO1 DATCON,DMCB,(3,WORK),(5,P+44)                                    
*                                  DISPLAY LATEST BILL DATE                     
DIST0040 EQU   *                                                                
         GOTO1 REPORT              DISPLAY PRINT LINE                           
         GOTO1 REPORT              DISPLAY BLANK LINE                           
DIST0060 EQU   *                                                                
         MVC   KEY,KEYSAV2         RESTORE ORIGINAL KEY                         
         GOTO1 HIGH                RESTART THIS KEY                             
*                                                                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(13),=C'DISPSTAT EXIT'                                        
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
DIST0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SETDATES: SET UP DATES FROM REQUEST CARD                                    
*                                                                               
SETDATES NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),QSTART      DETERMINE START OF BCST MONTH                
         MVC   WORK+4(2),=C'15'    SET MID-MONTH                                
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,STARTDAT)                              
*                                  SET COMPRESSED DATE                          
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,STDATBIN)                             
*   TEST                                                                        
*        MVC   P+1(10),=C'STARTDATE='                                           
*        MVC   P+12(6),WORK+6                                                   
*        MVC   P+20(4),STARTDAT                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SETSUBS:  EXTRACT SUBREP CODES FROM MASTER REP RECORD                       
*                                                                               
SETSUBS  NTR1                                                                   
         LA    RF,RREPELEM         SET A(01 ELEMENT)                            
SSUB0040 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   SSUB0050            YES - NO PROFILE ELEMENT                     
         DC    H'0'                SUBREP ELT MUST BE THERE                     
SSUB0050 EQU   *                                                                
         CLI   0(RF),2             PROFILE ELEMENT FOUND?                       
         BE    SSUB0060            YES - LOOK FOR PROGRAM UNIT SET              
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     SSUB0040                                                         
SSUB0060 EQU   *                                                                
*                                                                               
         USING RREPSUB,RF                                                       
*                                                                               
         ZIC   RE,RREPSCNT         SET COUNT                                    
         SLA   RE,1                DOUBLE FOR LENGTH OF ENTRY                   
         BCTR  RE,0                MINUS 1 FOR EX STATEMENT                     
         EX    RE,SSUB0080         MOVE BY LENGTH                               
         XIT1                                                                   
SSUB0080 EQU   *                                                                
         MVC   SUBREPS(0),RREPSCOD MOVE BY LENGTH                               
*                                                                               
         DROP  RF                                                               
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         LA    R1,SAVEREGS-ALTHDR                                               
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
*                                                                               
         CLI   MSTRFLAG,C'Y'       MASTER REP?                                  
         BNE   ALTH0120            NO                                           
         MVC   HEAD3+1(13),=C'COMPANY  (  )'                                    
         MVC   HEAD3+15(33),SVREPNAM                                            
         MVC   HEAD3+11(2),SUBINUSE                                             
         MVC   HEAD4+1(15),=C'EFFECTIVE DATE:'                                  
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'00'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(5,HEAD4+18)                                
         B     ALTH0140                                                         
ALTH0120 EQU   *                                                                
*                                                                               
         MVC   HEAD3+1(15),=C'EFFECTIVE DATE:'                                  
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'00'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(5,HEAD3+18)                                
*                                                                               
ALTH0140 EQU   *                                                                
         B     MODEEXIT                                                         
         EJECT                                                                  
GETREP   LA    RF,RREPREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
GETSTA   LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*   TEST                                                                        
*        MVC   P+1(03),=C'DM='                                                  
*        MVC   P+4(4),DMCB+8                                                    
*        MVC   P+12(34),RCONREC                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 EQU   *                                                                
         TM    DMCB+8,X'02'        IS RECORD MARKED FOR DELETION?               
         BNZ   MODEEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
PUTFILE  NTR1                                                                   
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    MODEEXIT            YES - DON'T REWRITE IT                       
         ST    RF,AIOAREA                                                       
         GOTO1 PREC                                                             
         B     MODEEXIT                                                         
         SPACE 3                                                                
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'T',AL1(32),X'0000'                                             
STNTEXT  DC    C'T',AL1(07),C'O',AL1(01),C'T',AL1(20),X'0000'                   
*              WORK SPACE ETC                                                   
         SPACE 1                                                                
ALTREPFL DS    CL2                                                              
MSTRFLAG DS    CL1                                                              
SUBREPS  DS    CL36                ROOM FOR 18 SUBREP CODES                     
SUBINUSE DS    CL2                 SUBREP CODE IN USE                           
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
PX       DS    CL80                ALTERNATE PRINT SETUP AREA                   
AIOAREA  DS    A                                                                
ASUBREP  DS    A                   ADDRESS OF SUBREP IN PROGRESS                
STARTDAT DS    CL2                 COMPRESSED START DATE FROM REQUEST           
STDATBIN DS    CL3                 BINARY START DATE     FROM REQUEST           
SVREPNAM DS    CL33                SAVE AREA FOR REP NAME                       
*                                                                               
WKSPACE  DS    0CL16                                                            
WKREP    DS    CL2                                                              
WKSTA    DS    CL5                                                              
WKJOIN   DS    CL2                                                              
WKBILL   DS    CL2                                                              
WKBOOK   DS    CL2                                                              
         DS    CL3                 SPARE                                        
*                                                                               
COMMAND  DS    CL8                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
*                                                                               
*   COMMISSION RECORD IS ORG'D TO THE BUY RECORD, WHICH ISN'T USED,             
*    RATHER THAN THE CONTRACT RECORD, WHICH IS                                  
*                                                                               
         ORG RBUYREC                                                            
       ++INCLUDE REGENCOM                                                       
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE RESUBREPS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'138REREP1S02 05/01/02'                                      
         END                                                                    
