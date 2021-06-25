*          DATA SET REREPSW04  AT LEVEL 228 AS OF 05/01/02                      
*PHASE RESW02B,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
         TITLE 'REREPSW02B (RESW02B) --- COMPANY MERGER'                        
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPSW02B -- REP FILE COMPANY MERGER.                    *            
*                       CREATE A SINGLE FILE FROM DI/HN DATA, AS   *            
*                       NEW COMPANY 'DR'.                          *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* SEP21/93 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     =  STATION DISPLAY                               *            
*     QUESTOR+1   =  GROUP+BUD/COMM TABLE                          *            
*     QUESTOR+2   =  Y  =  TABLE MATCHUP                           *            
*                    #  =  USE CONTRACT+BUY TEST COUNT             *            
*     QUESTOR+3   =  REGION RECORD                                 *            
*     QUESTOR+4   =  OFFICE RECORD                                 *            
*     QUESTOR+5   =  Y  =  SALESPERSON SETUP                       *            
*                    B  =  SALESPERSON SETUP + SALESPERSON RECORD  *            
*     QUESTOR+6   =  BUDGET RECORD                                 *            
*     QUESTOR+7   =  END OF MONTH RECORD                           *            
*     QUESTOR+8   =  DAYPART RECORD                                *            
*     QUESTOR+9   =  SDD RECORD                                    *            
*     QUESTOR+10  =  COMMISSION RECORD                             *            
*     QUESTOR+11  =  C  =  CONTRACT RECORD                         *            
*                    B  =  BUY      RECORD                         *            
*                    +  =  CONTRACT + BUY RECORD                   *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RESW02   CSECT                                                                  
         NMOD1 0,**RESW**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
SWXIT    XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         GOTO1 =A(INITIAL),DMCB,(RC)                                            
*                                                                               
         GOTO1 STATPROC,DMCB,(RC)  PROCESS STATION RECORDS                      
*                                                                               
         GOTO1 REGNPROC,DMCB,(RC)  PROCESS REGION RECORDS                       
*                                                                               
         GOTO1 OFFCPROC,DMCB,(RC)  PROCESS OFFICE RECORDS                       
*                                                                               
         GOTO1 SALEPROC,DMCB,(RC)  PROCESS S/P    RECORDS                       
*                                                                               
         GOTO1 BUDGPROC,DMCB,(RC)  PROCESS BUDGET RECORDS                       
*                                                                               
         GOTO1 EOMPROC,DMCB,(RC)   PROCESS EOM    RECORDS                       
*                                                                               
         GOTO1 DYPTPROC,DMCB,(RC)  PROCESS DAYPRT RECORDS                       
*                                                                               
         GOTO1 DYDFPROC,DMCB,(RC)  PROCESS DAYPART DEFINITION RECORDS           
*                                                                               
         GOTO1 COMMPROC,DMCB,(RC)  PROCESS COMMISSION RECORDS                   
*                                                                               
         GOTO1 CONTPROC,DMCB,(RC)  PROCESS CONTRACT RECORDS                     
*                                                                               
         GOTO1 =A(BUYPROC),DMCB,(RC)   PROCESS BUY RECORDS                      
*                                                                               
         GOTO1 =A(DISPTOTS),DMCB,(RC)  DISPLAY TOTALS FOR RUN                   
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         B     SWXIT               EXIT                                         
         EJECT                                                                  
*                                                                               
******************************************************************              
*  STATPROC:  SCAN DI AND HN STATION RECORDS.  CONSTRUCT TABLE   *              
*     BASED ON DSECT STADSECT.  THEN RESCAN TABLE, SELECTING     *              
*     APPROPRIATE RECORDS FOR OUTPUT.                            *              
******************************************************************              
*                                                                               
STATPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R7,ASTNAREA         A(STATION WORK TABLE)                        
         USING STADSECT,R7         SET WORK SPACE DEFINITION                    
         LA    R3,REC              IO+PUT AREA                                  
         USING RSTAREC,R3          SET RECORD DEFINITION                        
*                                     OVER IO AREA                              
*                                                                               
         ST    R7,STRTSRCH         SAVE A(START SEARCH)                         
         XC    KEY,KEY             SET UP STATION KEY FOR DI                    
         MVI   KEY,2                                                            
*                                                                               
*   RETRIEVE ALL STATION RECORDS FOR 'DI', AND TABLE.  KEEP THE                 
*      ADDRESS OF THE NEXT AVAILABLE SLOT IN THE TABLE.                         
*                                                                               
         MVC   KEY+20(2),=C'DI'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     STAT0040                                                         
STAT0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
STAT0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     STATION RECORD/SAME COMPANY?                 
         BNE   STAT0100            NO  - GO PROCESS HN                          
         GOTO1 GETRECRD            YES - RETRIEVE THE RECORD                    
         MVC   STADCALL,RSTAKSTA   INSERT STATION CALLS                         
         MVC   STADDIJN,RSTASTRT   INSERT JOIN DATE                             
         MVC   STADDILV,RSTAEND    INSERT LEAVE DATE                            
         MVC   STADDIGP,RSTAGRUP   INSERT GROUP/SUBGROUP                        
         MVC   STADDICL,RSTACLDT   INSERT CLOSE DATE                            
         MVC   STADDIDA,KEY+28     INSERT LINK ADDRESS                          
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         ST    R7,NEXTAREA         SAVE A(NEXT OPEN SLOT)                       
         B     STAT0020            GO BACK FOR NEXT                             
STAT0100 EQU   *                                                                
*                                                                               
*   RETRIEVE ALL STATION RECORDS FOR 'HN'.  SCAN TABLE TO SEE IF                
*      STATION IS ALREADY THERE.  IF IT IS, ADD 'HN' INFO INTO                  
*      SLOT WITH 'DI' INFO.  IF NOT THERE, INSERT THE STATION                   
*      INTO NEXT AVAILABLE SLOT.  TO ENSURE THAT ENTIRE TABLE                   
*      IS NOT SEARCHED EACH TIME, KEEP POINTER TO START ENTRY                   
*      FOR SEARCH.                                                              
*                                                                               
         XC    KEY,KEY             SET UP STATION KEY FOR HN                    
         MVI   KEY,2                                                            
         MVC   KEY+20(2),=C'HN'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     STAT0140                                                         
STAT0120 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
STAT0140 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     STATION RECORD/SAME COMPANY?                 
         BNE   STAT0200            NO  - GO GENERATE RECORDS                    
         GOTO1 GETRECRD            YES - RETRIEVE THE RECORD                    
         BAS   RE,FINDSTAT         LOOK FOR STATION IN TABLE                    
         BZ    STAT0120            FOUND - DATA INSERTED                        
*                                     GO BACK FOR NEXT                          
*                                  NOT FOUND - INSERT INFO IN NEXT SLOT         
         L     R7,NEXTAREA         SET A(NEXT OPEN SLOT)                        
         MVC   STADCALL,RSTAKSTA   INSERT STATION CALLS                         
         MVC   STADHNJN,RSTASTRT   INSERT JOIN DATE                             
         MVC   STADHNLV,RSTAEND    INSERT LEAVE DATE                            
         MVC   STADHNGP,RSTAGRUP   INSERT GROUP/SUBGROUP                        
         MVC   STADHNCL,RSTACLDT   INSERT CLOSE DATE                            
         MVC   STADHNDA,KEY+28     INSERT LINK ADDRESS                          
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         ST    R7,NEXTAREA         SAVE A(NEXT OPEN SLOT)                       
         B     STAT0120            GO BACK FOR NEXT                             
STAT0200 EQU   *                                                                
         L     R7,ASTNAREA         SET A(START OF STATION TABLE)                
STAT0220 EQU   *                                                                
         XC    FLAGBYTS,FLAGBYTS   RESET ALL FLAGS                              
         XC    KEY,KEY             CLEAR KEY AREA                               
         XC    REC-4(4),REC-4      CLEAR PUT LENGTH                             
         CLC   =C'DISPTABL',QUESTOR                                             
         BNE   STAT0222            NO SPECIAL DISPLAY                           
         MVC   P+20(STADLEN),0(R7)  DISPLAY TABLE ENTRY                         
         MVC   P+1(12),=C'TABLE ENTRY:'                                         
         GOTO1 REPORT                                                           
STAT0222 EQU   *                                                                
         OC    STADCALL,STADCALL   ANY ENTRY?                                   
         BZ    STAT0400            NO  - STATION PROCESSING COMPLETE            
*                                     SHOW GROUP TABLE                          
         OC    STADHNDA,STADHNDA   'HN' STATION?                                
         BNZ   STAT0260            YES                                          
*                                  NO  - GEN REC FROM 'DI'                      
         MVC   KEY+28(4),STADDIDA  INSERT DISK ADDRESS                          
         GOTO1 GETRECRD            RETRIEVE STATION RECORD                      
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
****>>   GOTO1 DISPPUT,DMCB,(RC)                                                
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
****>>   GOTO1 DISPSTAT,DMCB,(RC),(R7),1                                        
*                                  DISPLAY STATION INFORMATION                  
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         B     STAT0220            GO BACK FOR NEXT                             
STAT0260 EQU   *                                                                
         OC    STADDIDA,STADDIDA   'HN' + 'DI'?                                 
         BNZ   STAT0300            YES - COMPARE AND GEN                        
*                                  NO  - GEN REC FROM 'HN'                      
         MVC   KEY+28(4),STADHNDA  INSERT DISK ADDRESS                          
         GOTO1 GETRECRD            RETRIEVE STATION RECORD                      
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
****>>   GOTO1 DISPPUT,DMCB,(RC)                                                
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
****>>   GOTO1 DISPSTAT,DMCB,(RC),(R7),2                                        
*                                  DISPLAY STATION INFORMATION                  
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         B     STAT0220            GO BACK FOR NEXT                             
STAT0300 EQU   *                                                                
         BAS   RE,STATDATE         ESTABLISH DATES FOR STATION                  
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR,C'Y'        DISPLAY STATION DATA?                        
         BNE   STAT0320            NOT SPECIAL REQUEST                          
         MVC   P+1(15),=C'STATION RECORD:'                                      
         GOTO1 REPORT                                                           
****>>   GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 DISPSTAT,DMCB,(RC),(R7),3                                        
*                                  DISPLAY STATION INFORMATION                  
         GOTO1 XTRADISP,DMCB,(RC),(R7)                                          
STAT0320 EQU   *                                                                
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         B     STAT0220            GO BACK FOR NEXT                             
STAT0400 EQU   *                                                                
         CLI   QUESTOR+1,C'Y'      DISPLAY GRP/COMM/BUD?                        
         BNE   STAT0900            NO SPECIAL DISPLAY                           
         L     RF,AGRPAREA         DISPLAY GROUP/SUBGROUP AREA                  
         MVC   P+1(13),=C'GROUP/SUBGRP:'                                        
         MVC   P+20(50),0(RF)                                                   
         GOTO1 REPORT                                                           
         L     R3,ACOMAREA         DISPLAY COMM/BUDGET AREA                     
         LA    R1,7                LOOP CONTROL                                 
         MVC   P+1(17),=C'COMM/BUDGET AREA:'                                    
         GOTO1 REPORT                                                           
STAT0440 EQU   *                                                                
         MVC   P+1(70),0(R3)                                                    
         GOTO1 REPORT                                                           
         LA    R3,70(R3)           BUMP TO NEXT SEGMENT                         
         BCT   R1,STAT0440                                                      
STAT0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  FINDSTAT:  LOOK IN TABLE TO SEE IF 'HN' STATION ALSO EXISTS   *              
*     FOR 'DI'.  IF SO, ADD INFO TO 'DI' ENTRY.  USE 'STRTSRCH'  *              
*     TO LIMIT SEARCH NEEDS.                                     *              
******************************************************************              
*                                                                               
FINDSTAT NTR1                                                                   
         L     R7,STRTSRCH         SET A(START OF TABLE SEARCH)                 
*                                     'USING' STILL IN EFFECT                   
FIND0020 EQU   *                                                                
         CLC   STADCALL,RSTAKSTA   TABLE VS STATION RECORD                      
         BE    FIND0060            FOUND - INSERT HN DATA INTO SLOT             
         BH    FIND0040            TABLE > STATION RECORD                       
         ST    R7,STRTSRCH         STATION RECORD > TABLE                       
*                                     NEXT ITEM CAN ONLY BE FROM                
*                                     THIS POINT OUT: SAVE STRTSRCH             
         LA    R7,STADLEN(R7)                                                   
         L     RF,NEXTAREA         CHECK FOR END OF TABLE                       
         CR    R7,RF               END REACHED?                                 
         BNE   FIND0020            NO                                           
FIND0040 EQU   *                                                                
         LTR   RC,RC               SET CC NOT = ZERO                            
         B     FIND0080                                                         
FIND0060 EQU   *                                                                
         MVC   STADHNJN,RSTASTRT   INSERT JOIN DATE                             
         MVC   STADHNLV,RSTAEND    INSERT LEAVE DATE                            
         MVC   STADHNGP,RSTAGRUP   INSERT GROUP/SUBGROUP                        
         MVC   STADHNCL,RSTACLDT   INSERT CLOSE DATE                            
         MVC   STADHNDA,KEY+28     INSERT LINK ADDRESS                          
         CLI   QUESTOR+2,C'Y'      TABLE MATCH DISPLAY?                         
         BNE   FIND0062            NO                                           
         MVC   P+1(12),=C'TABLE MATCH:'                                         
         MVC   P+15(5),RSTAKSTA                                                 
         MVC   P+22(5),STADCALL                                                 
         ST    R7,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,P+30,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
FIND0062 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
FIND0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  STATDATE:  ESTABLISH DATES FOR STATION                        *              
*                                                                *              
******************************************************************              
*                                                                               
STATDATE NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY                                    
         XC    REC-4(4),REC-4                                                   
         OC    STADDILV,STADDILV   DI LEAVE DATE?                               
         BNZ   STDA0100            YES                                          
         OC    STADHNLV,STADHNLV   NO  - HN LEAVE DATE?                         
         BNZ   STDA0200            YES                                          
*                                                                               
*                                  NEITHER DI NOR HN LEFT                       
*                                                                               
         CLC   STADDIJN,STADHNJN   COMPARE JOIN DATES                           
         BH    STDA0020            HN JOINED EARLIER                            
*                                  DI JOINED EARLIER OR                         
*                                     AT SAME TIME                              
         MVI   BOTHOPEN,C'D'       SET: BOTH OPEN, USED DI                      
         MVC   KEY+28(4),STADDIDA  INSERT DISK ADDRESS                          
         B     STDA0040                                                         
STDA0020 EQU   *                                                                
         MVI   BOTHOPEN,C'H'       SET: BOTH OPEN, USED HN                      
         MVC   KEY+28(4),STADHNDA  INSERT DISK ADDRESS                          
STDA0040 EQU   *                                                                
         GOTO1 GETRECRD                                                         
         MVC   SAVEGRP,RSTAGRUP    SAVE GROUP/SUBGROUP                          
*                                     OF RECORD USED AS OUTPUT                  
         GOTO1 TABLSTAT,DMCB,(RC),(R3)                                          
*                                  TABLE FOR COMM/BUDGET USE                    
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         B     STDA0900            EXIT                                         
*                                                                               
STDA0100 EQU   *                   DI HAS LEAVE DATE                            
         OC    STADHNLV,STADHNLV   HN LEAVE DATE?                               
         BNZ   STDA0300            YES - BOTH HAVE LEFT                         
*                                  NO  - HN IS CURRENT                          
         MVC   KEY+28(4),STADHNDA  INSERT DISK ADDRESS                          
         GOTO1 GETRECRD            GET HN STATION RECORD                        
         MVC   SAVEGRP,RSTAGRUP    SAVE GROUP/SUBGROUP                          
*                                     OF RECORD USED AS OUTPUT                  
         GOTO1 TABLSTAT,DMCB,(RC),(R3)                                          
*                                  TABLE FOR COMM/BUDGET USE                    
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         CLC   STADHNJN,STADDILV   HN JOIN = DI LEAVE?                          
         BE    STDA0120            YES - CONSIDER CONSECUTIVE                   
*                                     USE DI JOIN DATE                          
         GOTO1 DATCON,DMCB,(3,STADDILV),(0,DATEWORK)                            
*                                  CONVERT DI LEAVE DATE TO YYMMDD              
         LA    RF,1                                                             
         GOTO1 ADDAY,DMCB,DATEWORK,DATEWORK+6,(RF)                              
         CLC   DATEWORK+6(3),STADHNJN                                           
*                                  DAY AFTER DI LEAVE:  IS IT                   
*                                     EQUAL TO HN JOIN DATE?                    
         BE    STDA0120            YES - USE DI JOIN DATE                       
         MVC   RSTASTRT,STADHNJN   NO  - INSERT HN JOIN DATE                    
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         MVI   HNOPEN,C'H'         SET 'HN CURRENT: HN START'                   
         B     STDA0900            EXIT                                         
*                                                                               
*                                  DATES ARE CONTIGUOUS -                       
STDA0120 EQU   *                      USE DI JOIN DATE                          
         MVC   RSTASTRT,STADDIJN   INSERT DI JOIN DATE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         MVI   HNOPEN,C'D'         SET 'HN CURRENT: DI START'                   
         B     STDA0900            EXIT                                         
STDA0200 EQU   *                   HN LEFT - DI IS CURRENT                      
*                                     USE DI RECORD                             
         MVC   KEY+28(4),STADDIDA  INSERT DISK ADDRESS                          
         GOTO1 GETRECRD            GET DI STATION RECORD                        
         MVC   SAVEGRP,RSTAGRUP    SAVE GROUP/SUBGROUP                          
*                                     OF RECORD USED AS OUTPUT                  
         GOTO1 TABLSTAT,DMCB,(RC),(R3)                                          
*                                  TABLE FOR COMM/BUDGET USE                    
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         CLC   STADDIJN,STADHNLV   DI JOIN = HN LEAVE?                          
         BE    STDA0220            YES - CONSIDER CONSECUTIVE                   
*                                     USE HN JOIN DATE                          
         GOTO1 DATCON,DMCB,(3,STADHNLV),(0,DATEWORK)                            
*                                  CONVERT HN LEAVE DATE TO YYMMDD              
         LA    RF,1                                                             
         GOTO1 ADDAY,DMCB,DATEWORK,DATEWORK+6,(RF)                              
         CLC   DATEWORK+6(3),STADDIJN                                           
*                                  DAY AFTER HN LEAVE:  IS IT                   
*                                     EQUAL TO DI JOIN DATE?                    
         BE    STDA0220            YES - USE HN JOIN DATE                       
         MVC   RSTASTRT,STADDIJN   NO  - INSERT DI JOIN DATE                    
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         MVI   DIOPEN,C'D'         SET 'DI CURRENT: DI START'                   
         B     STDA0900            EXIT                                         
*                                                                               
*                                  DATES ARE CONTIGUOUS -                       
STDA0220 EQU   *                      USE HN JOIN DATE                          
         MVC   RSTASTRT,STADHNJN   INSERT HN JOIN DATE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         MVI   DIOPEN,C'H'         SET 'DI CURRENT: HN START'                   
         B     STDA0900            EXIT                                         
STDA0300 EQU   *                                                                
         MVC   KEY+28(4),STADDIDA  INSERT DISK ADDRESS FOR DI                   
         MVI   BOTHLEFT,C'D'       SET 'DI RECORD USED'                         
         CLC   STADDILV,STADHNLV   COMPARE LEAVE DATES                          
         BNL   STDA0320            USE DI RECORD                                
         MVC   KEY+28(4),STADHNDA  INSERT DISK ADDRESS FOR HN                   
         MVI   BOTHLEFT,C'H'       SET 'HN RECORD USED'                         
STDA0320 EQU   *                                                                
         GOTO1 GETRECRD            GET DI STATION RECORD                        
         MVC   SAVEGRP,RSTAGRUP    SAVE GROUP/SUBGROUP                          
*                                     OF RECORD USED AS OUTPUT                  
         GOTO1 TABLSTAT,DMCB,(RC),(R3)                                          
*                                  TABLE FOR COMM/BUDGET USE                    
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         MVC   RSTASTRT,STADDIJN   INSERT DI JOIN DATE                          
         CLC   STADDIJN,STADHNJN   COMPARE JOIN DATES                           
         BL    STDA0340            USE DI: EARLIER                              
         MVC   RSTASTRT,STADHNJN   INSERT HN JOIN DATE                          
STDA0340 EQU   *                                                                
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         B     STDA0900            EXIT                                         
STDA0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
                                                                                
*                                                                               
******************************************************************              
*  DISPSTAT:  DISPLAY RESULT OF STATION UPDATE.                  *              
*     DMCB+4   =   SOURCE OF DATA                                *              
*          1   =   DI  FILE                                      *              
*          2   =   HN  FILE                                      *              
*          3   =   DI+HN                                         *              
*                                                                *              
******************************************************************              
*                                                                               
DISPSTAT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R7,4(R1)            RESET A(TABLE)                               
         USING STADSECT,R7                                                      
         MVC   P+1(5),STADCALL                                                  
         MVC   P+15(08),=C'SOURCE: '                                            
         MVC   P+23(05),=C'DI+HN'  SET TO 'FROM DI+HN'                          
         MVI   DBLSPACE,C'N'       SET DOUBLE SPACE INDICATOR                   
         CLI   DMCB+11,X'1'        FROM DI?                                     
         BNE   DIAT0020            NO                                           
         MVC   P+23(05),=C'DI   '  YES                                          
         MVI   DBLSPACE,C'Y'       SET DOUBLE SPACE INDICATOR                   
         B     DIAT0060                                                         
DIAT0020 EQU   *                                                                
         CLI   DMCB+11,X'2'        FROM HN?                                     
         BNE   DIAT0060            NO  - CONSIDER IT A '3'                      
         MVC   P+23(05),=C'HN   '  YES                                          
         MVI   DBLSPACE,C'Y'       SET DOUBLE SPACE INDICATOR                   
DIAT0060 EQU   *                                                                
         MVC   P+30(04),=C'JOIN'                                                
         MVC   P+45(04),=C'LEFT'                                                
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(5,P+36)                                
         OC    RSTAEND,RSTAEND     ANY END DATE?                                
         BZ    DIAT0080            NO                                           
         GOTO1 DATCON,DMCB,(3,RSTAEND),(5,P+50)                                 
DIAT0080 EQU   *                                                                
         GOTO1 REPORT                                                           
         CLI   DBLSPACE,C'N'       FROM DI+HN?                                  
         BE    DIAT0100            YES - DON'T DOUBLE SPACE                     
         GOTO1 REPORT                                                           
DIAT0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R7                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  XTRADISP:  DISPLAY ADDITIONAL INFO FOR STATION RECORDS WHEN   *              
*     TAKEN FROM BOTH DI AND HN RECORDS.                         *              
*                                                                *              
******************************************************************              
*                                                                               
XTRADISP NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R7,4(R1)            RESET A(TABLE)                               
         USING STADSECT,R7                                                      
         OC    BOTHOPEN,BOTHOPEN   BOTH STATIONS OPEN?                          
         BZ    XTRA0020            NO                                           
         MVC   P+1(10),=C'DI JOIN:  '                                           
         GOTO1 DATCON,DMCB,(3,STADDIJN),(5,P+11)                                
         MVC   P+20(10),=C'HN JOIN:  '                                          
         GOTO1 DATCON,DMCB,(3,STADHNJN),(5,P+30)                                
         GOTO1 REPORT                                                           
         B     XTRA0150            EXIT                                         
XTRA0020 EQU   *                                                                
         OC    HNOPEN,HNOPEN       HN CURRENT?                                  
         BZ    XTRA0040            NO                                           
*                                  YES - HN USED TO OUTPUT RECORD               
         MVC   P+1(10),=C'DI JOIN:  '                                           
         GOTO1 DATCON,DMCB,(3,STADDIJN),(5,P+11)                                
         MVC   P+20(10),=C'DI LEFT:  '                                          
         GOTO1 DATCON,DMCB,(3,STADDILV),(5,P+30)                                
         MVC   P+40(10),=C'HN JOIN:  '                                          
         GOTO1 DATCON,DMCB,(3,STADHNJN),(5,P+50)                                
         MVC   P+70(02),=C'HN'                                                  
         CLI   HNOPEN,C'H'         HN START DATE USED?                          
         BE    XTRA0030                                                         
         MVC   P+70(02),=C'DI'                                                  
XTRA0030 EQU   *                                                                
         MVC   P+73(15),=C'START DATE USED'                                     
         GOTO1 REPORT                                                           
         B     XTRA0150            EXIT                                         
XTRA0040 EQU   *                                                                
         OC    DIOPEN,DIOPEN       DI CURRENT?                                  
         BZ    XTRA0100            NO                                           
*                                  YES - DI USED TO OUTPUT RECORD               
         MVC   P+1(10),=C'HN JOIN:  '                                           
         GOTO1 DATCON,DMCB,(3,STADHNJN),(5,P+11)                                
         MVC   P+20(10),=C'HN LEFT:  '                                          
         GOTO1 DATCON,DMCB,(3,STADHNLV),(5,P+30)                                
         MVC   P+40(10),=C'DI JOIN:  '                                          
         GOTO1 DATCON,DMCB,(3,STADDIJN),(5,P+50)                                
         MVC   P+70(02),=C'DI'                                                  
         CLI   DIOPEN,C'D'         DI START DATE USED?                          
         BE    XTRA0060                                                         
         MVC   P+70(02),=C'HN'                                                  
XTRA0060 EQU   *                                                                
         MVC   P+73(15),=C'START DATE USED'                                     
         GOTO1 REPORT                                                           
         B     XTRA0150            EXIT                                         
XTRA0100 EQU   *                                                                
         OC    BOTHLEFT,BOTHLEFT   BOTH STATIONS LEFT?                          
         BZ    XTRA0900            NO  - FINISHED                               
         MVC   P+1(10),=C'HN JOIN:  '                                           
         GOTO1 DATCON,DMCB,(3,STADHNJN),(5,P+11)                                
         MVC   P+20(10),=C'HN LEFT:  '                                          
         GOTO1 DATCON,DMCB,(3,STADHNLV),(5,P+30)                                
         MVC   P+40(10),=C'DI JOIN:  '                                          
         GOTO1 DATCON,DMCB,(3,STADDIJN),(5,P+50)                                
         MVC   P+60(10),=C'DI LEFT:  '                                          
         GOTO1 DATCON,DMCB,(3,STADDILV),(5,P+70)                                
         GOTO1 REPORT                                                           
         B     XTRA0150                                                         
XTRA0150 EQU   *                                                                
         MVC   P+1(09),=C'DI GROUP:'                                            
         MVC   P+21(09),=C'HN GROUP:'                                           
         MVC   P+12(2),STADDIGP                                                 
         MVC   P+32(2),STADHNGP                                                 
         CLC   STADDIGP,STADHNGP   SAME GROUPS?                                 
         BE    XTRA0180            YES                                          
         MVC   P+50(28),=C'GROUP/SUBGROUP DIFFERENCE!!!'                        
         MVC   P+80(2),SAVEGRP                                                  
         MVC   P+83(13),=C'WILL BE USED!'                                       
         L     RF,ANEXTGRP         INSERT GROUP/SUBGROUP INTO TABLE             
         USING GRPDSECT,RF                                                      
         MVC   GRPDCALL,STADCALL   INSERT CALL LETTERS                          
         MVC   GRPDGRUP,SAVEGRP    INSERT NEW GROUP/SUBGROUP                    
         LA    RF,GRPDLEN(RF)      BUMP TO NEXT SLOT                            
         ST    RF,ANEXTGRP                                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
XTRA0180 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(09),=C'DI CLOSE:'                                            
         MVC   P+21(09),=C'HN CLOSE:'                                           
         GOTO1 DATCON,DMCB,(3,STADDICL),(6,P+12)                                
         GOTO1 DATCON,DMCB,(3,STADHNCL),(6,P+32)                                
         GOTO1 REPORT                                                           
XTRA0900 EQU   *                                                                
         GOTO1 REPORT              ADD A LINE FOR SPACING                       
         XIT1                                                                   
         EJECT                                                                  
         DROP  R7                                                               
*                                                                               
******************************************************************              
*  TABLSTAT: INSERT STATION/REP CODE INTO TABLE FOR USE IN       *              
*        GENERATING COMMISSION AND BUDGET RECORDS.               *              
******************************************************************              
*                                                                               
TABLSTAT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(STATION RECORD)                      
         USING RSTAREC,R3                                                       
         L     R2,ANEXTCOM         SET A(NEXT OPEN SLOT)                        
         USING COMDSECT,R2                                                      
         MVC   COMDCALL,RSTAKSTA   INSERT CALL LETTERS                          
*****>   MVC   COMDREPB,RSTAKREP   INSERT REP CODE FOR BUDGET                   
*********************************************************************           
*  TO DROP ALL BUDGET RECS FOR STATIONS WHICH EXIST IN BOTH COMPANIES,          
*     COMMENT OUT LINE IMMEDIATELY ABOVE, ACTIVATE LINE IMMEDIATELY             
*     BELOW, AND COMMENT OUT FORCED SETTING OF BUDGET REPS                      
*     BELOW, ALSO.                                                              
*                                                                               
         MVC   COMDREPB(1),RSTAKREP                                             
*                                  INSERT REP CODE FOR BUDGET                   
*                                                                               
*  TEMPORARILY, ONLY THE FIRST CHARACTER OF THE REP CODE IS INSERTED            
*                                                                               
*********************************************************************           
         MVC   COMDREPC,RSTAKREP   INSERT REP CODE FOR COMMISSION               
*********************************************************************           
*  TO DROP ALL COMM RECS FOR STATIONS WHICH EXIST IN BOTH COMPANIES,            
*     COMMENT OUT LINE IMMEDIATELY ABOVE, ACTIVATE LINE IMMEDIATELY             
*     BELOW, AND COMMENT OUT FORCED SETTING OF COMMISSION REPS                  
*     BELOW, ALSO.                                                              
*                                                                               
*****>   MVC   COMDREPC(1),RSTAKREP                                             
*                                  INSERT REP CODE FOR COMMISSION               
*                                                                               
*  TEMPORARILY, ONLY THE FIRST CHARACTER OF THE REP CODE IS INSERTED            
*                                                                               
*********************************************************************           
         CLC   COMDCALL,=C'WKSZF'  OVERRIDE THE VALUE                           
         BNE   TSTA0020            NO                                           
****     MVC   COMDREPB,=C'HN'     YES - FORCE 'HN' FOR BUDGET                  
         MVC   COMDREPC,=C'HN'     YES - FORCE 'HN' FOR COMMISSION              
TSTA0020 EQU   *                                                                
         CLC   COMDCALL,=C'KEWBF'  OVERRIDE THE VALUE                           
         BNE   TSTA0040            NO                                           
****     MVC   COMDREPB,=C'HN'     YES - FORCE 'HN' FOR BUDGET                  
         MVC   COMDREPC,=C'HN'     YES - FORCE 'HN' FOR COMMISSION              
TSTA0040 EQU   *                                                                
         CLC   COMDCALL,=C'KVOOC'  OVERRIDE THE VALUE                           
         BNE   TSTA0060            NO                                           
****     MVC   COMDREPB,=C'DI'     YES - FORCE 'DI' FOR BUDGET                  
         MVC   COMDREPC,=C'DI'     YES - FORCE 'DI' FOR COMMISSION              
TSTA0060 EQU   *                                                                
         CLC   COMDCALL,=C'WMIXF'  OVERRIDE THE VALUE                           
         BNE   TSTA0080            NO                                           
****     MVC   COMDREPB,=C'DI'     YES - FORCE 'DI' FOR BUDGET                  
TSTA0080 EQU   *                                                                
         CLC   COMDCALL,=C'KOLLF'  OVERRIDE THE VALUE                           
         BNE   TSTA0100            NO                                           
         MVC   COMDREPC,=C'DI'     YES - FORCE 'DI' FOR COMMISSION              
TSTA0100 EQU   *                                                                
         CLC   COMDCALL,=C'KVOOA'  OVERRIDE THE VALUE                           
         BNE   TSTA0120            NO                                           
         MVC   COMDREPC,=C'DI'     YES - FORCE 'DI' FOR COMMISSION              
TSTA0120 EQU   *                                                                
         CLC   COMDCALL,=C'KVOOF'  OVERRIDE THE VALUE                           
         BNE   TSTA0140            NO                                           
         MVC   COMDREPC,=C'DI'     YES - FORCE 'DI' FOR COMMISSION              
TSTA0140 EQU   *                                                                
         CLC   COMDCALL,=C'WCKZF'  OVERRIDE THE VALUE                           
         BNE   TSTA0160            NO                                           
         MVC   COMDREPC,=C'DI'     YES - FORCE 'DI' FOR COMMISSION              
TSTA0160 EQU   *                                                                
         CLC   COMDCALL,=C'WECKA'  OVERRIDE THE VALUE                           
         BNE   TSTA0180            NO                                           
         MVC   COMDREPC,=C'HN'     YES - FORCE 'HN' FOR COMMISSION              
TSTA0180 EQU   *                                                                
         CLC   COMDCALL,=C'WECKC'  OVERRIDE THE VALUE                           
         BNE   TSTA0200            NO                                           
         MVC   COMDREPC,=C'HN'     YES - FORCE 'HN' FOR COMMISSION              
TSTA0200 EQU   *                                                                
         CLC   COMDCALL,=C'WJYEF'  OVERRIDE THE VALUE                           
         BNE   TSTA0220            NO                                           
         MVC   COMDREPC,=C'HN'     YES - FORCE 'HN' FOR COMMISSION              
TSTA0220 EQU   *                                                                
         CLC   COMDCALL,=C'WMXPF'  OVERRIDE THE VALUE                           
         BNE   TSTA0240            NO                                           
         MVC   COMDREPC,=C'HN'     YES - FORCE 'HN' FOR COMMISSION              
TSTA0240 EQU   *                                                                
         CLC   COMDCALL,=C'WQOKF'  OVERRIDE THE VALUE                           
         BNE   TSTA0260            NO                                           
         MVC   COMDREPC,=C'HN'     YES - FORCE 'HN' FOR COMMISSION              
TSTA0260 EQU   *                                                                
         LA    R2,COMDLEN(R2)      BUMP TO NEXT SLOT                            
         ST    R2,ANEXTCOM         STORE IT                                     
         XC    0(COMDLEN,R2),0(R2)                                              
*                                  CLEAR NEXT ENTRY IN TABLE                    
         XIT1                                                                   
*                                                                               
         DROP  R3,R2                                                            
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  REGNPROC:  BRING OVER ALL DI REGION RECORDS.                  *              
******************************************************************              
*                                                                               
REGNPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RREGREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,3               SET UP REGION KEY FOR DI                     
         MVC   KEY+23(2),=C'DI'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     REGN0040                                                         
REGN0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
REGN0040 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     REGION RECORD/SAME COMPANY?                  
         BNE   REGN0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RREGLEN    INSERT LENGTH FOR PUT                        
         MVC   RREGKREP,NEWREP     INSERT NEW REP CODE                          
*        MVC   RREGTHQ(2),NEWREP   INSERT NEW REP CODE INTO TV HQ               
*        MVC   RREGRHQ(2),NEWREP   INSERT NEW REP CODE INTO RADIO HQ            
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+3,C'Y'      DISPLAY REGION RECORDS?                      
         BNE   REGN0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'REGION RECORD:'                                       
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     REGN0020            GO BACK FOR NEXT                             
REGN0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  OFFCPROC:  WILL BRING OVER ALL DI OFFICE RECORDS.             *              
******************************************************************              
*                                                                               
OFFCPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING ROFFREC,R3                                                       
         MVI   KEYTYPE,4                                                        
OFFC0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP OFFICE KEY FOR DI                     
         MVC   KEY+23(2),=C'DI'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     OFFC0040                                                         
OFFC0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
OFFC0040 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     OFFICE RECORD/SAME COMPANY?                  
         BNE   OFFC0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),ROFFLEN    INSERT LENGTH FOR PUT                        
         MVC   ROFFKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY OFFICE RECORD?                       
         BNE   OFFC0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'OFFICE RECORD:'                                       
         MVC   P+20(04),ROFFKREP                                                
         MVI   P+24,C'/'                                                        
         MVC   P+25(2),KEY+23                                                   
         GOTO1 REPORT                                                           
*****>   GOTO1 DISPPUT,DMCB,(RC)                                                
*****>   GOTO1 REPORT              INSERT A BLANK LINE                          
         B     OFFC0020            GO BACK FOR NEXT                             
OFFC0100 EQU   *                                                                
         CLI   KEYTYPE,X'44'       SECONDARY OFFICE RECORD DONE?                
         BE    OFFC0120            YES - FINISHED                               
         MVI   KEYTYPE,X'44'       NO  - DO IT                                  
         B     OFFC0010            GO BACK                                      
OFFC0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SALEPROC:  WILL BRING OVER ALL DI S/P    RECORDS.             *              
******************************************************************              
*                                                                               
SALEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RSALREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,6               SET UP S/P    KEY FOR DI                     
         MVC   KEY+22(2),=C'DI'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     SALE0040                                                         
SALE0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
SALE0040 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     S/P    RECORD/SAME COMPANY?                  
         BNE   SALE0100            NO  - NOW TABLE HN CODES                     
*                                     FOR TRANSLATING CONTRACTS                 
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+5,C'Y'      DISPLAY SALESPERSON RECORD?                  
         BNE   SALE0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'S/P    RECORD:'                                       
         MVC   P+20(05),RSALKREP                                                
         MVI   P+25,C'/'                                                        
         MVC   P+26(2),KEY+22                                                   
         GOTO1 REPORT                                                           
*****>   GOTO1 DISPPUT,DMCB,(RC)                                                
*****>   GOTO1 REPORT              INSERT A BLANK LINE                          
         B     SALE0020            GO BACK FOR NEXT                             
SALE0100 EQU   *                                                                
         L     R2,ANEXTSAL         SET A(NEXT SLOT FOR S/P)                     
         XC    KEY,KEY                                                          
         MVI   KEY,6               SET UP S/P    KEY FOR HN                     
         MVC   KEY+22(2),=C'HN'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     SALE0140                                                         
SALE0120 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
SALE0140 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     S/P    RECORD/SAME COMPANY?                  
         BNE   SALE0240            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   0(3,R2),RSALKSAL    INSERT SALESPERSON INTO TABLE                
         XC    3(9,R2),3(R2)       CLEAR TRANSLATE CODE + NEXT ENTRY            
         OC    RSALMRG,RSALMRG     ANY MERGE ENTRY?                             
         BZ    SALE0160            NO  - CREATE, AND DISPLAY MESSAGE            
         MVC   3(3,R2),RSALMRG     YES - INSERT TRANSLATE CODE                  
         CLI   QUESTOR+5,C'Y'      DISPLAY SALESPERSON SETUP?                   
         BE    SALE0145            YES                                          
         CLI   QUESTOR+5,C'B'      DISPLAY SALESPERSON SETUP + RECORD?          
         BNE   SALE0150            NO                                           
SALE0145 EQU   *                                                                
         MVC   P+1(22),=C'S/P TRANSLATE SET UP :'                               
         MVC   P+25(3),RSALKSAL    INSERT S/P CODE                              
         MVC   P+30(20),RSALNAME   INSERT NAME                                  
         MVC   P+55(3),=C'-->'                                                  
         MVC   P+60(3),RSALMRG     INSERT NEW S/P CODE                          
         GOTO1 REPORT              DISPLAY MESSAGE                              
         GOTO1 REPORT              DISPLAY BLANK LINE                           
SALE0150 EQU   *                                                                
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     INSERT NEW REP CODE                          
         MVC   SAVESALE,RSALKSAL   SAVE ORIGINAL SALESPERSON CODE               
         MVC   RSALKSAL,RSALMRG    INSERT NEW SALESPERSON CODE                  
         MVC   RSALMRG,SAVESALE    INSERT ORIGINAL SALESPERSON CODE             
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+5,C'B'      DISPLAY SALESPERSON RECORD?                  
         BNE   SALE0220            NO                                           
         GOTO1 DISPPUT,DMCB,(RC)   YES                                          
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     SALE0220                                                         
SALE0160 EQU   *                                                                
         MVC   P+1(22),=C'S/P TRANSLATE CREATED:'                               
         MVC   P+25(3),RSALKSAL    INSERT S/P CODE                              
         MVC   P+30(20),RSALNAME   INSERT NAME                                  
         MVC   P+55(3),=C'-->'                                                  
         MVC   P+60(1),NEWSPALF    NEW ALPHA CODE                               
         EDIT  NEWSPNUM,(2,P+61),FILL=0                                         
         MVC   3(3,R2),P+60        INSERT NEW CODE INTO TABLE                   
         GOTO1 REPORT              DISPLAY MESSAGE                              
         ZIC   RF,NEWSPNUM         BUMP NUMBER                                  
         LA    RF,1(RF)                                                         
         STC   RF,NEWSPNUM         STORE IT BACK                                
         CLI   NEWSPNUM,100        NEW ALPHA NEEDED?                            
         BNE   SALE0180            NO                                           
*                                                                               
*   THE FOLLOWING WILL WORK WELL ENOUGH FOR THIS APPLICATION, WHERE             
*      SLIGHTLY MORE THAN 100 NEW CODES ARE POSSIBLE AT MAX.                    
*                                                                               
         ZIC   RF,NEWSPALF         YES - BUMP THE CHARACTER                     
         LA    RF,1(RF)                                                         
         STC   RF,NEWSPALF         STORE IT BACK                                
SALE0180 EQU   *                                                                
         MVC   RSALKSAL,3(R2)      INSERT TABLE CODE INTO RECORD                
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+5,C'Y'      DISPLAY S/P RECORD?                          
         BNE   SALE0200            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'S/P    RECORD:'                                       
         MVC   P+20(05),RSALKREP                                                
         MVI   P+25,C'/'                                                        
         MVC   P+26(2),KEY+22                                                   
         GOTO1 REPORT                                                           
SALE0200 EQU   *                                                                
         GOTO1 REPORT              DISPLAY MESSAGE                              
SALE0220 EQU   *                                                                
         LA    R2,6(R2)            BUMP TO NEXT SLOT                            
         B     SALE0120            GO BACK FOR NEXT                             
SALE0240 EQU   *                                                                
         CLI   QUESTOR+5,C'Y'      DISPLAY S/P RECORD?                          
         BNE   SALE0280            NO                                           
         L     R2,ASALAREA         RESET A(S/P AREA)                            
         LA    R1,10               SET LOOP CONTROL                             
         MVC   P+1(10),=C'S/P TABLE:'                                           
         GOTO1 REPORT                                                           
SALE0260 EQU   *                                                                
         MVC   P+1(72),0(R2)                                                    
         GOTO1 REPORT                                                           
         LA    R2,72(R2)                                                        
         BCT   R1,SALE0260                                                      
SALE0280 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  BUDGPROC:  WILL BRING OVER ALL DI/HN BUDGET RECORDS.          *              
******************************************************************              
*                                                                               
BUDGPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RBUDREC,R3                                                       
         MVC   REPCODE,=C'DI'      PROCESS DI FIRST                             
BUDG0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'13'           SET UP BUDGET KEY                            
         MVC   KEY+16(2),REPCODE   INSERT REP CODE                              
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     BUDG0040                                                         
BUDG0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
BUDG0040 EQU   *                                                                
         CLC   KEY(18),KEYSAVE     BUDGET RECORD/SAME COMPANY?                  
         BNE   BUDG0100            NO  - FINISHED                               
         GOTO1 BUDGALT,DMCB,(RC)                                                
*                                  CHECK IF STATION/REP IS TO BE                
*                                     USED:  IF NOT, SKIP BEFORE READ           
         BZ    BUDG0060            CC = ZERO: SKIP THIS RECORD                  
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RBUDLEN    INSERT LENGTH FOR PUT                        
         MVC   RBUDKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+6,C'Y'      DISPLAY BUDGET RECORD?                       
         BNE   BUDG0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'BUDGET RECORD:'                                       
         MVC   P+20(11),RBUDKREP   INSERT BUDGET KEY                            
         MVI   P+31,C'/'                                                        
         MVC   P+32(2),KEY+16                                                   
         GOTO1 REPORT                                                           
****>    GOTO1 DISPPUT,DMCB,(RC)                                                
****>    GOTO1 REPORT              INSERT A BLANK LINE                          
         B     BUDG0020            GO BACK FOR NEXT                             
BUDG0060 EQU   *                                                                
         MVC   P+1(22),=C'BUDGET RECORD SKIPPED:'                               
         MVC   P+25(11),KEY+16                                                  
         MVI   P+36,C'/'                                                        
         MVC   P+37(2),KEY+16                                                   
         GOTO1 REPORT                                                           
         B     BUDG0020            GO BACK FOR NEXT                             
BUDG0100 EQU   *                                                                
         CLC   REPCODE,=C'HN'      HN PROCESSED?                                
         BE    BUDG0120            YES - FINISHED                               
         MVC   REPCODE,=C'HN'      NO  - PROCESS IT                             
         B     BUDG0010            GO BACK AND DO IT                            
BUDG0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  BUDGALT :  CHECK TABLE FOR STATION/REP CODE CHOICE.           *              
******************************************************************              
*                                                                               
BUDGALT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,ACOMAREA         A(COMM/BUDGET TABLE AREA)                    
         USING COMDSECT,R2                                                      
BALT0020 EQU   *                                                                
         OC    COMDCALL,COMDCALL   ANY TABLE ENTRY?                             
         BZ    BALT0100            NO  - EXIT CC NOT ZERO                       
         CLC   COMDCALL,KEY+20     STATION IN TABLE?                            
         BNE   BALT0040            NO  - CHECK NEXT                             
         CLC   COMDREPB,KEY+16     YES - SAME REP?                              
         BE    BALT0100            YES - EXIT CC NOT ZERO                       
         B     BALT0120            NO  - EXIT CC ZERO                           
BALT0040 EQU   *                                                                
         LA    R2,COMDLEN(R2)      BUMP TO NEXT SLOT                            
         B     BALT0020            GO BACK FOR NEXT                             
BALT0100 EQU   *                                                                
         LTR   RC,RC               SET CC NOT ZERO                              
         B     BALT0140                                                         
BALT0120 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
BALT0140 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  EOMPROC:   WILL BRING OVER ALL DI EOM    RECORDS.             *              
******************************************************************              
*                                                                               
EOMPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING REOMREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'18'           SET UP EOM    KEY FOR DI                     
         MVC   KEY+24(2),=C'DI'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     EOMP0040                                                         
EOMP0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
EOMP0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     EOM    RECORD/SAME COMPANY?                  
         BNE   EOMP0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),REOMLEN    INSERT LENGTH FOR PUT                        
         MVC   REOMKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+7,C'Y'      EOM RECORD DISPLAY?                          
         BNE   EOMP0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'EOM    RECORD:'                                       
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     EOMP0020            GO BACK FOR NEXT                             
EOMP0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DYPTPROC:  WILL BRING OVER ALL DI DAYPART RECORDS             *              
******************************************************************              
*                                                                               
DYPTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RDPTREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'24'           SET UP DAYPART KEY FOR DI                    
         MVC   KEY+24(2),=C'DI'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     DYPT0040                                                         
DYPT0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
DYPT0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     BUDGET RECORD/SAME COMPANY?                  
         BNE   DYPT0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RDPTLEN    INSERT LENGTH FOR PUT                        
         MVC   RDPTKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+8,C'Y'      DAYPART RECORD DISPLAY?                      
         BNE   DYPT0020            NOT SPECIAL REQUEST                          
         MVC   P+1(15),=C'DAYPART RECORD:'                                      
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     DYPT0020            GO BACK FOR NEXT                             
DYPT0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DYDFPROC:  WILL BRING OVER ALL DI DAYPART DEFINITION RECORDS  *              
******************************************************************              
*                                                                               
DYDFPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RSDDREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'26'           SET UP DAYPART DEFIN KEY FOR DI              
         MVC   KEY+20(2),=C'DI'                                                 
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     DYDF0040                                                         
DYDF0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
DYDF0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     BUDGET RECORD/SAME COMPANY?                  
         BNE   DYDF0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RSDDLEN    INSERT LENGTH FOR PUT                        
         MVC   RSDDKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+9,C'Y'      SDD RECORD DISPLAY?                          
         BNE   DYDF0020            NOT SPECIAL REQUEST                          
         MVC   P+1(11),=C'SDD RECORD:'                                          
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     DYDF0020            GO BACK FOR NEXT                             
DYDF0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  COMMPROC:  WILL BRING OVER ALL DI/HN COMMISSION RECORDS       *              
******************************************************************              
*                                                                               
COMMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RCOMREC,R3                                                       
         MVC   REPCODE,=C'DI'      PROCESS DI FIRST                             
COMM0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'29'           SET UP COMM   KEY                            
         MVC   KEY+11(2),REPCODE   INSERT REP CODE                              
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     COMM0040                                                         
COMM0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
COMM0040 EQU   *                                                                
         CLC   KEY(13),KEYSAVE     COMM   RECORD/SAME COMPANY?                  
         BNE   COMM0100            NO  - FINISHED                               
         GOTO1 COMMALT,DMCB,(RC)                                                
*                                  CHECK IF STATION/REP IS TO BE                
*                                     USED:  IF NOT, SKIP BEFORE READ           
         BZ    COMM0060            CC = ZERO: SKIP THIS RECORD                  
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RCOMLEN    INSERT LENGTH FOR PUT                        
         MVC   RCOMKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+10,C'Y'     COMM RECORD DISPLAY?                         
         BNE   COMM0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'COMM   RECORD:'                                       
         MVC   P+20(14),RCOMKREP   INSERT COMM   KEY                            
         MVI   P+34,C'/'                                                        
         MVC   P+35(2),KEY+11                                                   
         GOTO1 REPORT                                                           
****>    GOTO1 DISPPUT,DMCB,(RC)                                                
****>    GOTO1 REPORT              INSERT A BLANK LINE                          
         B     COMM0020            GO BACK FOR NEXT                             
COMM0060 EQU   *                                                                
         MVC   P+1(22),=C'COMM   RECORD SKIPPED:'                               
         MVC   P+25(14),KEY+11                                                  
         MVI   P+39,C'/'                                                        
         MVC   P+40(2),KEY+11                                                   
         GOTO1 REPORT                                                           
         B     COMM0020            GO BACK FOR NEXT                             
COMM0100 EQU   *                                                                
         CLC   REPCODE,=C'HN'      HN PROCESSED?                                
         BE    COMM0120            YES - FINISHED                               
         MVC   REPCODE,=C'HN'      NO  - PROCESS IT                             
         B     COMM0010            GO BACK AND DO IT                            
COMM0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  COMMALT :  CHECK TABLE FOR STATION/REP CODE CHOICE.           *              
******************************************************************              
*                                                                               
COMMALT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,ACOMAREA         A(COMM/BUDGET TABLE AREA)                    
         USING COMDSECT,R2                                                      
CALT0020 EQU   *                                                                
         OC    COMDCALL,COMDCALL   ANY TABLE ENTRY?                             
         BZ    CALT0100            NO  - EXIT CC NOT ZERO                       
         CLC   COMDCALL,KEY+13     STATION IN TABLE?                            
         BNE   CALT0040            NO  - CHECK NEXT                             
         CLC   COMDREPC,KEY+11     YES - SAME REP?                              
         BE    CALT0100            YES - EXIT CC NOT ZERO                       
         B     CALT0120            NO  - EXIT CC ZERO                           
CALT0040 EQU   *                                                                
         LA    R2,COMDLEN(R2)      BUMP TO NEXT SLOT                            
         B     CALT0020            GO BACK FOR NEXT                             
CALT0100 EQU   *                                                                
         LTR   RC,RC               SET CC NOT ZERO                              
         B     CALT0140                                                         
CALT0120 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
CALT0140 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.  ADJUST CON # OF    *              
*     HNNY RECORDS BY 100,000.  REPLACE SALESPERSON CODES FROM   *              
*     TABLE.  REPLACE GROUP/SUBGROUP AS APPROPRIATE.  ADJUST     *              
*     CONT #S IN COMBO CONTROL ELEMENT ALSO.                     *              
******************************************************************              
*                                                                               
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RCONREC,R3                                                       
         MVC   REPCODE,=C'DI'      SET CODE FOR DI                              
CPRO0020 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           SET UP CONTRACT KEY                          
         MVC   KEY+2(2),REPCODE    INSERT REP CODE                              
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     CPRO0060                                                         
CPRO0040 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
CPRO0060 EQU   *                                                                
         CLC   KEY(4),KEYSAVE      SAME COMPANY/RECTYPE?                        
         BNE   CPRO0600            NO  - FINISHED                               
         L     RF,NUMCONS          BUMP CONTRACT COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,NUMCONS                                                       
         CLI   QUESTOR+2,C'#'      USE TEST COUNTS?                             
         BNE   CPRO0070            NO                                           
         CLC   NUMCONS,=F'10000'   **TEST CUTOFF**                              
         BH    CPRO0600            NUMBER PROCESSED:  SKIP OUT                  
CPRO0070 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD            YES - RETRIEVE RECORD                        
         CLC   REPCODE,=C'HN'      HN BEING PROCESSED?                          
         BNE   CPRO0080            NO  - DON'T CHANGE NUMBERS, S/P              
         GOTO1 NUMDELTA,DMCB,RCONKCON    CHANGE CONTRACT NUMBER                 
         BAS   RE,CMBDELTA         CHANGE COMBO CONTROL ELEMENT                 
         BAS   RE,SPDELTA          CHANGE SALESPERSON                           
CPRO0080 EQU   *                                                                
         BAS   RE,GRPDELTA         CHANGE GROUP IF NEEDED                       
         ZICM  RF,RCONLEN,2        INCREMENT TOTAL BYTE COUNTER                 
         A     RF,CONBYTES                                                      
         ST    RF,CONBYTES         STORE IT BACK                                
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUT                        
         MVC   RCONKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,CONCTR           BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR           SAVE IT BACK                                 
         BAS   RE,PUTRECS                                                       
         CLI   QUESTOR+11,C'C'     DISPLAY CONTRACT RECORD?                     
         BE    CPRO0100            YES                                          
         CLI   QUESTOR+11,C'+'     NO  - CONTRACT + BUY?                        
         BNE   CPRO0040            NO                                           
CPRO0100 EQU   *                                                                
         CLC   NUMCONS,=F'50'      DISPLAY FIRST 50 CONTRACTS ONLY              
         BH    CPRO0040            GO BACK FOR NEXT CONTRACT                    
         MVC   P+1(19),=C'   CONTRACT RECORD:'                                  
         MVC   P+1(2),REPCODE                                                   
         EDIT  NUMCONS,(3,P+30)                                                 
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT                                                           
         B     CPRO0040            GO BACK FOR NEXT CONTRACT                    
CPRO0600 EQU   *                                                                
         CLC   REPCODE,=C'HN'      HN PROCESSED?                                
         BE    CPRO0620            YES - FINISHED                               
         MVC   REPCODE,=C'HN'      NO  - PROCESS IT                             
         XC    NUMCONS,NUMCONS     CLEAR THE COUNTER                            
         B     CPRO0020            GO BACK AND DO IT                            
CPRO0620 EQU   *                                                                
         L     RF,CONBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         D     RE,CONCTR           NUM BYTES / # CONTRACTS                      
         MVC   P+1(24),=C'AVERAGE CONTRACT RECORD:'                             
         EDIT  (RF),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  NUMDELTA:  CHANGE THE CONTRACT NUMBER.                        *              
******************************************************************              
*                                                                               
NUMDELTA NTR1                                                                   
         L     R2,0(R1)            A(NUMBER BEING CHANGED)                      
         GOTO1 HEXOUT,DMCB,(R2),CNUMAREA,4,=C'TOG'                              
*                                  CONVERT CON# TO EBCDIC                       
         ZAP   DUB,=P'0'           CLEAR WORKAREA                               
         PACK  DUB,CNUMAREA        PACK NUMBER FOR ADDITION                     
         CVB   R0,DUB              CONVERT IT TO BINARY                         
         A     R0,=F'100000'       ADD 100,000 TO CONTRACT NUMBER               
         EDIT  (R0),(8,CNUMAREA),FILL=0                                         
*                                  PUT THE NUMBER BACK OUT                      
         GOTO1 =V(HEXIN),DMCB,CNUMAREA,(R2),8,=C'TOG'                           
*                                  CONVERT CON# FROM EBCDIC                     
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CMBDELTA:  CHANGE THE CONTRACT NUMBERS IN COMBO CONTROL ELT   *              
******************************************************************              
*                                                                               
CMBDELTA NTR1                                                                   
         LA    R2,RCONELEM         A(DESCRIP ELT)                               
CMBD0020 EQU   *                                                                
         CLI   0(R2),X'0'          END OF RECORD?                               
         BE    CMBD0200            YES - FINISHED                               
         CLI   0(R2),X'17'         COMBO CONTROL ELEMENT?                       
         BE    CMBD0040            YES - PROCESS IT                             
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     CMBD0020            GO BACK FOR NEXT                             
CMBD0040 EQU   *                                                                
         ZIC   RF,1(R2)            CALC # STATIONS IN ELT                       
         SR    RE,RE                                                            
         BCTR  RF,0                SUBTRACT ELEMENT ID +                        
         BCTR  RF,0                   ELEMENT LENGTH                            
         LA    R1,9                LENGTH OF DATA W/IN ITEM                     
         DR    RE,R1               DIVIDE LENGTH BY 9                           
         LR    R5,RF               NUMBER OF ELEMENTS                           
         LA    R4,7(R2)            A(1ST CON# IN ELEMENT)                       
CMBD0060 EQU   *                                                                
         GOTO1 NUMDELTA,DMCB,(R4)                                               
*                                  INCREMENT THE CONTRACT NUMBER                
         LA    R4,9(R4)            BUMP TO NEXT CONTRACT #                      
         BCT   R5,CMBD0060         GO BACK FOR NEXT                             
*                                  DROP-THRU:  FINISHED                         
CMBD0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SPDELTA :  CHANGE THE SALESPERSON CODE IN CONTRACT            *              
******************************************************************              
*                                                                               
SPDELTA  NTR1                                                                   
         L     RF,ASALAREA         A(SALESPERSON AREA)                          
SPDE0020 EQU   *                                                                
         OC    0(3,RF),0(RF)       ANY ENTRY?                                   
         BZ    SPDE0060            NO  - FINISHED                               
         CLC   RCONSAL,0(RF)       SALESPERSON = TABLE?                         
         BNE   SPDE0040            NO  - CHECK NEXT ENTRY                       
         MVC   RCONSAL,3(RF)       YES - INSERT CHANGE TO S/P                   
         B     SPDE0060            FINISHED                                     
SPDE0040 EQU   *                                                                
         LA    RF,6(RF)            BUMP TO NEXT ENTRY                           
         B     SPDE0020            GO BACK FOR NEXT                             
SPDE0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  GRPDELTA:  CHANGE THE GROUP/SUBGROUP IN CONTRACT, IF NEEDED   *              
******************************************************************              
*                                                                               
GRPDELTA NTR1                                                                   
         L     RF,AGRPAREA         A(GROUP/SUBGROUP AREA)                       
GRPD0020 EQU   *                                                                
         OC    0(5,RF),0(RF)       ANY ENTRY?                                   
         BZ    GRPD0060            NO  - FINISHED                               
         CLC   RCONKSTA,0(RF)      CONTRACT STATION = TABLE?                    
         BNE   GRPD0040            NO  - CHECK NEXT ENTRY                       
         MVC   RCONKGRP,5(RF)      YES - INSERT CHANGE TO GROUP/SUBGRP          
         MVC   P+1(13),=C'GRP/SUBGROUP:'                                        
         MVC   P+16(27),RCONKEY                                                 
         GOTO1 REPORT                                                           
         B     GRPD0060                                                         
GRPD0040 EQU   *                                                                
         LA    RF,7(RF)            BUMP TO NEXT SLOT                            
         B     GRPD0020            GO BACK FOR NEXT                             
GRPD0060 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     SWXIT                                                            
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     SWXIT                                                            
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     SWXIT                                                            
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
MASTREP  DS    A                                                                
SUBREP   DS    A                                                                
ABLDAREA DS    A                                                                
AGRPAREA DS    A                                                                
ANEXTGRP DS    A                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
ACOMAREA DS    A                                                                
ANEXTCOM DS    A                                                                
ASTNAREA DS    A                                                                
LBLDAREA DS    F                                                                
NEXTAREA DS    A                   NEXT OPEN SLOT                               
STRTSRCH DS    A                   A(START OF SEARCH)                           
NUMBLD   DS    F                                                                
NUMCONS  DS    F                                                                
NUMBUYS  DS    F                                                                
CONBYTES DS    F                                                                
BUYBYTES DS    F                                                                
CONCTR   DS    F                                                                
BUYCTR   DS    F                                                                
OTHERCTR DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL1'A'              NEW SALESPERSON CODE                         
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
NEWREP   DC    CL2'D4'             NEW REP CODE                                 
FLAGBYTS DS    0CL12               FLAGS                                        
BOTHOPEN DS    CL1                 NEITHER STATION LEFT                         
HNOPEN   DS    CL1                                                              
DIOPEN   DS    CL1                                                              
BOTHLEFT DS    CL1                                                              
         DS    CL8                 SPARE                                        
REPCODE  DS    CL2                                                              
KEYTYPE  DS    CL1                                                              
DBLSPACE DS    CL1                                                              
DATEWORK DS    CL24                DATE WORK AREA                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
         SPACE 2                                                                
         DS    0H                  HALFWORD ALIGNMENT NEEDED.                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   INITIALIZATIONS ....                                                        
*                                                                               
         CSECT                                                                  
INITIAL  NMOD1 0,*INIT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1800000,4000000                                 
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVI   RCSUBPRG,2          SET HEADING FLAG                             
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   AGRPAREA,P2         A(GROUP DIFF TABLE)                          
         MVC   ANEXTGRP,P2         A(NEXT GROUP DIFF SLOT)                      
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         LA    RF,200(RF)                                                       
         ST    RF,ASALAREA         ESTABLISH SALESPERSON AREA                   
         ST    RF,ANEXTSAL         A(NEXT S/P SLOT)                             
         A     RF,=F'1200'         LEAVE ROOM FOR 200 ENTRIES                   
*                                     6 CHARS * 200 SLOTS                       
         ST    RF,ACOMAREA         A(COMMISSION/BUDGET TABLE)                   
         ST    RF,ANEXTCOM         A(NEXT COMM/BUDGET SLOT)                     
         A     RF,=F'500'          LEAVE ROOM FOR 70 ENTRIES                    
*                                     7 CHARS * 70 SLOTS + SPARE                
         ST    RF,ASTNAREA         A(STATION TABLE)                             
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  BUYPROC:   RETRIEVE ALL BUY RECORDS.  ADJUST CON # OF HNNY    *              
*     RECORDS BY 100,000.                                        *              
******************************************************************              
*                                                                               
BUYPROC  NMOD1 0,*BUYP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RBUYREC,R3                                                       
         MVC   REPCODE,=C'DI'      SET CODE FOR DI                              
BPRO0020 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           SET UP BUY RECD KEY                          
         MVC   KEY+16(2),REPCODE   INSERT REP CODE                              
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 HIGHDIR2            RETRIEVE FIRST KEY                           
         B     BPRO0060                                                         
BPRO0040 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 SEQDIR2             RETRIEVE NEXT KEY                            
BPRO0060 EQU   *                                                                
         CLC   KEY(18),KEYSAVE     SAME COMPANY/RECTYPE?                        
         BNE   BPRO0600            NO  - FINISHED                               
         L     RF,NUMBUYS          BUMP BUY RECD COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,NUMBUYS                                                       
         CLI   QUESTOR+2,C'#'      USE TEST COUNT?                              
         BNE   BPRO0070            NO                                           
         CLC   NUMBUYS,=F'50000'   **TEST CUTOFF**                              
         BH    BPRO0600            NUMBER PROCESSED:  SKIP OUT                  
BPRO0070 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECR2            YES - RETRIEVE RECORD                        
         CLC   REPCODE,=C'HN'      HN BEING PROCESSED?                          
         BNE   BPRO0080            NO  - DON'T CHANGE NUMBERS                   
         GOTO1 BUYDELTA,DMCB,RBUYKCON    CHANGE CONT# IN BUY RECD               
BPRO0080 EQU   *                                                                
         ZICM  RF,RBUYLEN,2        INCREMENT TOTAL BYTE COUNTER                 
         A     RF,BUYBYTES                                                      
         ST    RF,BUYBYTES         STORE IT BACK                                
         MVC   REC-4(2),RBUYLEN    INSERT LENGTH FOR PUT                        
         MVC   RBUYKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,BUYCTR           BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR           SAVE IT BACK                                 
         BAS   RE,PUTREC2                                                       
         CLI   QUESTOR+11,C'B'     DISPLAY BUY RECORD?                          
         BE    BPRO0100            YES                                          
         CLI   QUESTOR+11,C'+'     NO  - CONTRACT + BUY?                        
         BNE   BPRO0040            NO                                           
BPRO0100 EQU   *                                                                
         CLC   NUMBUYS,=F'50'      DISPLAY FIRST 50 BUY RECDS ONLY              
         BH    BPRO0040            GO BACK FOR NEXT BUY RECORD                  
         MVC   P+1(16),=C'     BUY RECORD:'                                     
         MVC   P+1(2),REPCODE                                                   
         EDIT  NUMBUYS,(3,P+20)                                                 
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT2,DMCB,(RC)                                               
         GOTO1 REPORT                                                           
         B     BPRO0040            GO BACK FOR NEXT BONTRACT                    
BPRO0600 EQU   *                                                                
         CLC   REPCODE,=C'HN'      HN PROCESSED?                                
         BE    BPRO0620            YES - FINISHED                               
         MVC   REPCODE,=C'HN'      NO  - PROCESS IT                             
         XC    NUMBUYS,NUMBUYS     CLEAR THE COUNTER                            
         B     BPRO0020            GO BACK AND DO IT                            
BPRO0620 EQU   *                                                                
         L     RF,BUYBYTES         CALCULATE AVERAGE RECORD SIZE                
         SR    RE,RE                                                            
         D     RE,BUYCTR           NUM BYTES / 100,000 RECS                     
         MVC   P+1(24),=C'AVERAGE BUY      RECORD:'                             
         EDIT  (RF),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  BUYDELTA:  CHANGE THE CONTRACT NUMBER IN BUY RECORD.          *              
******************************************************************              
*                                                                               
BUYDELTA NTR1                                                                   
         L     R2,0(R1)            A(NUMBER BEING CHANGED)                      
*                                                                               
         CLC   NUMBUYS,=F'50'      DISPLAY FIRST 50 BUY RECDS ONLY              
         BH    BDEL0020            ALL DISPLAYED                                
         MVC   P+1(16),=C'     BUY OLD # :'                                     
         MVC   P+1(2),REPCODE                                                   
         EDIT  NUMBUYS,(3,P+20)                                                 
         GOTO1 HEXOUT,DMCB,(R2),P+30,4,=C'TOG'                                  
*                                  DISPLAY ORIGINAL CONTRACT NUMBER             
         GOTO1 REPORT                                                           
*                                  REVERSE CON#: (9'S COMP/REV)                 
BDEL0020 EQU   *                                                                
         PACK  RNUMAREA+0(1),3(1,R2)                                            
         PACK  RNUMAREA+1(1),2(1,R2)                                            
         PACK  RNUMAREA+2(1),1(1,R2)                                            
         PACK  RNUMAREA+3(1),0(1,R2)                                            
         GOTO1 HEXOUT,DMCB,RNUMAREA,CNUMAREA,4,=C'TOG'                          
*                                  CONVERT CON# (9'S COMP) TO EBCDIC            
         ZAP   DUB,=P'0'           CLEAR WORKAREA                               
         PACK  DUB,CNUMAREA        PACK NUMBER FOR ADDITION                     
         CVB   R0,DUB              CONVERT IT TO BINARY                         
         S     R0,=F'100000'       SUBTRACT 100,000 FROM CON#                   
         EDIT  (R0),(8,CNUMAREA),FILL=0                                         
*                                  PUT THE NUMBER BACK OUT                      
         GOTO1 =V(HEXIN),DMCB,CNUMAREA,RNUMAREA,8,=C'TOG'                       
*                                  CONVERT CON# FROM EBCDIC                     
*                                     THEN REVERSE IT                           
         PACK  3(1,R2),RNUMAREA+0(1)                                            
         PACK  2(1,R2),RNUMAREA+1(1)                                            
         PACK  1(1,R2),RNUMAREA+2(1)                                            
         PACK  0(1,R2),RNUMAREA+3(1)                                            
*                                                                               
         CLC   NUMBUYS,=F'50'      DISPLAY FIRST 50 BUY RECDS ONLY              
         BH    BDEL0040            ALL DISPLAYED                                
         MVC   P+1(16),=C'     BUY NEW # :'                                     
         MVC   P+1(2),REPCODE                                                   
         EDIT  NUMBUYS,(3,P+20)                                                 
         GOTO1 HEXOUT,DMCB,(R2),P+30,4,=C'TOG'                                  
*                                  DISPLAY ORIGINAL CONTRACT NUMBER             
         GOTO1 REPORT                                                           
BDEL0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPPUT2: DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT2 NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTREC2:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTREC2  NTR1                                                                   
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         XIT1                                                                   
         EJECT                                                                  
HIGHDIR2 NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK2                                                         
         SPACE 2                                                                
SEQDIR2  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK2                                                         
         SPACE 3                                                                
GETRECR2 LA    R6,GETREC                                                        
         B     LINKFIL2                                                         
         SPACE 2                                                                
LINKFIL2 NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK2                                                         
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK2 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT2                                                           
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT2                                                           
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRAC2                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT2   CR    RB,RB                                                            
         B     SWXIT2                                                           
         SPACE 1                                                                
NEXIT2   LTR   RB,RB                                                            
SWXIT2   XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRAC2  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDMX,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRAC22                                                         
         LA    R4,TRACEKY2                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRAC24                                                         
         SPACE 1                                                                
DMTRAC22 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRAC24 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDMX,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     SWXIT2                                                           
         SPACE 2                                                                
TRACEDMX DS    C                                                                
TRACEKY2 DS    CL32                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:  SCAN DI AND HN STATION RECORDS.  CONSTRUCT TABLE   *              
*     BASED ON DSECT STADSECT.  THEN RESCAN TABLE, SELECTING     *              
*     APPROPRIATE RECORDS FOR OUTPUT.                            *              
******************************************************************              
*                                                                               
DISPTOTS NMOD1 0,*TOTS*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'CONTRACTS     PROCESSED:'                             
         EDIT  CONCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUYS          PROCESSED:'                             
         EDIT  BUYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MISCELLANEOUS PROCESSED:'                             
         EDIT  OTHERCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   WORKAREA DSECT TO PROCESS STATION RECORDS                                   
*                                                                               
STADSECT DSECT                                                                  
STADCALL DS    CL5                 STATION CALL LETTERS                         
STADDIJN DS    CL3                 DI DATE JOINED                               
STADDILV DS    CL3                 DI DATE LEFT                                 
STADHNJN DS    CL3                 HN DATE JOINED                               
STADHNLV DS    CL3                 HN DATE LEFT                                 
STADDIGP DS    CL2                 DI GROUP/SUBGROUP                            
STADHNGP DS    CL2                 HN GROUP/SUBGROUP                            
STADDICL DS    CL2                 DI CLOSED THROUGH DATE                       
STADHNCL DS    CL2                 HN CLOSED THROUGH DATE                       
STADDIDA DS    CL4                 DI DISK ADDRESS OF RECORD                    
STADHNDA DS    CL4                 HN DISK ADDRESS OF RECORD                    
*                                                                               
STADLEN  EQU   *-STADCALL          LENGTH OF ENTRY                              
*                                                                               
         EJECT                                                                  
*                                                                               
*   WORKAREA DSECT TO PROCESS GROUP CHANGES                                     
*                                                                               
GRPDSECT DSECT                                                                  
GRPDCALL DS    CL5                 STATION CALL LETTERS                         
GRPDGRUP DS    CL2                 GROUP/SUBGROUP                               
*                                                                               
GRPDLEN  EQU   *-GRPDCALL          LENGTH OF ENTRY                              
*                                                                               
         EJECT                                                                  
*                                                                               
*   WORKAREA DSECT TO PROCESS COMMISSION AND BUDGET RECORDS                     
*                                                                               
COMDSECT DSECT                                                                  
COMDCALL DS    CL5                 STATION CALL LETTERS                         
COMDREPB DS    CL2                 REP CODE: BUDGET                             
COMDREPC DS    CL2                 REP CODE: COMMISSION                         
*                                                                               
COMDLEN  EQU   *-COMDCALL          LENGTH OF ENTRY                              
*                                                                               
         EJECT                                                                  
*  INCLUDE REGENCOM                COMMISSION RECORD                            
*  INCLUDE REGENREG                REGION RECORD                                
*  INCLUDE REGENOFF                OFFICE RECORD                                
*  INCLUDE REGENEOM                EOM RECORD                                   
*  INCLUDE REGENDPT                DAYPART RECORD                               
*  INCLUDE REGENBUD                BUDGET RECORD                                
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION RECORD                               
*  INCLUDE REGENSDD                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCOM          COMMISSION RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREG          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOFF          OFFICE     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOM          END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDPTA         END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUD          BUDGET RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'228REREPSW04 05/01/02'                                      
         END                                                                    
