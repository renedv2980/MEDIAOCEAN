*          DATA SET REREPSW03  AT LEVEL 036 AS OF 05/01/02                      
*PHASE RESW02B,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE QSORT                                                                  
         TITLE 'REREPSW03  (RESW03) --- COMPANY MERGER: MMR/TO/CM'              
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPSW03  -- REP FILE COMPANY MERGER.                    *            
*                       CREATE A SINGLE FILE FROM I1/TO/CM DATA,   *            
*                       AS NEW COMPANY 'XX'                        *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JUN05/96 (BU ) --- ORIGINAL ENTRY, BASED ON PRIOR MERGER         *            
*                    REREPSW02B (DI/HN)                            *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   =  Y  =  STATION DISPLAY                         *            
*                 =  X  =  STATION MERGER DISPLAY                  *            
*     QUESTOR+1   =  GROUP+BUD/COMM TABLE                          *            
*     QUESTOR+2   =  Y  =  TABLE MATCHUP                           *            
*     QUESTOR+3   =  REGION RECORD                                 *            
*     QUESTOR+4   =  OFFICE RECORD                                 *            
*     QUESTOR+5   =  Y  =  SALESPERSON SETUP                       *            
*                    B  =  SALESPERSON SETUP + SALESPERSON RECORD  *            
*     QUESTOR+6   =  BUDGET RECORD                                 *            
*     QUESTOR+7   =  END OF MONTH RECORD                           *            
*                    #  =  USE CONTRACT+BUY TEST COUNT             *            
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
         NMOD1 0,**RESW**,R9,R8,RR=R5                                           
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
         GOTO1 OBUDPROC,DMCB,(RC)  PROCESS OFFICE BUDGET RECORDS                
*                                                                               
         GOTO1 PGTRPROC,DMCB,(RC)  PROCESS PROGRAM TYPE RECORDS                 
*                                                                               
         GOTO1 SCOMPROC,DMCB,(RC)  PROCESS STANDARD COMMENT RECORDS             
*                                                                               
         GOTO1 SCTYPROC,DMCB,(RC)  PROCESS CONTRACT TYPE RECORDS                
*                                                                               
         GOTO1 RADRPROC,DMCB,(RC)  PROCESS RADAR RECORDS                        
*                                                                               
         GOTO1 OCMTPROC,DMCB,(RC)  PROCESS OFFICE COMMENT RECORDS               
*                                                                               
         GOTO1 LABLPROC,DMCB,(RC)  PROCESS LABEL RECORDS                        
*                                                                               
         GOTO1 TYPEPROC,DMCB,(RC)  PROCESS TYPE  RECORDS                        
*                                                                               
         GOTO1 SETRPROC,DMCB,(RC)  PROCESS SET   RECORDS                        
*                                                                               
         GOTO1 TERRPROC,DMCB,(RC)  PROCESS TERRITORY RECORDS                    
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
*  STATPROC:  SCAN ALL REPS  STATION RECORDS.  CONSTRUCT TABLE   *              
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
         XC    KEY,KEY             SET UP STATION KEY FOR REP1                  
         MVI   KEY,2                                                            
*                                                                               
*   RETRIEVE ALL STATION RECORDS FOR REP1, AND TABLE.  KEEP THE                 
*      ADDRESS OF THE NEXT AVAILABLE SLOT IN THE TABLE.                         
*                                                                               
         MVC   KEY+20(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     STAT0040                                                         
STAT0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
STAT0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     STATION RECORD/SAME COMPANY?                 
         BNE   STAT0100            NO  - GO PROCESS HN                          
         GOTO1 GETRECRD            YES - RETRIEVE THE RECORD                    
         MVC   STADCALL,RSTAKSTA   INSERT STATION CALLS                         
         MVC   STADS1JN,RSTASTRT   INSERT JOIN DATE                             
         MVC   STADS1LV,RSTAEND    INSERT LEAVE DATE                            
         MVC   STADS1GP,RSTAGRUP   INSERT GROUP/SUBGROUP                        
         MVC   STADS1CL,RSTACLDT   INSERT CLOSE DATE                            
         MVC   STADS1DA,KEY+28     INSERT LINK ADDRESS                          
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         ST    R7,NEXTAREA         SAVE A(NEXT OPEN SLOT)                       
         B     STAT0020            GO BACK FOR NEXT                             
STAT0100 EQU   *                                                                
*                                                                               
*   RETRIEVE ALL STATION RECORDS FOR REP2.  SCAN TABLE TO SEE IF                
*      STATION IS ALREADY THERE.  IF IT IS, ADD REP2 INFO INTO                  
*      SLOT WITH REP1 INFO.  IF NOT THERE, INSERT THE STATION                   
*      INTO NEXT AVAILABLE SLOT.  TO ENSURE THAT ENTIRE TABLE                   
*      IS NOT SEARCHED EACH TIME, KEEP POINTER TO START ENTRY                   
*      FOR SEARCH.                                                              
*                                                                               
         XC    KEY,KEY             SET UP STATION KEY FOR HN                    
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REP2                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     STAT0140                                                         
STAT0120 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
STAT0140 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     STATION RECORD/SAME COMPANY?                 
         BNE   STAT0150            NO  - GO GENERATE RECORDS                    
         GOTO1 GETRECRD            YES - RETRIEVE THE RECORD                    
         BAS   RE,FINDSTAT         LOOK FOR STATION IN TABLE                    
         BZ    STAT0120            FOUND - DATA INSERTED                        
*                                     GO BACK FOR NEXT                          
*                                  NOT FOUND - INSERT INFO IN NEXT SLOT         
         L     R7,NEXTAREA         SET A(NEXT OPEN SLOT)                        
         MVC   STADCALL,RSTAKSTA   INSERT STATION CALLS                         
         MVC   STADS2JN,RSTASTRT   INSERT JOIN DATE                             
         MVC   STADS2LV,RSTAEND    INSERT LEAVE DATE                            
         MVC   STADS2GP,RSTAGRUP   INSERT GROUP/SUBGROUP                        
         MVC   STADS2CL,RSTACLDT   INSERT CLOSE DATE                            
         MVC   STADS2DA,KEY+28     INSERT LINK ADDRESS                          
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         ST    R7,NEXTAREA         SAVE A(NEXT OPEN SLOT)                       
         B     STAT0120            GO BACK FOR NEXT                             
STAT0150 EQU   *                                                                
*                                                                               
*   RETRIEVE ALL STATION RECORDS FOR REP3.  SCAN TABLE TO SEE IF                
*      STATION IS ALREADY THERE.  IF IT IS, ADD REP3 INFO INTO                  
*      SLOT WITH REP1/REP2 INFO.  IF NOT THERE, INSERT THE STATION              
*      INTO NEXT AVAILABLE SLOT.  TO ENSURE THAT ENTIRE TABLE                   
*      IS NOT SEARCHED EACH TIME, KEEP POINTER TO START ENTRY                   
*      FOR SEARCH.                                                              
*                                                                               
         XC    KEY,KEY             SET UP STATION KEY FOR HN                    
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REP3                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     STAT0170                                                         
STAT0160 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
STAT0170 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     STATION RECORD/SAME COMPANY?                 
         BNE   STAT0200            NO  - GO GENERATE RECORDS                    
         GOTO1 GETRECRD            YES - RETRIEVE THE RECORD                    
         BAS   RE,FINDSTA2         LOOK FOR STATION IN TABLE                    
         BZ    STAT0160            FOUND - DATA INSERTED                        
*                                     GO BACK FOR NEXT                          
*                                  NOT FOUND - INSERT INFO IN NEXT SLOT         
         L     R7,NEXTAREA         SET A(NEXT OPEN SLOT)                        
         MVC   STADCALL,RSTAKSTA   INSERT STATION CALLS                         
         MVC   STADS3JN,RSTASTRT   INSERT JOIN DATE                             
         MVC   STADS3LV,RSTAEND    INSERT LEAVE DATE                            
         MVC   STADS3GP,RSTAGRUP   INSERT GROUP/SUBGROUP                        
         MVC   STADS3CL,RSTACLDT   INSERT CLOSE DATE                            
         MVC   STADS3DA,KEY+28     INSERT LINK ADDRESS                          
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         ST    R7,NEXTAREA         SAVE A(NEXT OPEN SLOT)                       
         B     STAT0160            GO BACK FOR NEXT                             
STAT0200 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
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
         OC    STADS1DA,STADS1DA   REP1 STATION?                                
         BZ    STAT0260            NO                                           
         OC    STADS2DA,STADS2DA   YES - REP2 STATION ALSO?                     
         BNZ   STAT0300            YES - COMPARE DATES                          
         OC    STADS3DA,STADS3DA   NO  - REP3 STATION ALSO?                     
         BNZ   STAT0300            YES - COMPARE DATES                          
*                                  NO  - GEN REC FROM REP1                      
         MVC   KEY+28(4),STADS1DA  INSERT DISK ADDRESS                          
         GOTO1 GETRECRD            RETRIEVE STATION RECORD                      
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         CLI   QUESTOR+0,C'Y'      DISPLAY STATION DATA?                        
         BNE   STAT0250            NOT SPECIAL REQUEST                          
         MVC   P+1(15),=C'STATION RECORD:'                                      
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
STAT0250 EQU   *                                                                
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         GOTO1 DISPSTAT,DMCB,(RC),(R7),1                                        
*                                  DISPLAY STATION INFORMATION                  
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         B     STAT0220            GO BACK FOR NEXT                             
STAT0260 EQU   *                                                                
         OC    STADS2DA,STADS2DA   REP2 STATION?                                
         BZ    STAT0280            NO  - ONLY REP3 LEFT                         
         OC    STADS3DA,STADS3DA   YES - REP3 STATION ALSO?                     
         BNZ   STAT0300            YES - COMPARE AND GEN                        
*                                  NO  - GEN REC FROM REP2                      
         MVC   KEY+28(4),STADS2DA  INSERT DISK ADDRESS                          
         GOTO1 GETRECRD            RETRIEVE STATION RECORD                      
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         CLI   QUESTOR+0,C'Y'      DISPLAY STATION DATA?                        
         BNE   STAT0270            NOT SPECIAL REQUEST                          
         MVC   P+1(15),=C'STATION RECORD:'                                      
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
STAT0270 EQU   *                                                                
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         GOTO1 DISPSTAT,DMCB,(RC),(R7),2                                        
*                                  DISPLAY STATION INFORMATION                  
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         B     STAT0220            GO BACK FOR NEXT                             
STAT0280 EQU   *                                                                
*                                  GEN REC FROM REP3                            
         MVC   KEY+28(4),STADS3DA  INSERT DISK ADDRESS                          
         GOTO1 GETRECRD            RETRIEVE STATION RECORD                      
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         CLI   QUESTOR+0,C'Y'      DISPLAY STATION DATA?                        
         BNE   STAT0290            NOT SPECIAL REQUEST                          
         MVC   P+1(15),=C'STATION RECORD:'                                      
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
STAT0290 EQU   *                                                                
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         GOTO1 DISPSTAT,DMCB,(RC),(R7),3                                        
*                                  DISPLAY STATION INFORMATION                  
         LA    R7,STADLEN(R7)      BUMP TO NEXT TABLE ENTRY                     
         B     STAT0220            GO BACK FOR NEXT                             
STAT0300 EQU   *                                                                
         BAS   RE,STATDATE         ESTABLISH DATES FOR STATION                  
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
*  FINDSTAT:  LOOK IN TABLE TO SEE IF REP2 STATION ALSO EXISTS   *              
*     FOR REP1.  IF SO, ADD INFO TO REP1 ENTRY.  USE 'STRTSRCH'  *              
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
         MVC   STADS2JN,RSTASTRT   INSERT JOIN DATE                             
         MVC   STADS2LV,RSTAEND    INSERT LEAVE DATE                            
         MVC   STADS2GP,RSTAGRUP   INSERT GROUP/SUBGROUP                        
         MVC   STADS2CL,RSTACLDT   INSERT CLOSE DATE                            
         MVC   STADS2DA,KEY+28     INSERT LINK ADDRESS                          
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
*  FINDSTA2:   LOOK IN TABLE TO SEE IF REP3 STATION ALSO EXISTS  *              
*     FOR REP1/2. IF SO, ADD INFO TO BASE ENTRY.                 *              
*     CAN'T LIMIT STATION SEARCH FOR THIRD REP                   *              
******************************************************************              
*                                                                               
FINDSTA2 NTR1                                                                   
         L     R7,ASTNAREA         SET A(START OF TABLE SEARCH)                 
*                                     'USING' STILL IN EFFECT                   
FIN20020 EQU   *                                                                
         CLC   STADCALL,RSTAKSTA   TABLE VS STATION RECORD                      
         BE    FIN20060            FOUND - INSERT HN DATA INTO SLOT             
         LA    R7,STADLEN(R7)                                                   
         L     RF,NEXTAREA         CHECK FOR END OF TABLE                       
         CR    R7,RF               END REACHED?                                 
         BNE   FIN20020            NO                                           
FIN20040 EQU   *                                                                
         LTR   RC,RC               SET CC NOT = ZERO                            
         B     FIN20080                                                         
FIN20060 EQU   *                                                                
         MVC   STADS3JN,RSTASTRT   INSERT JOIN DATE                             
         MVC   STADS3LV,RSTAEND    INSERT LEAVE DATE                            
         MVC   STADS3GP,RSTAGRUP   INSERT GROUP/SUBGROUP                        
         MVC   STADS3CL,RSTACLDT   INSERT CLOSE DATE                            
         MVC   STADS3DA,KEY+28     INSERT LINK ADDRESS                          
         CLI   QUESTOR+2,C'Y'      TABLE MATCH DISPLAY?                         
         BNE   FIN20062            NO                                           
         MVC   P+1(12),=C'TABLE MATCH:'                                         
         MVC   P+15(5),RSTAKSTA                                                 
         MVC   P+22(5),STADCALL                                                 
         ST    R7,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,P+30,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
FIN20062 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
FIN20080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  STATDATE:  ESTABLISH DATES FOR STATION                        *              
*                                                                *              
******************************************************************              
*                                                                               
STATDATE NTR1                                                                   
         LA    R3,REC              IO+PUT AREA                                  
         USING RSTAREC,R3          SET RECORD DEFINITION                        
*                                     OVER IO AREA                              
         XC    DATETABL,DATETABL                                                
*                                  CLEAR TABLE                                  
         XC    DTABLCTR,DTABLCTR   CLEAR COUNTER                                
         LA    RF,DATETABL         SET A(NEXT ENTRY IN TABLE)                   
*                                                                               
         OC    STADS1DA,STADS1DA   ANY ENTRY FOR REP1?                          
         BZ    STDA0040            NO                                           
         ZICM  RE,STADS1LV,3       YES - GET/INVERT LEAVE DATE                  
         LCR   RE,RE               COMPLEMENT THE REGISTER                      
         STCM  RE,7,DLVDATE(RF)    INSERT INTO TABLE                            
         MVC   DL2DATE(3,RF),STADS1LV                                           
*                                  INSERT LEAVE DATE INTO 2ND SLOT              
         MVC   DJNDATE(3,RF),STADS1JN                                           
*                                  INSERT JOIN DATE INTO TABLE                  
         MVC   DRPTABL(2,RF),=C'01'                                             
         LA    RF,LDTBLSLT(RF)     BUMP TO NEXT TABLE ENTRY                     
         L     RE,DTABLCTR         LOAD COUNTER                                 
         LA    RE,1(RE)            INCREMENT COUNTER                            
         ST    RE,DTABLCTR         REPLACE COUNTER                              
STDA0040 EQU   *                                                                
         OC    STADS2DA,STADS2DA   ANY ENTRY FOR REP2?                          
         BZ    STDA0080            NO                                           
         ZICM  RE,STADS2LV,3       YES - GET/INVERT LEAVE DATE                  
         LCR   RE,RE               COMPLEMENT THE REGISTER                      
         STCM  RE,7,DLVDATE(RF)    INSERT INTO TABLE                            
         MVC   DL2DATE(3,RF),STADS2LV                                           
*                                  INSERT LEAVE DATE INTO 2ND SLOT              
         MVC   DJNDATE(3,RF),STADS2JN                                           
*                                  INSERT JOIN DATE INTO TABLE                  
         MVC   DRPTABL(2,RF),=C'02'                                             
         LA    RF,LDTBLSLT(RF)     BUMP TO NEXT TABLE ENTRY                     
         L     RE,DTABLCTR         LOAD COUNTER                                 
         LA    RE,1(RE)            INCREMENT COUNTER                            
         ST    RE,DTABLCTR         REPLACE COUNTER                              
STDA0080 EQU   *                                                                
         OC    STADS3DA,STADS3DA   ANY ENTRY FOR REP3?                          
         BZ    STDA0120            NO                                           
         ZICM  RE,STADS3LV,3       YES - GET/INVERT LEAVE DATE                  
         LCR   RE,RE               COMPLEMENT THE REGISTER                      
         STCM  RE,7,DLVDATE(RF)    INSERT INTO TABLE                            
         MVC   DL2DATE(3,RF),STADS3LV                                           
*                                  INSERT LEAVE DATE INTO 2ND SLOT              
         MVC   DJNDATE(3,RF),STADS3JN                                           
*                                  INSERT JOIN DATE INTO TABLE                  
         MVC   DRPTABL(2,RF),=C'03'                                             
         LA    RF,LDTBLSLT(RF)     BUMP TO NEXT TABLE ENTRY                     
         L     RE,DTABLCTR         LOAD COUNTER                                 
         LA    RE,1(RE)            INCREMENT COUNTER                            
         ST    RE,DTABLCTR         REPLACE COUNTER                              
STDA0120 EQU   *                                                                
         LA    R2,DATETABL         SET A(DATE TABLE)                            
         L     R5,DTABLCTR         SET NUMBER OF RECORDS                        
         GOTO1 =V(QSORT),DMCB,(R2),(R5),11,06,0                                 
*                                                                               
*   TEST                                                                        
         CLI   QUESTOR+0,C'Y'      DISPLAY STATION DATA?                        
         BNE   STDA0140            NOT SPECIAL REQUEST                          
         MVC   P+1(19),=C'DATE TABLE DISPLAY:'                                  
         MVC   P+22(5),STADCALL    INSERT CALL LETTERS                          
         GOTO1 REPORT                                                           
         MVC   P+1(LDTBLSLT),LEAVDAT1                                           
         GOTO1 REPORT                                                           
         MVC   P+1(LDTBLSLT),LEAVDAT2                                           
         GOTO1 REPORT                                                           
         MVC   P+1(LDTBLSLT),LEAVDAT3                                           
         GOTO1 REPORT                                                           
STDA0140 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         MVC   GENREP,REPTABL1     SAVE REP# FOR RECORD READ                    
         MVC   KEY+28(4),STADS1DA  INSERT DISK ADDRESS: REP1                    
         MVC   JOINDATE,STADS1JN   INSERT REP1 JOIN DATE                        
         CLI   GENREP+1,C'1'       REP1?                                        
         BE    STDA0160            YES - USE DISK ADDR: REP1                    
         MVC   KEY+28(4),STADS2DA  INSERT DISK ADDRESS: REP2                    
         MVC   JOINDATE,STADS2JN   INSERT REP2 JOIN DATE                        
         CLI   GENREP+1,C'2'       REP2?                                        
         BE    STDA0160            YES - USE DISK ADDR: REP2                    
         MVC   KEY+28(4),STADS3DA  INSERT DISK ADDRESS: REP3                    
         MVC   JOINDATE,STADS3JN   INSERT REP3 JOIN DATE                        
STDA0160 EQU    *                                                               
         GOTO1 GETRECRD            RETRIEVE REPX STATION RECORD                 
         MVC   SAVEGRP,RSTAGRUP    SAVE GROUP/SUBGROUP                          
*                                     OF RECORD USED AS OUTPUT                  
         GOTO1 TABLSTAT,DMCB,(RC),(R3)                                          
*                                  TABLE FOR COMM/BUDGET USE                    
*                                                                               
*                                  SORT TABLE INTO JOIN/LEAVE ORDER             
         LA    R2,DATETABL         SET A(DATE TABLE)                            
         L     R5,DTABLCTR         SET NUMBER OF RECORDS                        
         GOTO1 =V(QSORT),DMCB,(R2),(R5),11,06,3                                 
*                                                                               
*   TEST                                                                        
         CLI   QUESTOR+0,C'Y'      DISPLAY STATION DATA?                        
         BNE   STDA0180            NOT SPECIAL REQUEST                          
         MVC   P+1(19),=C'DATE TABLE DISPLAY:'                                  
         MVC   P+22(5),STADCALL    INSERT CALL LETTERS                          
         GOTO1 REPORT                                                           
         MVC   P+1(LDTBLSLT),LEAVDAT1                                           
         GOTO1 REPORT                                                           
         MVC   P+1(LDTBLSLT),LEAVDAT2                                           
         GOTO1 REPORT                                                           
         MVC   P+1(LDTBLSLT),LEAVDAT3                                           
         GOTO1 REPORT                                                           
STDA0180 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
*                                  DETERMINE CONSECUTIVE LV/JN DATES            
         OC    REPTABL3,REPTABL3   THREE ENTRIES IN TABLE?                      
         BZ    STDA0240            NO  - ONLY TWO                               
*                                  YES - BEGIN TEST WITH REP3 VS REP2           
         CLC   JOINDAT3,LEAVDT2A   RP3 JOIN = REP2 LEAVE?                       
         BNE   STDA0200            NO                                           
         MVC   JOINDATE,JOINDAT2   YES - SAVE REP2 JOIN DATE                    
         B     STDA0240            CONTINUE CHECK                               
STDA0200 EQU   *                                                                
         OC    LEAVDT2A,LEAVDT2A   ANY VALUE IN LEAVE DATE?                     
         BZ    STDA0320            NO  - USE JOINED VALUE                       
         GOTO1 DATCON,DMCB,(3,LEAVDT2A),(0,DATEWORK)                            
*                                  CONVERT REP2 LEAVE DATE TO YYMMDD            
         LA    RF,1                                                             
         GOTO1 ADDAY,DMCB,DATEWORK,DATEWORK+6,(RF)                              
         CLC   DATEWORK+6(3),JOINDAT3                                           
*                                  DAY AFTER REP2 LEAVE:  IS IT                 
*                                     EQUAL TO REP3 JOIN DATE?                  
         BNE   STDA0320            NO  - USE JOINDATE SAVED                     
         MVC   JOINDATE,JOINDAT2   YES - INSERT REP2 JOIN DATE                  
*                                  TEST OF REP2 VS REP1                         
STDA0240 EQU   *                                                                
         CLC   JOINDAT2,LEAVDT1A   REP2 JOIN = REP1 LEAVE?                      
         BNE   STDA0280            NO                                           
         MVC   JOINDATE,JOINDAT1   YES - SAVE REP1 JOIN DATE                    
         B     STDA0320            CONTINUE CHECK                               
STDA0280 EQU   *                                                                
         OC    LEAVDT1A,LEAVDT2A   ANY VALUE IN LEAVE DATE?                     
         BZ    STDA0320            NO  - USE JOINED VALUE                       
         GOTO1 DATCON,DMCB,(3,LEAVDT1A),(0,DATEWORK)                            
*                                  CONVERT REP1 LEAVE DATE TO YYMMDD            
         LA    RF,1                                                             
         GOTO1 ADDAY,DMCB,DATEWORK,DATEWORK+6,(RF)                              
         CLC   DATEWORK+6(3),JOINDAT2                                           
*                                  DAY AFTER REP1 LEAVE:  IS IT                 
*                                     EQUAL TO REP2 JOIN DATE?                  
         BNE   STDA0320            NO  - USE JOINDATE SAVED                     
         MVC   JOINDATE,JOINDAT1   YES - INSERT REP1 JOIN DATE                  
STDA0320 EQU   *                                                                
         MVC   RSTASTRT,JOINDATE   INSERT PROPER JOIN DATE                      
*                                                                               
         MVC   REC-4(2),RSTALEN    INSERT LENGTH FOR PUT                        
         MVC   RSTAKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+0,C'X'      DISPLAY STATION MERGES ONLY?                 
         BE    STDA0340            NOT SPECIAL REQUEST                          
         CLI   QUESTOR+0,C'Y'      DISPLAY STATION DATA?                        
         BNE   STDA0350            NOT SPECIAL REQUEST                          
STDA0340 EQU   *                                                                
         MVC   P+1(15),=C'STATION RECORD:'                                      
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
STDA0350 EQU   *                                                                
         GOTO1 DISPSTAT,DMCB,(RC),(R7),4                                        
*                                  DISPLAY STATION INFORMATION                  
STDA0360 EQU   *                                                                
         XIT1                      EXIT                                         
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPSTAT:  DISPLAY RESULT OF STATION UPDATE.                  *              
*     DMCB+4   =   SOURCE OF DATA                                *              
*          1   =   MMR FILE                                      *              
*          2   =   TO  FILE                                      *              
*          3   =   CMB FILE                                      *              
*          4   =   MERGE                                         *              
*                                                                *              
******************************************************************              
*                                                                               
DISPSTAT NTR1                                                                   
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R7,4(R1)            RESET A(TABLE)                               
         USING STADSECT,R7                                                      
         MVC   P+1(5),STADCALL                                                  
         MVC   P+15(08),=C'SOURCE: '                                            
         MVC   P+23(05),=C'MERGE'  SET TO 'FROM MERGE'                          
         MVI   DBLSPACE,C'Y'       SET DOUBLE SPACE INDICATOR                   
         CLI   DMCB+11,X'1'        FROM REP1?                                   
         BNE   DIAT0020            NO                                           
         MVC   P+23(5),SPACES                                                   
         MVC   P+23(02),REP1       YES                                          
         MVI   DBLSPACE,C'Y'       SET DOUBLE SPACE INDICATOR                   
         B     DIAT0060                                                         
DIAT0020 EQU   *                                                                
         CLI   DMCB+11,X'2'        FROM REP2?                                   
         BNE   DIAT0040            NO  - CONSIDER IT A '3'                      
         MVC   P+23(5),SPACES                                                   
         MVC   P+23(02),REP2       YES                                          
         MVI   DBLSPACE,C'Y'       SET DOUBLE SPACE INDICATOR                   
         B     DIAT0060                                                         
DIAT0040 EQU   *                                                                
         CLI   DMCB+11,X'3'        FROM REP3?                                   
         BNE   DIAT0060            NO  - CONSIDER IT A '4'                      
         MVC   P+23(5),SPACES                                                   
         MVC   P+23(02),REP3       YES                                          
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
         DROP  R3,R7                                                            
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
*        OC    BOTHOPEN,BOTHOPEN   BOTH STATIONS OPEN?                          
*        BZ    XTRA0020            NO                                           
*        MVC   P+1(10),=C'DI JOIN:  '                                           
*        GOTO1 DATCON,DMCB,(3,STADS1JN),(5,P+11)                                
*        MVC   P+20(10),=C'HN JOIN:  '                                          
*        GOTO1 DATCON,DMCB,(3,STADS2JN),(5,P+30)                                
*        GOTO1 REPORT                                                           
*        B     XTRA0150            EXIT                                         
XTRA0020 EQU   *                                                                
*        OC    HNOPEN,HNOPEN       HN CURRENT?                                  
*        BZ    XTRA0040            NO                                           
*                                  YES - HN USED TO OUTPUT RECORD               
*        MVC   P+1(10),=C'DI JOIN:  '                                           
*        GOTO1 DATCON,DMCB,(3,STADS1JN),(5,P+11)                                
*        MVC   P+20(10),=C'DI LEFT:  '                                          
*        GOTO1 DATCON,DMCB,(3,STADS1LV),(5,P+30)                                
*        MVC   P+40(10),=C'HN JOIN:  '                                          
*        GOTO1 DATCON,DMCB,(3,STADS2JN),(5,P+50)                                
*        MVC   P+70(02),REP2                                                    
*        CLI   HNOPEN,C'H'         HN START DATE USED?                          
*        BE    XTRA0030                                                         
*        MVC   P+70(02),REP1                                                    
XTRA0030 EQU   *                                                                
*        MVC   P+73(15),=C'START DATE USED'                                     
*        GOTO1 REPORT                                                           
*        B     XTRA0150            EXIT                                         
XTRA0040 EQU   *                                                                
*        OC    DIOPEN,DIOPEN       DI CURRENT?                                  
*        BZ    XTRA0100            NO                                           
*                                  YES - DI USED TO OUTPUT RECORD               
*        MVC   P+1(10),=C'HN JOIN:  '                                           
*        GOTO1 DATCON,DMCB,(3,STADS2JN),(5,P+11)                                
*        MVC   P+20(10),=C'HN LEFT:  '                                          
*        GOTO1 DATCON,DMCB,(3,STADS2LV),(5,P+30)                                
*        MVC   P+40(10),=C'DI JOIN:  '                                          
*        GOTO1 DATCON,DMCB,(3,STADS1JN),(5,P+50)                                
*        MVC   P+70(02),REP1                                                    
*        CLI   DIOPEN,C'D'         DI START DATE USED?                          
*        BE    XTRA0060                                                         
*        MVC   P+70(02),REP2                                                    
XTRA0060 EQU   *                                                                
*        MVC   P+73(15),=C'START DATE USED'                                     
*        GOTO1 REPORT                                                           
*        B     XTRA0150            EXIT                                         
XTRA0100 EQU   *                                                                
*        OC    BOTHLEFT,BOTHLEFT   BOTH STATIONS LEFT?                          
*        BZ    XTRA0900            NO  - FINISHED                               
*        MVC   P+1(10),=C'HN JOIN:  '                                           
*        GOTO1 DATCON,DMCB,(3,STADS2JN),(5,P+11)                                
*        MVC   P+20(10),=C'HN LEFT:  '                                          
*        GOTO1 DATCON,DMCB,(3,STADS2LV),(5,P+30)                                
*        MVC   P+40(10),=C'DI JOIN:  '                                          
*        GOTO1 DATCON,DMCB,(3,STADS1JN),(5,P+50)                                
*        MVC   P+60(10),=C'DI LEFT:  '                                          
*        GOTO1 DATCON,DMCB,(3,STADS1LV),(5,P+70)                                
*        GOTO1 REPORT                                                           
*        B     XTRA0150                                                         
XTRA0150 EQU   *                                                                
         MVC   P+1(11),=C'REP1 GROUP:'                                          
         MVC   P+21(11),=C'REP2 GROUP:'                                         
         MVC   P+41(11),=C'REP3 GROUP:'                                         
         MVC   P+12(2),STADS1GP                                                 
         MVC   P+32(2),STADS2GP                                                 
         MVC   P+52(2),STADS3GP                                                 
         OC    STADS1GP,STADS1GP   ANY REP1 GROUP?                              
         BZ    XTRA0158            NO  - COMPARE REP2 VS REP3                   
         OC    STADS2GP,STADS2GP   YES - ANY REP2 GROUP?                        
         BZ    XTRA0156            NO  - COMPARE REP1 VS REP3                   
         CLC   STADS1GP,STADS2GP   YES - REP1 = REP2 GROUP?                     
         BNE   XTRA0160            NO                                           
         OC    STADS3GP,STADS3GP   YES - ANY REP3 GROUP?                        
         BZ    XTRA0180            NO  - REP1 = REP2                            
XTRA0156 EQU   *                                                                
         CLC   STADS3GP,STADS1GP   YES - REP3 = REP1?                           
         BE    XTRA0180            YES - SAME GROUP!!                           
         B     XTRA0160            NO  - GROUP DIFFERENCE                       
XTRA0158 EQU   *                                                                
         CLC   STADS2GP,STADS3GP   YES - REP2 = REP2?                           
         BE    XTRA0180            YES - SAME GROUP!!                           
XTRA0160 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+10(28),=C'GROUP/SUBGROUP DIFFERENCE!!!'                        
         MVC   P+40(2),SAVEGRP                                                  
         MVC   P+43(13),=C'WILL BE USED!'                                       
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
         MVC   P+1(11),=C'REP1 CLOSE:'                                          
         MVC   P+21(11),=C'REP2 CLOSE:'                                         
         MVC   P+41(11),=C'REP3 CLOSE:'                                         
         GOTO1 DATCON,DMCB,(3,STADS1CL),(6,P+12)                                
         GOTO1 DATCON,DMCB,(3,STADS2CL),(6,P+32)                                
         GOTO1 DATCON,DMCB,(3,STADS3CL),(6,P+52)                                
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
         MVC   COMDREPB,RSTAKREP   INSERT REP CODE FOR BUDGET                   
*********************************************************************           
*  TO DROP ALL BUDGET RECS FOR STATIONS WHICH EXIST IN BOTH COMPANIES,          
*     COMMENT OUT LINE IMMEDIATELY ABOVE, ACTIVATE LINE IMMEDIATELY             
*     BELOW, AND COMMENT OUT FORCED SETTING OF BUDGET REPS                      
*     BELOW, ALSO.                                                              
*                                                                               
*****>   MVC   COMDREPB(1),RSTAKREP                                             
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
*        CLC   COMDCALL,=C'WKSZF'  OVERRIDE THE VALUE                           
*        BNE   TSTA0020            NO                                           
*****>   MVC   COMDREPB,REP2       YES - FORCE 'HN' FOR BUDGET                  
*        MVC   COMDREPC,REP2       YES - FORCE 'HN' FOR COMMISSION              
TSTA0020 EQU   *                                                                
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
*  REGNPROC:  BRING OVER ALL REP1 REGION RECORDS.                *              
******************************************************************              
*                                                                               
REGNPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RREGREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,3               SET UP REGION KEY FOR REP1                   
         MVC   KEY+23(2),REP1                                                   
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
*  OFFCPROC:  WILL BRING OVER ALL REP1 OFFICE RECORDS.           *              
******************************************************************              
*                                                                               
OFFCPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING ROFFREC,R3                                                       
         MVI   KEYTYPE,4                                                        
OFFC0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP OFFICE KEY FOR REP1                   
         MVC   KEY+23(2),REP1                                                   
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
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
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
*  OBUDPROC:  WILL BRING OVER ALL REP1 OFFICE BUDGET RECORDS.    *              
******************************************************************              
*                                                                               
OBUDPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING ROBDREC,R3                                                       
         MVI   KEYTYPE,X'19'                                                    
OBUD0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP OFFBUD KEY FOR REP1                   
         MVC   KEY+17(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     OBUD0040                                                         
OBUD0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
OBUD0040 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     OFFICE BUDGET RECORD/SAME COMPANY?           
         BNE   OBUD0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),ROBDLEN    INSERT LENGTH FOR PUT                        
         MVC   ROBDKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OBUDCTR          BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OBUDCTR          SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY OFFICE BUDGET RECORD?                
         BNE   OBUD0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'OFFBUD RECORD:'                                       
         MVC   P+20(10),ROBDKREP                                                
         MVI   P+30,C'/'                                                        
         MVC   P+31(2),KEY+17                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     OBUD0020            GO BACK FOR NEXT                             
OBUD0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PGTRPROC:  WILL BRING OVER ALL REP1 PROGRAM TYPE RECORDS.     *              
******************************************************************              
*                                                                               
PGTRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RPGTREC,R3                                                       
         MVI   KEYTYPE,X'25'                                                    
PGTR0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP PROGRAM TYPE FOR REP1                 
         MVC   KEY+24(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     PGTR0040                                                         
PGTR0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
PGTR0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     PROGTYPE RECORD/SAME COMPANY?                
         BNE   PGTR0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RPGTLEN    INSERT LENGTH FOR PUT                        
         MVC   RPGTKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,SRACTR           BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,SRACTR           SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY PROGRAM TYPE RECORD?                 
         BNE   PGTR0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'PROGRAM TYPE :'                                       
         MVC   P+20(03),RPGTKREP                                                
         MVI   P+24,C'/'                                                        
         MVC   P+25(2),KEY+24                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     PGTR0020            GO BACK FOR NEXT                             
PGTR0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SCOMPROC:  WILL BRING OVER ALL REP1 STD COMMENT  RECORDS.     *              
******************************************************************              
*                                                                               
SCOMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RCMTREC,R3                                                       
         MVI   KEYTYPE,X'2E'                                                    
SCOM0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP PROGRAM TYPE FOR REP1                 
         MVC   KEY+15(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     SCOM0040                                                         
SCOM0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
SCOM0040 EQU   *                                                                
         CLC   KEY(17),KEYSAVE     STANDARD COMMT SAME REC/SAME COMP?           
         BNE   SCOM0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RCMTLEN    INSERT LENGTH FOR PUT                        
         MVC   RCMTKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,SCOMCTR          BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,SCOMCTR          SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY STD COMMENT RECORD?                  
         BNE   SCOM0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'STD COMMENT  :'                                       
         MVC   P+20(12),RCMTKREP                                                
         MVI   P+32,C'/'                                                        
         MVC   P+33(2),KEY+15                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     SCOM0020            GO BACK FOR NEXT                             
SCOM0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  RADRPROC:  WILL BRING OVER ALL REP1 RADAR        RECORDS.     *              
******************************************************************              
*                                                                               
RADRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RRDAREC,R3                                                       
         MVI   KEYTYPE,X'33'                                                    
RADR0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP RADAR RECORD FOR REP1                 
         MVC   KEY+17(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     RADR0040                                                         
RADR0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
RADR0040 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     RADAR SAME REC/SAME COMP?                    
         BNE   RADR0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RRDALEN    INSERT LENGTH FOR PUT                        
         MVC   RRDAKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,RDACTR           BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,RDACTR           SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY RADAR RECORD?                        
         BNE   RADR0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'RADAR RECORD :'                                       
         MVC   P+20(10),RRDAKREP                                                
         MVI   P+30,C'/'                                                        
         MVC   P+31(2),KEY+17                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     RADR0020            GO BACK FOR NEXT                             
RADR0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  OCMTPROC:  WILL BRING OVER ALL REP1 OFF COMMENT  RECORDS.     *              
******************************************************************              
*                                                                               
OCMTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING ROCMREC,R3                                                       
         MVI   KEYTYPE,X'34'                                                    
OCMT0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP OFF COMMENT RECORD FOR REP1           
         MVC   KEY+20(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     OCMT0040                                                         
OCMT0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
OCMT0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     OFF/COMMENT SAME REC/SAME COMP?              
         BNE   OCMT0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),ROCMLEN    INSERT LENGTH FOR PUT                        
         MVC   ROCMKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OCOMCTR          BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OCOMCTR          SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY OFFICE COMMENT RECORD?               
         BNE   OCMT0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'OFF COMMENT  :'                                       
         MVC   P+20(07),ROCMKREP                                                
         MVI   P+27,C'/'                                                        
         MVC   P+28(2),KEY+20                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     OCMT0020            GO BACK FOR NEXT                             
OCMT0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  LABLPROC:  WILL BRING OVER ALL REP1 LABEL        RECORDS.     *              
******************************************************************              
*                                                                               
LABLPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RLABREC,R3                                                       
         MVI   KEYTYPE,X'36'                                                    
LABL0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP LABEL RECORD FOR REP1                 
         MVC   KEY+17(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     LABL0040                                                         
LABL0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
LABL0040 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     LABEL SAME REC/SAME COMP?                    
         BNE   LABL0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RLABLEN    INSERT LENGTH FOR PUT                        
         MVC   RLABREP,NEWREP      INSERT NEW REP CODE                          
         L     RF,LABLCTR          BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,LABLCTR          SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY OFFICE COMMENT RECORD?               
         BNE   LABL0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'LABEL RECORD :'                                       
         MVC   P+20(10),RLABREP                                                 
         MVI   P+30,C'/'                                                        
         MVC   P+31(2),KEY+17                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     LABL0020            GO BACK FOR NEXT                             
LABL0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  TYPEPROC:  WILL BRING OVER ALL REP1 TYPE         RECORDS.     *              
******************************************************************              
*                                                                               
TYPEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RTYPREC,R3                                                       
         MVI   KEYTYPE,X'30'                                                    
TYPE0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP TYPE  RECORD FOR REP1                 
         MVC   KEY+17(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     TYPE0040                                                         
TYPE0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
TYPE0040 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     TYPE  SAME REC/SAME COMP?                    
         BNE   TYPE0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RTYPLEN    INSERT LENGTH FOR PUT                        
         MVC   RTYPKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,TYPECTR          BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,TYPECTR          SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY TYPE RECORD?                         
         BNE   TYPE0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'TYPE  RECORD :'                                       
         MVC   P+20(10),RTYPKREP                                                
         MVI   P+30,C'/'                                                        
         MVC   P+31(2),KEY+17                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     TYPE0020            GO BACK FOR NEXT                             
TYPE0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SETRPROC:  WILL BRING OVER ALL REP1 SET          RECORDS.     *              
******************************************************************              
*                                                                               
SETRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RSETREC,R3                                                       
         MVI   KEYTYPE,X'38'                                                    
SETR0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP SET   RECORD FOR REP1                 
         MVC   KEY+19(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     SETR0040                                                         
SETR0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
SETR0040 EQU   *                                                                
         CLC   KEY(21),KEYSAVE     SET   SAME REC/SAME COMP?                    
         BNE   SETR0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RSETLEN    INSERT LENGTH FOR PUT                        
         MVC   RSETKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,SETCTR           BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,SETCTR           SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY SET RECORD?                          
         BNE   SETR0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'SET   RECORD :'                                       
         MVC   P+20(08),RSETKREP                                                
         MVI   P+30,C'/'                                                        
         MVC   P+31(2),KEY+19                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     SETR0020            GO BACK FOR NEXT                             
SETR0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  TERRPROC:  WILL BRING OVER ALL REP1 TERRITORY    RECORDS.     *              
******************************************************************              
*                                                                               
TERRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RTERREC,R3                                                       
         MVI   KEYTYPE,X'3D'                                                    
TERR0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP TERR RECORD FOR REP1                  
         MVC   KEY+23(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     TERR0040                                                         
TERR0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
TERR0040 EQU   *                                                                
         CLC   KEY(25),KEYSAVE     TERR  SAME REC/SAME COMP?                    
         BNE   TERR0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RTERLEN    INSERT LENGTH FOR PUT                        
         MVC   RTERKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,TERRCTR          BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,TERRCTR          SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY TERRITORY RECORD?                    
         BNE   TERR0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'TERR  RECORD :'                                       
         MVC   P+20(04),RTERKREP                                                
         MVI   P+24,C'/'                                                        
         MVC   P+25(2),KEY+23                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     TERR0020            GO BACK FOR NEXT                             
TERR0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SCTYPROC:  WILL BRING OVER ALL REP1 CONTRACT TYPE RECORDS.    *              
******************************************************************              
*                                                                               
SCTYPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RCMTREC,R3                                                       
         MVI   KEYTYPE,X'32'                                                    
SCTY0010 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),KEYTYPE      SET UP PROGRAM TYPE FOR REP1                 
         MVC   KEY+24(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     SCTY0040                                                         
SCTY0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
SCTY0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     CONTRACT TYPE SAME REC/SAME COMP?            
         BNE   SCTY0100            NO  - FINISHED                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RCTYLEN    INSERT LENGTH FOR PUT                        
         MVC   RCTYKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,CTYPCTR          BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,CTYPCTR          SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+4,C'Y'      DISPLAY CONTRACT TYPE RECORD?                
         BNE   SCTY0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'CONTRCT TYPE :'                                       
         MVC   P+20(03),RCTYKREP                                                
         MVI   P+24,C'/'                                                        
         MVC   P+25(2),KEY+24                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         B     SCTY0020            GO BACK FOR NEXT                             
SCTY0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SALEPROC:  WILL BRING OVER ALL REP1 S/P    RECORDS.           *              
******************************************************************              
*                                                                               
SALEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RSALREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,6               SET UP S/P    KEY FOR REP1                   
         MVC   KEY+22(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     SALE0040                                                         
SALE0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
SALE0040 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     S/P    RECORD/SAME COMPANY?                  
         BNE   SALE0100            NO  - NOW TABLE REP2/REP3 CODES              
*                                     FOR TRANSLATING CONTRACTS                 
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     INSERT NEW REP CODE                          
         L     RF,OTHERCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,OTHERCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
*                                                                               
*   TABLE REP1 S/P CODE, REP ID                                                 
*                                                                               
*        L     R2,ANEXTSAL         SET A(NEXT SLOT FOR S/P)                     
*        MVC   0(3,R2),RSALKSAL    INSERT S/P INTO TABLE                        
*        MVC   3(2,R2),RSALKREP    INSERT REP CODE INTO TABLE                   
*        XC    5(11,R2),5(R2)      CLEAR REST+NEXT ENTRY                        
*        LA    R2,8(R2)            BUMP TO NEXT ENTRY                           
*        ST    R2,ANEXTSAL         SAVE A(NEXT S/P SLOT)                        
*                                                                               
         CLI   QUESTOR+5,C'Y'      DISPLAY SALESPERSON RECORD?                  
         BNE   SALE0020            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'S/P REP1 RECD:'                                       
         MVC   P+20(05),RSALKREP                                                
         MVI   P+25,C'/'                                                        
         MVC   P+26(2),KEY+22                                                   
         GOTO1 REPORT                                                           
***>>>   GOTO1 DISPPUT,DMCB,(RC)                                                
***>>>   GOTO1 REPORT              INSERT A BLANK LINE                          
         B     SALE0020            GO BACK FOR NEXT                             
SALE0100 EQU   *                                                                
         LA    RF,NEWSPALF         SET A(1ST/2ND ALFA CHARS)                    
         ST    RF,ANSPALF          SET A(1ST ALFA)                              
         ST    RF,ANSPALF2         SET A(2ND ALFA)                              
         MVC   ACTIVREP,REP2                                                    
SALE0110 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,6               SET UP S/P    KEY FOR REP2                   
         MVC   KEY+22(2),ACTIVREP  INSERT REP                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     SALE0140                                                         
SALE0120 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
SALE0140 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     S/P    RECORD/SAME COMPANY?                  
         BE    SALE0142            YES - FINISHED                               
         CLC   ACTIVREP,REP3       REP3 DONE?                                   
         BE    SALE0240            YES - FINISHED                               
         MVC   ACTIVREP,REP3       NO  - INSERT REP3                            
         B     SALE0110            GO BACK AND DO REP3                          
SALE0142 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         L     R2,ANEXTSAL                                                      
         MVC   0(3,R2),RSALKSAL    INSERT SALESPERSON INTO TABLE                
         MVC   3(2,R2),RSALKREP    INSERT REP CODE INTO TABLE                   
         XC    5(11,R2),5(R2)      CLEAR TRANSLATE CODE + NEXT ENTRY            
         OC    RSALMRG,RSALMRG     ANY MERGE ENTRY?                             
         B     SALE0160            FORCE CREATE                                 
***>>>   BZ    SALE0160            NO  - CREATE, AND DISPLAY MESSAGE            
         MVC   3(5,R2),RSALMRG     YES - INSERT TRANSLATE CODE                  
         CLI   QUESTOR+5,C'Y'      DISPLAY SALESPERSON SETUP?                   
         BE    SALE0145            YES                                          
         CLI   QUESTOR+5,C'B'      DISPLAY SALESPERSON SETUP + RECORD?          
         BNE   SALE0150            NO                                           
SALE0145 EQU   *                                                                
         MVC   P+1(22),=C'S/P XLATE SET UP:     '                               
         MVC   P+20(2),RSALKREP    INSERT REP CODE                              
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
         MVC   P+1(22),=C'S/P XLATE CREATED:    '                               
         MVC   P+20(2),RSALKREP    INSERT REP CODE                              
         MVC   P+25(3),RSALKSAL    INSERT S/P CODE                              
         MVC   P+30(20),RSALNAME   INSERT NAME                                  
         MVC   P+55(3),=C'-->'                                                  
         L     RF,ANSPALF                                                       
         MVC   P+60(1),0(RF)       NEW ALPHA CODE: 1ST POSITION                 
         L     RF,ANSPALF2                                                      
         MVC   P+61(1),0(RF)       NEW ALPHA CODE: 2ND POSITION                 
         EDIT  NEWSPNUM,(1,P+62),FILL=0                                         
         MVC   3(2,R2),RSALKREP    INSERT REP CODE INTO TABLE                   
         MVC   5(3,R2),P+60        INSERT NEW CODE INTO TABLE                   
         GOTO1 REPORT              DISPLAY MESSAGE                              
         ZIC   RF,NEWSPNUM         BUMP NUMBER                                  
         LA    RF,1(RF)                                                         
         STC   RF,NEWSPNUM         STORE IT BACK                                
         CLI   NEWSPNUM,10         NEW ALPHA NEEDED?                            
         BNE   SALE0180            NO                                           
         MVI   NEWSPNUM,0          CLEAR COUNTER                                
*                                                                               
         L     RF,ANSPALF2         YES - BUMP TO NEXT CHARACTER                 
         LA    RF,1(RF)            BUMP A(2ND CHAR)                             
         CLI   0(RF),C'*'          DELIMITER REACHED?                           
         BNE   SALE0170            NO  - BUMP ALF2 ADDR                         
         LA    RF,NEWSPALF         YES - RESET 2ND CHAR TO A                    
         ST    RF,ANSPALF2                                                      
         L     RF,ANSPALF          BUMP 1ST CHAR                                
         LA    RF,1(RF)                                                         
         ST    RF,ANSPALF                                                       
         B     SALE0180                                                         
SALE0170 EQU   *                                                                
         ST    RF,ANSPALF2         STORE IT BACK                                
SALE0180 EQU   *                                                                
         MVC   SAVESALE,RSALKSAL   SAVE ORIGINAL SALESPERSON CODE               
         MVC   RSALKSAL,5(R2)      INSERT TABLE CODE INTO RECORD                
         MVC   RSALMRG,SAVESALE    INSERT ORIGINAL SALESPERSON CODE             
         MVC   REC-4(2),RSALLEN    INSERT LENGTH FOR PUT                        
         MVC   RSALKREP,NEWREP     INSERT NEW REP CODE                          
         OI    RSALFLG,X'80'       TURN ON FORMAT INDICATOR                     
         L     RF,SPCRTCTR         BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,SPCRTCTR         SAVE IT BACK                                 
         BAS   RE,PUTRECS          GENERATE NEW OUTPUT                          
         CLI   QUESTOR+5,C'Y'      DISPLAY S/P RECORD?                          
         BNE   SALE0200            NOT SPECIAL REQUEST                          
         MVC   P+1(14),=C'S/P REP2/3   :'                                       
         MVC   P+20(05),RSALKREP                                                
         MVI   P+25,C'/'                                                        
         MVC   P+26(2),KEY+22                                                   
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)   YES                                          
         GOTO1 REPORT              INSERT A BLANK LINE                          
SALE0200 EQU   *                                                                
         GOTO1 REPORT              DISPLAY MESSAGE                              
SALE0220 EQU   *                                                                
         LA    R2,8(R2)            BUMP TO NEXT SLOT                            
         ST    R2,ANEXTSAL         SAVE A(NEXT SLOT)                            
         B     SALE0120            GO BACK FOR NEXT                             
SALE0240 EQU   *                                                                
         CLI   QUESTOR+5,C'Y'      DISPLAY S/P RECORD?                          
         BNE   SALE0280            NO                                           
         L     R2,ASALAREA         RESET A(S/P AREA)                            
         MVC   P+1(10),=C'S/P TABLE:'                                           
         GOTO1 REPORT                                                           
SALE0260 EQU   *                                                                
         MVC   P+1(72),0(R2)                                                    
         GOTO1 REPORT                                                           
         LA    R2,72(R2)                                                        
         OC    0(8,R2),0(R2)       1ST SLOT EMPTY FOR LINE?                     
         BNZ   SALE0260            NO  - GO BACK FOR NEXT                       
SALE0280 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  BUDGPROC:  WILL BRING OVER ALL REP BUDGET RECORDS, BASED ON   *              
*        WHICH REP WAS USED IN CASES WHERE > 1 REP HAD STATION   *              
******************************************************************              
*                                                                               
BUDGPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RBUDREC,R3                                                       
         MVC   REPCODE,REP1        PROCESS REP1 FIRST                           
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
         CLC   REPCODE,REP3        REP3 PROCESSED?                              
         BE    BUDG0120            YES - FINISHED                               
         CLC   REPCODE,REP2        NO  - REP2 PROCESSED?                        
         BNE   BUDG0110            NO  - PROCESS REP2                           
         MVC   REPCODE,REP3        YES - PROCESS REP3                           
         B     BUDG0010            GO BACK AND DO REP3                          
BUDG0110 EQU   *                                                                
         MVC   REPCODE,REP2        PROCESS REP2                                 
         B     BUDG0010            GO BACK AND DO REP2                          
BUDG0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  BUDGALT :  CHECK TABLE FOR STATION/REP CODE CHOICE.           *              
*        EXIT:  CC  ZERO     = SKIP RECORD                       *              
*               CC  NOT ZERO = USE RECORD                        *              
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
         BNE   BALT0120            NO  - EXIT CC ZERO                           
         CLC   KEY+18(2),=C'96'    YES - IS YEAR EARLIER THAN 96?               
         BL    BALT0120            YES - EXIT CC ZERO                           
         B     BALT0100            NO  - EXIT CC NOT ZERO                       
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
*  EOMPROC:   WILL BRING OVER ALL REP1 EOM  RECORDS.             *              
******************************************************************              
*                                                                               
EOMPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING REOMREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'18'           SET UP EOM    KEY FOR REP1                   
         MVC   KEY+24(2),REP1                                                   
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
*  DYPTPROC:  WILL BRING OVER ALL REP1 DAYPART RECORDS           *              
******************************************************************              
*                                                                               
DYPTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RDPTREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'24'           SET UP DAYPART KEY FOR REP1                  
         MVC   KEY+24(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     DYPT0040                                                         
DYPT0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
DYPT0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     DAYPRT RECORD/SAME COMPANY?                  
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
*  DYDFPROC:  WILL BRING OVER ALL REP1 DAYPART DEFINITION RECORDS*              
******************************************************************              
*                                                                               
DYDFPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RSDDREC,R3                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'26'           SET UP DAYPART DEFIN KEY FOR REP1            
         MVC   KEY+20(2),REP1                                                   
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     DYDF0040                                                         
DYDF0020 EQU   *                                                                
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
DYDF0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     DYPT DEF RECORD/SAME COMPANY?                
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
*  COMMPROC:  WILL BRING OVER ALL REP COMM   RECORDS, BASED ON   *              
*        WHICH REP WAS USED IN CASES WHERE > 1 REP HAD STATION   *              
******************************************************************              
*                                                                               
COMMPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RCOMREC,R3                                                       
         MVC   REPCODE,REP1        PROCESS REP1 FIRST                           
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
         CLC   REPCODE,REP3        REP3 PROCESSED?                              
         BE    COMM0120            YES - FINISHED                               
         CLC   REPCODE,REP2        NO  - REP2 PROCESSED?                        
         BNE   COMM0110            NO  - PROCESS REP2                           
         MVC   REPCODE,REP3        YES - PROCESS REP3                           
         B     COMM0010            GO BACK AND DO REP3                          
COMM0110 EQU   *                                                                
         MVC   REPCODE,REP2        PROCESS REP2                                 
         B     COMM0010            GO BACK AND DO REP2                          
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
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.  NO CONTRACT NUMBER *              
*     ADJUSTMENTS ARE REQUIRED. REPLACE S/P CODE FROM TABLE.     *              
*             REPLACE GROUP/SUBGROUP AS APPROPRIATE.             *              
*                                                                *              
******************************************************************              
*                                                                               
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RCONREC,R3                                                       
         MVC   REPCODE,REP1        SET CODE FOR REP1                            
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
         CLI   QUESTOR+7,C'#'      USE TEST COUNTS?                             
         BNE   CPRO0070            NO                                           
         CLC   NUMCONS,=F'10000'   **TEST CUTOFF**                              
         BH    CPRO0600            NUMBER PROCESSED:  SKIP OUT                  
CPRO0070 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD            YES - RETRIEVE RECORD                        
         CLC   REPCODE,REP2        REP2 BEING PROCESSED?                        
         BE    CPRO0075            YES - S/P                                    
         CLC   REPCODE,REP3        NO  - REP3 BEING PROCESSED?                  
         BNE   CPRO0080            NO  - DON'T CHANGE NUMBERS, S/P              
CPRO0075 EQU   *                                                                
***      GOTO1 NUMDELTA,DMCB,RCONKCON    CHANGE CONTRACT NUMBER                 
***      BAS   RE,CMBDELTA         CHANGE COMBO CONTROL ELEMENT                 
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
         CLC   REPCODE,REP1        REP1 PROCESSED?                              
         BNE   CPRO0604            YES -                                        
         MVC   REPCODE,REP2        NO  - PROCESS IT                             
         XC    NUMCONS,NUMCONS     CLEAR THE COUNTER                            
         B     CPRO0020            GO BACK AND DO IT                            
CPRO0604 EQU   *                                                                
         CLC   REPCODE,REP2        REP2 PROCESSED?                              
         BNE   CPRO0608            YES -                                        
         MVC   REPCODE,REP3        NO  - PROCESS IT                             
         XC    NUMCONS,NUMCONS     CLEAR THE COUNTER                            
         B     CPRO0020            GO BACK AND DO IT                            
CPRO0608 EQU   *                                                                
         CLC   REPCODE,REP3        REP3 PROCESSED?                              
         BE    CPRO0620            YES - FINISHED                               
         DC    H'0'                UNRECOGNIZED REPCODE                         
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
         CLC   RCONKREP,3(RF)      REP = TABLE?                                 
         BNE   SPDE0040            NO  - CHECK NEXT ENTRY                       
         MVC   RCONSAL,5(RF)       YES - INSERT CHANGE TO S/P                   
         B     SPDE0060            FINISHED                                     
SPDE0040 EQU   *                                                                
         LA    RF,8(RF)            BUMP TO NEXT ENTRY                           
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
SPCRTCTR DS    F                                                                
OBUDCTR  DS    F                                                                
SRACTR   DS    F                                                                
SCOMCTR  DS    F                                                                
CTYPCTR  DS    F                                                                
RDACTR   DS    F                                                                
OCOMCTR  DS    F                                                                
LABLCTR  DS    F                                                                
TYPECTR  DS    F                                                                
SETCTR   DS    F                                                                
TERRCTR  DS    F                                                                
ANSPALF  DS    F                                                                
ANSPALF2 DS    F                                                                
CNUMAREA DS    CL8                                                              
RNUMAREA DS    CL8                                                              
SAVEGRP  DS    CL2                 GROUP/SUBGROUP TO USE                        
SAVESALE DS    CL3                                                              
NEWSPALF DC    CL27'ABCDEFGHIJKLMNOPQRSTUVWXYZ*'                                
*                                  NEW SALESPERSON CODE LETTERS                 
NEWSPNUM DC    XL1'00'             NUMBER SALESPERSON NUMBER                    
NEWREP   DC    CL2'AQ'             NEW REP CODE                                 
REP1     DC    CL2'I1'             1ST REP                                      
REP2     DC    CL2'TO'             2ND REP                                      
REP3     DC    CL2'CM'             3RD REP                                      
FLAGBYTS DS    0CL12               FLAGS                                        
BOTHOPEN DS    CL1                 NEITHER STATION LEFT                         
HNOPEN   DS    CL1                                                              
DIOPEN   DS    CL1                                                              
BOTHLEFT DS    CL1                                                              
ACTIVREP DS    CL2                                                              
REPCODE  DS    CL2                                                              
KEYTYPE  DS    CL1                                                              
DBLSPACE DS    CL1                                                              
DATEWORK DS    CL24                DATE WORK AREA                               
*                                                                               
DATETABL DS    0CL33                                                            
DLVDATE  EQU   *-LEAVDAT1                                                       
LEAVDAT1 DS    CL3                                                              
DJNDATE  EQU   *-LEAVDAT1                                                       
JOINDAT1 DS    CL3                                                              
DL2DATE  EQU   *-LEAVDAT1                                                       
LEAVDT1A DS    CL3                                                              
DRPTABL  EQU   *-LEAVDAT1                                                       
REPTABL1 DS    CL2                                                              
LDTBLSLT EQU   *-LEAVDAT1                                                       
LEAVDAT2 DS    CL3                                                              
JOINDAT2 DS    CL3                                                              
LEAVDT2A DS    CL3                                                              
REPTABL2 DS    CL2                                                              
LEAVDAT3 DS    CL3                                                              
JOINDAT3 DS    CL3                                                              
LEAVDT3A DS    CL3                                                              
REPTABL3 DS    CL2                                                              
DTABLADR DS    F                                                                
DTABLCTR DS    F                                                                
GENREP   DS    CL2                                                              
JOINDATE DS    CL3                                                              
*                                                                               
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
         A     RF,=F'16000'        LEAVE ROOM FOR 2000 ENTRIES                  
*                                     8 CHARS * 2000 SLOTS                      
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
*  BUYPROC:   RETRIEVE ALL BUY RECORDS.  NO CON# ADJUSTMENTS     *              
*     ARE NECESSARY.                                             *              
******************************************************************              
*                                                                               
BUYPROC  NMOD1 0,*BUYP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RBUYREC,R3                                                       
         MVC   REPCODE,REP1        SET CODE FOR DI                              
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
         CLI   QUESTOR+7,C'#'      USE TEST COUNT?                              
         BNE   BPRO0070            NO                                           
         CLC   NUMBUYS,=F'50000'   **TEST CUTOFF**                              
         BH    BPRO0600            NUMBER PROCESSED:  SKIP OUT                  
BPRO0070 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECR2            YES - RETRIEVE RECORD                        
*                                                                               
*   BUY CONTRACT NUMBERS NOT BEING ADJUSTED: NEXT ROUTINE IGNORED.              
*                                                                               
***>>>   CLC   REPCODE,REP2        REPN BEING PROCESSED?                        
***>>>   BNE   BPRO0080            NO  - DON'T CHANGE NUMBERS                   
***>>>   GOTO1 BUYDELTA,DMCB,RBUYKCON    CHANGE CONT# IN BUY RECD               
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
         CLC   REPCODE,REP1        REP1 PROCESSED?                              
         BNE   BPRO0604            YES -                                        
         MVC   REPCODE,REP2        NO  - PROCESS IT                             
         XC    NUMBUYS,NUMBUYS     CLEAR THE COUNTER                            
         B     BPRO0020            GO BACK AND DO IT                            
BPRO0604 EQU   *                                                                
         CLC   REPCODE,REP2        REP2 PROCESSED?                              
         BNE   BPRO0608            YES -                                        
         MVC   REPCODE,REP3        NO  - PROCESS IT                             
         XC    NUMBUYS,NUMBUYS     CLEAR THE COUNTER                            
         B     BPRO0020            GO BACK AND DO IT                            
BPRO0608 EQU   *                                                                
         CLC   REPCODE,REP3        REP3 PROCESSED?                              
         BE    BPRO0620            YES - FINISHED                               
         DC    H'0'                UNRECOGNIZED REPCODE                         
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
         MVC   P+1(24),=C'S/P RECORDS CREATED    :'                             
         EDIT  SPCRTCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OFFICE BUDGET PROCESSED:'                             
         EDIT  OBUDCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'PROGRAM TYPE  PROCESSED:'                             
         EDIT  SRACTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STD COMMENTS  PROCESSED:'                             
         EDIT  SCOMCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACT TYPE PROCESSED:'                             
         EDIT  CTYPCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'RADAR RECORDS PROCESSED:'                             
         EDIT  RDACTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OFFICE COMMTS PROCESSED:'                             
         EDIT  OCOMCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'LABEL RECORDS PROCESSED:'                             
         EDIT  LABLCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TYPE RECORDS  PROCESSED:'                             
         EDIT  TYPECTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SET RECORDS   PROCESSED:'                             
         EDIT  SETCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TERRITORY(S)  PROCESSED:'                             
         EDIT  TERRCTR,(12,P+30),COMMAS=YES                                     
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
STADS1JN DS    CL3                 S1 DATE JOINED                               
STADS1LV DS    CL3                 S1 DATE LEFT                                 
STADS2JN DS    CL3                 S2 DATE JOINED                               
STADS2LV DS    CL3                 S2 DATE LEFT                                 
STADS3JN DS    CL3                 S3 DATE JOINED                               
STADS3LV DS    CL3                 S3 DATE LEFT                                 
STADS1GP DS    CL2                 S1 GROUP/SUBGROUP                            
STADS2GP DS    CL2                 S2 GROUP/SUBGROUP                            
STADS3GP DS    CL2                 S3 GROUP/SUBGROUP                            
STADS1CL DS    CL2                 S1 CLOSED THROUGH DATE                       
STADS2CL DS    CL2                 S2 CLOSED THROUGH DATE                       
STADS3CL DS    CL2                 S3 CLOSED THROUGH DATE                       
STADS1DA DS    CL4                 S1 DISK ADDRESS OF RECORD                    
STADS2DA DS    CL4                 S2 DISK ADDRESS OF RECORD                    
STADS3DA DS    CL4                 S3 DISK ADDRESS OF RECORD                    
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
       ++INCLUDE REGENOBUD         OFFICE BUDGET RECORD                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOCM          OFFICE COMMENT RECORD                        
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPGTX         PROGRAM TYPE  RECORD                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENLAB          LABEL  RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCMT          STANDARD COMMENT RECORD                      
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTY          CONTRACT TYPE RECORD                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENRDA          RADAR RECORD                                 
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOM          END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDPTA         DAYPART RECORD                               
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
         ORG   RECORD                                                           
       ++INCLUDE REGENTER          TERRITORY RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSET          SET DEFINITION                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTYP          TYPE (30) DEFINITION                         
         EJECT                                                                  
       ++INCLUDE REGENSDD          DAYPART DEFINITION                           
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036REREPSW03 05/01/02'                                      
         END                                                                    
