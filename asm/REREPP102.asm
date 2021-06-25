*          DATA SET REREPP102  AT LEVEL 033 AS OF 05/01/02                      
*PHASE REP102A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE REGENBUC                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE QSORT                                                                  
         TITLE 'REREPP102  (REP102A) --- PETRY CONVERSION'                      
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPP102  -- PETRY CONVERSION: CONTRACT FILE CONVERSION. *            
*                      ACCEPT PETRY TAPE, CONVERT CONTRACT RECORDS *            
*                      TO CONTRACTS AND BUYLINES IN DDS FORMAT     *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* MAY24/95 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                  *            
*  TO DISPLAY A RANGE OF CONTRACTS BEING CONVERTED (SERIAL COUNT   *            
*     FROM X TO Y)                                                 *            
*                                                                  *            
*     QRECORD+20-25  =  SIX=DIGIT LOW-COUNTER VALUE FOR DISPLAYS   *            
*     QRECORD+26-31  =  SIX=DIGIT HI -COUNTER VALUE FOR DISPLAYS   *            
*                       BOTH VALUES MUST BE ENTERED FOR DISPLAYS   *            
*                                                                  *            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   = Y  DISPLAY ADVERTISER INPUT                    *            
*     QUESTOR+1   = Y  DISPLAY ADVERTISER OUTPUT                   *            
*     QUESTOR+2   = Y  DISPLAY CONTRACT INPUT                      *            
*     QUESTOR+3   = Y  DISPLAY CONTRACT OUTPUT                     *            
*     QUESTOR+4   = Y  DISPLAY BUY      INPUT                      *            
*     QUESTOR+5   = Y  DISPLAY BUY      OUTPUT                     *            
*     QUESTOR+6   = Y  DISPLAY AGENCY TABLE                        *            
*                 = A  DISPLAY AGENCY TBL + O/P (1ST N RECS)       *            
*                 = B  DISPLAY AGENCY O/P ONLY (1ST N RECS)        *            
*     QUESTOR+7   = Y  DISPLAY STATION CODE TABLE                  *            
*                 = A  DISPLAY STATION CODE TABLE + MISSING STNS   *            
*                 = B  DISPLAY MISSING STATIONS                    *            
*     QUESTOR+8   = Y  DISPLAY S/P INPUT + FINAL TABLE             *            
*                 = A  DISPLAY S/P FINAL TABLE                     *            
*                 = B  DISPLAY S/P RECORDS OUTPUT                  *            
*     QUESTOR+9   = Y  DISPLAY S/P NAME TABLE                      *            
*     QUESTOR+10  =                                                *            
*     QUESTOR+11  =                                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REP102   CSECT                                                                  
         NMOD1 0,**REP1**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAIN0640                                                         
*                                                                               
MAIN0020 DS    0H                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTRT          SAVE START TIME                              
*                                                                               
         GOTO1 =A(INITIAL),DMCB,(RC),RR=Y                                       
         GOTO1 =A(TABLINIT),DMCB,(RC),RR=Y                                      
*                                                                               
*   TEST                                                                        
*        B     MAIN0420            **TEST                                       
*   TEST END                                                                    
*                                                                               
*                                                                               
MAIN0040 EQU   *                                                                
         GOTO1 =A(GETTPREC),DMCB,(RC),RR=Y                                      
*                                  UNBLOCK AND DELIVER TAPE REC                 
         BZ    MAIN0400            CC = ZERO = END OF FILE                      
*                                                                               
*   TEST                                                                        
*        CLC   CONCTR,=F'1'           PROCESS N CONTRACTS                       
*        BH    MAIN0400               THEN FORCE EOJ                            
*   TEST END                                                                    
*                                                                               
*                                                                               
*   TEST                                                                        
*        CLC   TOTCTR,=F'100'         PROCESS N RECORDS                         
*        BH    MAIN0400               THEN FORCE EOJ                            
*        GOTO1 =A(DISPITST),DMCB,(RC),RR=Y                                      
*   TEST END                                                                    
*                                                                               
         L     RF,TOTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR           INCREMENT TOTAL RECORDS READ                 
         L     RF,DISPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,DISPCTR          INCREMENT TOTAL RECORDS READ                 
         C     RF,=F'1000'         DISPLAY EVERY N RECORDS                      
         BNE   MAIN0060                                                         
         MVC   P+1(11),=C'PROCESSING:'                                          
         EDIT  TOTCTR,(10,P+15)     DISPLAY TOTAL COUNTER                       
         EDIT  CONCTR,(10,P+28)     DISPLAY CONTRACT COUNTER                    
         EDIT  BUYCTR,(10,P+41)     DISPLAY BUY      COUNTER                    
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+55,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
         XC    DISPCTR,DISPCTR                                                  
MAIN0060 EQU   *                                                                
         LA    RF,RECTABLE         IS RECORD TO BE PROCESSED?                   
MAIN0080 EQU   *                                                                
         CLC   RECORD2+30(2),=X'0020'                                           
**TEST2* CLC   RECORD2+30(2),=X'0010'                                           
**TEST3* CLC   RECORD2+30(2),=X'0004'                                           
*                                  LAST RECORD TO BE CONVERTED                  
*                                     COMPLETED?                                
         BH    MAIN0400            YES - FINISHED                               
         CLC   RECORD2+30(2),=X'0002'                                           
*                                  STATION RECORDS COMPLETED?                   
         BNH   MAIN0100            NO                                           
         CLI   CMPFLAG,C'N'        JUST COMPLETED?  (FIRST TIME)                
         BNE   MAIN0100                                                         
         MVI   CMPFLAG,C'Y'                                                     
         MVC   P+1(22),=C'COMPETITIVE STN COUNT:'                               
         EDIT  CMPCTR,(5,P+25)                                                  
         GOTO1 REPORT                                                           
*                                                                               
*   TEST                                                                        
*        L     R2,ACMPAREA                                                      
*        SR    R1,R1                                                            
*        LA    R3,2000                                                          
*        MVC   P+1(09),=C'COMP DUMP'                                            
*        GOTO1 REPORT                                                           
LOUP0010 EQU   *                                                                
*        OC    0(12,R2),0(R2)      ANYTHING IN FIELD?                           
*        BZ    MAIN0100            NO  - FINISHED                               
*        LA    R1,1(R1)                                                         
*        EDIT (R1),(5,P+1)                                                      
*        MVC   P+10(63),0(R2)                                                   
*        GOTO1 REPORT                                                           
*        LA    R2,63(R2)                                                        
*        BCT   R3,LOUP0010                                                      
*        DC    H'0'                DUMP AT END                                  
*   TEST END                                                                    
*                                                                               
MAIN0100 EQU   *                                                                
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    MAIN0040            YES - SKIP THIS RECORD TYPE                  
         CLC   0(2,RF),RECORD2+30  CHECK RECORD TYPE AGAINST TABLE              
         BE    MAIN0120            PROCESS                                      
         LA    RF,2(RF)            BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0080            GO BACK FOR NEXT                             
*                                                                               
RECTABLE EQU   *                                                                
*NOT CON DC    X'0001'             REP       RECORD                             
         DC    X'0002'             STATION   RECORD                             
*                                  NOT CONVERTED:  NEED COMPETITIVE             
*NOT CON DC    X'0003'             OFFICE    RECORD                             
         DC    X'0004'             S/P       RECORD                             
*NOT CON DC    X'0005'             GROUP     RECORD                             
*NOT CON DC    X'0006'             REGION    RECORD                             
*NOT CON DC    X'0007'             DIVISION  RECORD                             
*NOT CON DC    X'000A'             AGENCY    RECORD                             
         DC    X'000B'             ADVERT    RECORD                             
         DC    X'0010'             CONTRACT  RECORD                             
         DC    X'0020'             BUY       RECORD                             
*NOT CON DC    X'002B'             CATEGORY  RECORD                             
*NOT CON DC    X'002C'             CLASS     RECORD                             
         DC    X'FFFF'             DELIMITER                                    
MAIN0120 EQU   *                                                                
         CLC   =X'0001',RECORD2+30 REP       RECORD FOUND?                      
         BNE   MAIN0140            NO                                           
         GOTO1 REPRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0140 EQU   *                                                                
         CLC   =X'0002',RECORD2+30 STATION   RECORD FOUND?                      
         BNE   MAIN0160            NO                                           
         GOTO1 STARPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0160 EQU   *                                                                
         CLC   =X'0003',RECORD2+30 OFFICE    RECORD FOUND?                      
         BNE   MAIN0180            NO                                           
         GOTO1 OFFRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0180 EQU   *                                                                
         CLC   =X'0004',RECORD2+30 S/P       RECORD FOUND?                      
         BNE   MAIN0200            NO                                           
         GOTO1 SALRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0200 EQU   *                                                                
         CLC   =X'0005',RECORD2+30 GROUP     RECORD FOUND?                      
         BNE   MAIN0220            NO                                           
         GOTO1 GRORPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0220 EQU   *                                                                
         CLC   =X'0006',RECORD2+30 REGION    RECORD FOUND?                      
         BNE   MAIN0240            NO                                           
         GOTO1 REGRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0240 EQU   *                                                                
         CLC   =X'0007',RECORD2+30 DIVISION  RECORD FOUND?                      
         BNE   MAIN0260            NO                                           
         GOTO1 DIVRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0260 EQU   *                                                                
*        CLC   =X'000A',RECORD2+30 AGENCY    RECORD FOUND?                      
*        BNE   MAIN0280            NO                                           
*        GOTO1 AGYRPROC,DMCB,(RC)                                               
*        B     MAIN0040            GO BACK FOR NEXT                             
MAIN0280 EQU   *                                                                
         CLC   =X'000B',RECORD2+30 ADVERT    RECORD FOUND?                      
         BNE   MAIN0300            NO                                           
         GOTO1 ADVRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0300 EQU   *                                                                
         CLC   =X'0010',RECORD2+30 CONTRACT  RECORD FOUND?                      
         BNE   MAIN0320            NO                                           
         GOTO1 CONRPROC,DMCB,(RC)                                               
         MVI   CONFLAG,C'Y'        SET 'CONTRACTS FOUND'                        
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0320 EQU   *                                                                
         CLC   =X'0020',RECORD2+30 BUY       RECORD FOUND?                      
         BNE   MAIN0340            NO                                           
         GOTO1 BUYRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0340 EQU   *                                                                
         CLC   =X'002B',RECORD2+30 CATEGORY  RECORD FOUND?                      
         BNE   MAIN0360            NO                                           
         GOTO1 CATRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0360 EQU   *                                                                
         CLC   =X'002C',RECORD2+30 CLASS     RECORD FOUND?                      
         BNE   MAIN0380            NO                                           
         GOTO1 CLSRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0380 EQU   *                                                                
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0400 EQU   *                                                                
         CLI   BUYREC4,C'Y'        BUY RECORD IN RECORD4?                       
         BNE   MAIN0420            NO                                           
         L     RF,ARECORD4                                                      
         GOTO1 PUTRECS,DMCB,(RC),(RF)                                           
*                                  NO  - PUT RECORD4 TO OUTPUT                  
         L     RF,BUYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
         GOTO1 DISPPUT,DMCB,(RC)                                                
MAIN0420 EQU   *                                                                
         GOTO1 =A(SHOWSPS),DMCB,(RC),RR=RELO                                    
*                                                                               
         GOTO1 =A(GENSTATS),DMCB,(RC),RR=RELO                                   
*                                                                               
         GOTO1 =A(CATSMISS),DMCB,(RC),RR=RELO                                   
*                                                                               
         CLOSE INTAPE                                                           
         OPEN  (INTAPE,(INPUT))    REOPEN INPUT FOR AGENCY RECS                 
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVI   RCSUBPRG,0          TURN OFF SECONDARY HEADING                   
         MVC   P+1(21),=C'BEGINNING AGENCY PASS'                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVI   BLKFLAG,C'Y'        SET 'BLOCK EMPTY' FLAG AGAIN                 
         MVI   BLK#1,C'Y'          SET 'SKIP 1ST BLOCK' AGAIN                   
MAIN0440 EQU   *                                                                
         GOTO1 =A(GETTPREC),DMCB,(RC),RR=Y                                      
*                                  UNBLOCK AND DELIVER TAPE REC                 
         BZ    MAIN0600            CC = ZERO = END OF FILE                      
         CLC   RECORD2+30(2),=X'000A'                                           
*                                  AGENCY RECORD FOUND?                         
         BH    MAIN0600            NO  - ALL PASSED: FINISHED                   
         BNE   MAIN0440            NO  - GO BACK FOR NEXT                       
         GOTO1 =A(AGYRPROC),DMCB,(RC),RR=Y                                      
*                                  YES - CHECK IF NEW RECORD NEEDED             
         B     MAIN0440            GO BACK FOR NEXT                             
MAIN0600 EQU   *                                                                
         GOTO1 =A(DISPTOTS),DMCB,(RC),RR=RELO                                   
*                                  DISPLAY TOTALS FOR RUN                       
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
*                                                                               
MAIN0640 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  REPRPROC:  PROCESS THE REP       RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
REPRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  STARPROC:  PROCESS THE STATION   RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
STARPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   =C'PT',JSTAKREP     'PT' CONTRACT?                               
         BNE   STAR0900            NO  - SKIP THIS RECORD                       
         L     RF,CMPCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CMPCTR           COUNT NUMBER OF COMP STATIONS                
         L     R2,ACMPNXT          SET A(NEXT COMP AREA)                        
         SR    R7,R7               SET COUNTER TO ZERO                          
         L     R3,ACMPNXT                                                       
         LA    R3,7(R3)            BUMP TO FIRST COMPETITIVE                    
         MVC   0(4,R2),JSTAKSTA    INSERT REP'D STA INTO 1ST POSITION           
         MVC   4(2,R2),JSTAKGRP    INSERT GROUP/SUBGROUP                        
         MVI   JDSELT,X'20'        GET COMPETITIVE STATION ELEMENT              
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     STAR0040                                                         
STAR0020 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
STAR0040 EQU   *                                                                
         BNZ   STAR0200            NOT FOUND/FINISHED                           
         L     RF,JDSADDR          SET A(ELEMENT FOUND)                         
         MVC   0(4,R3),1(RF)       INSERT CALL LETTERS                          
         MVC   4(3,R3),7(RF)       INSERT AFFILIATION                           
         LA    R3,7(R3)            BUMP TO NEXT COMPETITIVE SLOT                
         LA    R7,1(R7)            INCREASE COUNTER                             
         B     STAR0020            GO BACK FOR NEXT ELEMENT                     
STAR0200 EQU   *                                                                
         C     R7,=F'8'            MORE THAN 8 COMPETITIVES?                    
         BNH   STAR0400            NO  - FINISHED                               
         MVC   P+1(15),=C'> 8 COMP STATS:'                                      
         MVC   P+17(4),JSTAKSTA                                                 
         GOTO1 REPORT                                                           
STAR0400 EQU   *                                                                
*        EDIT  CMPCTR,(5,P+1)                                                   
*        MVC   P+10(63),0(R2)      DISPLAY STATION COMPS                        
*        GOTO1 REPORT                                                           
         LA    R2,63(R2)           BUMP TO NEXT TABLE ENTRY                     
         ST    R2,ACMPNXT                                                       
STAR0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  OFFRPROC:  PROCESS THE OFFICE    RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
OFFRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SALRPROC:  PROCESS THE S/P       RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
SALRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,ASPNEND          SET A(SALESPERSON NEXT AREA)                 
         CLC   =C'PT',JSALKREP     'PT' S/P?                                    
         BNE   SALR0900            NO  - SKIP THIS RECORD                       
         L     RF,NAMCTR           INCREMENT S/P NAME COUNT                     
         LA    RF,1(RF)                                                         
         ST    RF,NAMCTR                                                        
         MVC   0(8,R3),JSALKSAL    INSERT SP/DIV-TEAM/OFFICE                    
         MVC   8(30,R3),JSALNAME   INSERT SP NAME INTO TABLE                    
         MVC   38(12,R3),JSALNMBR  INSERT TELEPHONE NUMBER                      
         CLI   QUESTOR+9,C'Y'      DISPLAY NAME TABLE ENTRY?                    
         BNE   SALR0040            NO                                           
         GOTO1 =A(DISPIPUT),DMCB,(RC),RR=Y                                      
*        MVC   P+1(05),=C'NAME:'                                                
*        MVC   P+6(30),0(R3)       INSERT TABLE ENTRY                           
*        GOTO1 REPORT                                                           
SALR0040 EQU   *                                                                
         LA    R3,50(R3)           BUMP TO NEXT SLOT                            
         ST    R3,ASPNEND          RESET NEXT SLOT                              
*                                                                               
SALR0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  GRORPROC:  PROCESS THE GROUP     RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
GRORPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  REGRPROC:  PROCESS THE REGION    RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
REGRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DIVRPROC:  PROCESS THE DIVISION  RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
DIVRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  ADVRPROC:  PROCESS THE ADVERT    RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
ADVRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XCEFL RECORD,1008         CLEAR THE RECORD BUILD AREA                  
         CLC   =C'PT',JADVKREP     'PT' ADVERTISER?                             
         BNE   ADVR0900            NO  - SKIP THIS RECORD                       
         L     RF,ADVCTR           INCREMENT ADVERTISER COUNT                   
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
         GOTO1 =A(DISPIPUT),DMCB,(RC),RR=Y                                      
         MVC   RADVLEN(2),=X'0064'                                              
         MVI   RADVKTYP,X'08'      INSERT RECORD TYPE                           
         MVC   RADVKADV(4),JADVKADV                                             
*                                  INSERT ADVERT CODE                           
         MVC   RADVKREP,PETRYREP   INSERT REP CODE                              
*                                                                               
         MVC   RAGYELEM(2),=X'0142'                                             
*                                  INSERT ELT CODE/LENGTH                       
         MVC   RADVNAME,JADVNAME   INSERT NAME                                  
*                                                                               
         LA    R1,CATTABLE                                                      
ADVR0020 EQU   *                                                                
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    ADVR0060            YES - NOT FOUND - INSERT DEFAULT             
*                                                                               
*   TEST                                                                        
*        MVC   P+1(08),=C'CAT TAB='                                             
*        MVC   P+9(6),0(R1)        INSERT CATEGORY CAT TABLE ENTRY              
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   JADVCAT,0(R1)       IN TABLE?                                    
         BE    ADVR0040            YES - USE VALUE                              
         LA    R1,LCATTABL(R1)     NO  - BUMP TO NEXT ENTRY                     
         B     ADVR0020            GO BACK FOR NEXT                             
ADVR0040 EQU   *                                                                
         MVC   RADVCATG,4(R1)      INSERT CATEGORY CODE FROM TABLE              
ADVR0060 EQU   *                                                                
         GOTO1 PUTRECS,DMCB,(RC),RECORD                                         
         GOTO1 DISPPUT,DMCB,(RC)                                                
ADVR0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CONRPROC:  PROCESS THE CONTRACT  RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
CONRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     RF,CONSREAD         INCREMENT CON COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,CONSREAD                                                      
         CLC   =C'PT',JCONKREP     'PT' CONTRACT?                               
         BNE   CONR0900            NO  - SKIP THIS RECORD                       
*                                                                               
*   TEST                                                                        
         CLC   =X'99999999',JCONKCON                                            
*        BE    TCON0001            SKIP ALL OTHERS                              
*        CLC   =X'02321131',JCONKCON                                            
*        BE    TCON0001            SKIP ALL OTHERS                              
*        CLC   =X'02250043',JCONKCON                                            
         BNE   CONR0900            SKIP ALL OTHERS                              
TCON0001 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         XCEFL RECORD,1008         CLEAR THE RECORD BUILD AREA                  
         L     RF,CONCTR           INCREMENT CONTRACT COUNTER                   
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         L     RF,CONDCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,CONDCTR          INCREMENT TOTAL RECORDS READ                 
*                                                                               
*   TEST                                                                        
*        CLC   CONDCTR,=F'50'      DISPLAY EVERY 50   RECORDS                   
*        BNE   CONR0002                                                         
*        CLC   CONCTR,=F'434860'   **TEST START CON COUNT                       
*        BL    CONR0002            NOT REACHED: DON'T DISPLAY                   
*        MVC   P+1(11),=C'TOT/CON/BUY'                                          
*        EDIT  TOTCTR,(7,P+15)     DISPLAY TOTAL COUNTER                        
*        EDIT  CONCTR,(7,P+25)     DISPLAY CONTRACT COUNTER                     
*        EDIT  BUYCTR,(7,P+35)     DISPLAY BUY      COUNTER                     
*        GOTO1 REPORT                                                           
*        XC    CONDCTR,CONDCTR                                                  
CONR0002 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(DISPIPUT),DMCB,(RC),RR=Y                                      
         MVC   RCONLEN(2),=X'005E' SET INITIAL LENGTH                           
         MVI   RCONKTYP,X'0C'      INSERT RECORD TYPE                           
         MVC   RCONKREP,PETRYREP   INSERT REP CODE                              
         MVI   ACEGRAPH,C'0'       CLEAR ACE/GRAPHNET INDICATOR                 
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,JCONKSTA,(R2),(R4),12,4,500,RR=RELO             
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    CONR0004            YES                                          
*        MVC   P+1(21),=C'STATION NOT IN TABLE:'                                
*        MVC   P+28(4),JCONKSTA                                                 
*        GOTO1 REPORT                                                           
         MVC   RCONKGRP,=C'P*'     INSERT DUMMY GROUP/SUBGROUP                  
***>>>   MVC   RCONKGRP,=C'P '     INSERT GROUP/SUBGROUP                        
         GOTO1 =A(MISTSTAT),DMCB,(RC),RR=Y                                      
         B     CONR0005                                                         
CONR0004 EQU   *                                                                
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RCONKGRP,4(R2)      INSERT GROUP/SUBGROUP                        
         MVC   ACEGRAPH,6(R2)                                                   
CONR0005 EQU   *                                                                
         MVC   RCONKSTA(4),JCONKSTA                                             
*                                  INSERT STATION                               
         MVI   RCONKSTA+4,C' '     INSERT SPACE AS MEDIA                        
****>>>  MVC   RCONKOFF,JCONKOFF   INSERT OFFICE CODE                           
*                                                                               
         GOTO1 =A(CONVAGY),DMCB,(RC),RR=Y                                       
*                                  INSERT AGENCY/AGENCY OFFICE                  
         MVC   RCONKADV,JCONKADV   INSERT ADVERTISER                            
         MVC   RCONKCON,JCONKCON   INSERT CONTRACT NUMBER                       
*                                                                               
         MVC   RCONELEM(2),=X'013C'                                             
*                                  INSERT DESCRIPT ELT CODE/LENGTH              
         MVC   RCONBUYR,SPACES                                                  
         MVI   JDSELT,X'20'        GET BUYER NAME ELEMENT                       
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   CONR0040            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
*                                                                               
*   TEST                                                                        
*        CLC   =C'##',1(RF)        BUYER CODE  KEY?                             
*        BNE   TCON0006            NO                                           
*        MVC   P+1(18),=C'BUYER   CODE USED:'                                   
*        GOTO1 REPORT                                                           
*        GOTO1 =A(DISPITST),DMCB,(RC),RR=Y                                      
*        L     RF,JDSADDR          RESET A(JDS ELT)                             
TCON0006 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,CONR0020         MOVE BUYER NAME BY LENGTH                    
         B     CONR0040                                                         
CONR0020 EQU   *                                                                
         MVC   RCONBUYR(0),1(RF)   MOVE BUYER BY LENGTH                         
CONR0040 EQU   *                                                                
         MVI   ACCTPROD,C'N'       TURN OFF 'ACC-' PRODUCT FLAG                 
         MVC   RCONPRD,SPACES      CLEAR THE PRODUCT CODE                       
*                                                                               
         MVC   ELTBILD1(2),=X'0516'                                             
*                                  BUILD PRODUCT NAME ELEMENT                   
         MVC   ELTBILD1+2(20),SPACES                                            
         MVI   JDSELT,X'30'        GET PRODUCT NAME ELEMENT                     
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   CONR0050            NOT FOUND - ADD ELEMENT ANYWAY               
         L     RF,JDSADDR          SET A(JDS ELT)                               
*                                                                               
*   TEST                                                                        
*        CLC   =C'##',1(RF)        PRODUCT REC KEY?                             
*        BNE   TCON0048            NO                                           
*        MVC   P+1(18),=C'PRODUCT CODE USED:'                                   
*        GOTO1 REPORT                                                           
*        GOTO1 =A(DISPITST),DMCB,(RC),RR=Y                                      
*        L     RF,JDSADDR          RESET A(JDS ELT)                             
TCON0048 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         CLC   =C'ACC-',1(RF)      PRODUCT CODE = 'ACC-'?                       
         BNE   CONR0045            NO                                           
         MVI   ACCTPROD,C'Y'       TURN ON 'ACC- PROD' FLAG                     
CONR0045 EQU   *                                                                
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,CONR0060         MOVE PRODUCT NAME BY LENGTH                  
         B     CONR0055                                                         
CONR0050 EQU   *                                                                
         MVI   ELTBILD1+2,C'>'     NO PRODUCT NAME INDICATOR                    
         L     RF,NOPRDCTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NOPRDCTR                                                      
CONR0055 EQU   *                                                                
         GOTO1 HELOCON1            INSERT ELEMENT, ERASE ELEMENT                
         B     CONR0080                                                         
CONR0060 EQU   *                                                                
         MVC   ELTBILD1+2(0),1(RF)                                              
CONR0080 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVI   JDSELT,X'01'        GET CONTRACT COMMENT ELEMENT                 
*        GOTO1 GETJDSEL,DMCB,(RC)                                               
*        BNZ   TCON0082            NOT FOUND - FINISHED                         
*        L     RF,JDSADDR          SET A(JDS ELT)                               
*        CLC   =C'##',2(RF)        STANDARD COMMENT?                            
*        BNE   TCON0082            NO                                           
*        MVC   P+1(18),=C'STANDARD COMMENT :'                                   
*        GOTO1 REPORT                                                           
*        GOTO1 =A(DISPITST),DMCB,(RC),RR=Y                                      
TCON0082 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(TEMCONV),DMCB,(RC),0,RR=Y                                     
*                                                                               
******************************************************************              
         GOTO1 =A(SALCONV),DMCB,(RC),RR=Y                                       
******************************************************************              
*                                                                               
         GOTO1 JDSDATE,DMCB,JCONSDTE,RCONDATE,(2,0)                             
         GOTO1 JDSDATE,DMCB,JCONEDTE,RCONDATE+3,(2,0)                           
         MVC   RCONMOD,JCONMOD     INSERT MOD #                                 
         GOTO1 JDSDATE,DMCB,JCONMODD,RCONMODD,(2,0)                             
*                                                                               
         OI    RCONMODR+1,X'10'    INDICATE 'CONVERTED' ORDER                   
         TM    JCONCNTL,X'40'      PENDING BIT ON?                              
         BO    CONR0084            YES - PENDING                                
         OI    RCONMODR,X'30'      NO  - SET BUY ADDED/NOT PENDING              
CONR0084 EQU   *                                                                
         TM    JCONCNTL,X'40'      PENDING BIT ON?                              
         BO    CONR0100            YES - PENDING                                
         GOTO1 JDSDATE,DMCB,JCONAVDT,RCONCREA,(2,0)                             
*                                  NO  - USE ORDER DATE                         
         B     CONR0120                                                         
CONR0100 EQU   *                                                                
         GOTO1 JDSDATE,DMCB,JCONCREA,RCONCREA,(2,0)                             
*                                  PENDING: USE CREATION DATE                   
CONR0120 EQU   *                                                                
         GOTO1 JDSDATE,DMCB,JCONCREA,RCONHDRD,(2,0)                             
*                                  SET HEADER CREATION DATE                     
         MVI   RCONRTGS,C'N'       SET TO 'NSI'                                 
         TM    JCONOPT,X'80'       NSI?                                         
         BO    CONR0140            YES                                          
         TM    JCONOPT,X'40'       ARB?                                         
         BNO   CONR0140            NO                                           
         MVI   RCONRTGS,C'A'       YES - SET TO 'ARB'                           
CONR0140 EQU   *                                                                
*                                                                               
*                                  TRUE ACTIVITY DATE X'08' ELEMENT             
*                                                                               
         MVI   ELTBILD3,8          BUILD TRUE ACTIVITY DATE ELT                 
         MVI   ELTBILD3+1,12       INSERT LENGTH                                
         LA    R4,ELTBILD3                                                      
         USING RCONACEL,R4                                                      
         MVC   RCONACTA,RCONCREA   INSERT CREATE DATE AS TRUE ACT DATE          
*                                                                               
         GOTO1 HELOCON3                                                         
*                                  INSERT X'08' ELT INTO RECORD                 
         DROP  R4                                                               
*                                                                               
**       MVC   RCONINVD,???        LAST INVOICE CHANGE DATE                     
         MVC   RCONWKS,JCONWKS     INSERT NUMBER OF WEEKS                       
*                                                                               
*   DESCRIPTOR ELEMENT SHOULD BE DIRECTLY ACCESSIBLE.                           
*                                                                               
*   SPECIAL CONTRACT TYPE SETTING:  IF OFFICE = UW, AND SP = NHUW,              
*        OUTPUT CONTRACT TYPE SHOULD BE 'N' (NETWORK)                           
*   REQUIRES CONFIRMATION!!                                                     
*                                                                               
         CLC   =C'UW',JCONKOFF     'UNWIRED' OFFICE?                            
         BNE   CONR0142            NO                                           
         CLC   =C'NHUW',JCONKSAL   YES - 'NETWK' S/P?                           
         BNE   CONR0142            NO                                           
         MVI   RCONTYPE,C'N'       YES - SET CON TYPE TO 'N' NETWORK            
         B     CONR0200                                                         
*                                                                               
CONR0142 EQU   *                                                                
         CLI   JCONTYPE,C'+'       'HISTORICAL' CONTRACT TYPE?                  
         BNE   CONR0144            NO                                           
         MVI   RCONTYPE,C'H'       YES - SET TO HISTORICAL                      
         B     CONR0200                                                         
CONR0144 EQU   *                                                                
         CLC   =C'DT',JCONKOFF                                                  
         BNE   CONR0146                                                         
         CLC   =C'RSDT',JCONKSAL                                                
         BNE   CONR0146                                                         
         MVI   RCONTYPE,C'R'       SET 'DIRECT RESPONSE'                        
         B     CONR0200                                                         
CONR0146 EQU   *                                                                
         CLC   =C'DN',JCONKOFF                                                  
         BNE   CONR0148                                                         
         CLC   =C'ELDN',JCONKSAL                                                
         BNE   CONR0148                                                         
         MVI   RCONTYPE,C'R'       SET 'DIRECT RESPONSE'                        
         B     CONR0200                                                         
CONR0148 EQU   *                                                                
         MVI   JDSELT,X'05'        GET CATEGORY CODE ELT                        
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   CONR0150            NOT FOUND                                    
         L     R2,JDSADDR          SET A(JDS ELT)                               
         CLC   =C'DR01',2(R2)      DIRECT RESPONSE?                             
         BNE   CONR0150            NO                                           
         MVI   RCONTYPE,C'R'       YES - SET CON TYPE DR                        
         B     CONR0200                                                         
CONR0150 EQU   *                                                                
         CLC   =C'PP01',2(R2)      PAID PROGRAMMING?                            
         BNE   CONR0151            NO                                           
         MVI   RCONTYPE,C'P'       YES - SET CON TYPE PP                        
         B     CONR0200                                                         
*                                                                               
CONR0151 EQU   *                                                                
         CLC   =C'TD01',2(R2)      TRADE?                                       
         BNE   CONR0152            NO                                           
         MVI   RCONTYPE,C'T'       YES - SET CON TYPE TRADE                     
         B     CONR0200                                                         
*                                                                               
CONR0152 EQU   *                                                                
         MVI   RCONTYPE,C'S'       SET DEFAULT                                  
CONR0200 EQU   *                                                                
         MVI   JDSELT,X'05'        GET CATEGORY CODE ELT                        
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   CONR0250            NOT FOUND                                    
         L     R2,JDSADDR          SET A(JDS ELT)                               
         MVC   ELTCATEG+2(4),2(R2)                                              
*                                  INSERT CATEGORY CODE INTO ELEMENT            
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTCATEG,0             
*                                  ADD ELEMENT TO RECORD                        
         B     CONR0210                                                         
ELTCATEG DC    X'760600000000'                                                  
*                                                                               
CONR0210 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(08),=C'JDS CAT='                                             
*        MVC   P+9(4),2(R2)        INSERT CATEGORY JDS                          
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         LA    R1,CATTABLE                                                      
CONR0220 EQU   *                                                                
         CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    CONR0250            YES - NOT FOUND - INSERT DEFAULT             
*                                                                               
*   TEST                                                                        
*        MVC   P+1(08),=C'CAT TAB='                                             
*        MVC   P+9(6),0(R1)        INSERT CATEGORY CAT TABLE ENTRY              
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   2(4,R2),0(R1)       IN TABLE?                                    
         BE    CONR0240            YES - USE VALUE                              
         LA    R1,LCATTABL(R1)     NO  - BUMP TO NEXT ENTRY                     
         B     CONR0220            GO BACK FOR NEXT                             
CONR0240 EQU   *                                                                
         MVC   RCONCTGY,4(R1)      INSERT CATEGORY CODE FROM TABLE              
         B     CONR0260                                                         
*                                                                               
*   CATTABLE:  SIX-BYTE ENTRIES:                                                
*        1 - 4 = PETRY CODE                                                     
*        5 - 6 = DDS EQUIVALENT                                                 
*                                                                               
CATTABLE DS    0CL6                                                             
         DC    C'AB0149'                                                        
LCATTABL EQU   *-CATTABLE                                                       
         DC    C'AC01O1'                                                        
         DC    C'AG0159'                                                        
         DC    C'AL0154'                                                        
         DC    C'AP01R5'                                                        
         DC    C'AU01A1'                                                        
         DC    C'BW01B1'                                                        
         DC    C'CO01C1'                                                        
         DC    C'DR01D1'                                                        
         DC    C'FB0158'                                                        
         DC    C'FF01F1'                                                        
         DC    C'GO01O1'                                                        
         DC    C'KD01L1'                                                        
         DC    C'ME0180'                                                        
         DC    C'MV01K1'                                                        
         DC    C'OL0147'                                                        
         DC    C'PG01PG'                                                        
         DC    C'PO0190'                                                        
         DC    C'PP0166'                                                        
         DC    C'PT01PT'                                                        
         DC    C'RE0141'                                                        
         DC    C'RT01R3'                                                        
         DC    C'SD0130'                                                        
         DC    C'TC01P1'                                                        
         DC    C'TD01TR'                                                        
         DC    C'TT0157'                                                        
         DC    6X'FF'                                                           
CONR0250 EQU   *                                                                
         MVC   RCONCTGY,=C'XX'     INSERT CATEGORY CODE DEFAULT                 
         L     RF,ACATAREA         INSERT INTO 'MISSING CAT' TABLE              
CONR0255 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    CONR0258            YES - ADD AT END                             
         CLC   2(4,R2),0(RF)       ENTRY ALREADY IN TABLE?                      
         BE    CONR0260            YES - DON'T INSERT AGAIN                     
         LA    RF,4(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     CONR0255            GO BACK AND CHECK NEXT                       
CONR0258 EQU   *                                                                
         MVC   0(4,RF),2(R2)       INSERT CATEGORY INTO TABLE                   
CONR0260 EQU   *                                                                
         LA    R0,2                SET LOOP CONTROL: 2 MAX                      
         MVI   JDSELT,X'01'        GET CONTRACT COMMENT ELT                     
         GOTO1 GETJDSEL,DMCB,(RC)                                               
CONR0280 EQU   *                                                                
         BNZ   CONR0340            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         CLC   =C'A=',2(RF)        AVAIL COMMENT?                               
         BE    CONR0310            YES - SKIP IT                                
         CLC   =C'##',2(RF)        STANDARD COMMENT FROM JDS?                   
         BNE   CONR0290            NO  -                                        
         MVC   ELTBILD1(20),SPACES CLEAR TO SPACES                              
CONR0290 EQU   *                                                                
         CLC   JDSLEN,=F'60'       COMMENT EXCEEDS 60 CHARS?                    
         BNH   CONR0300            NO                                           
         LA    RE,60               YES - SET MAX TO 60                          
         ST    RE,JDSLEN           YES                                          
CONR0300 EQU   *                                                                
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,CONR0320         MOVE PRODUCT NAME BY LENGTH                  
         MVI   ELTBILD1,X'02'      INSERT CONTRACT COMMENT ELT CODE             
         LA    RE,3(RE)            CALCULATE L(ELEMENT)                         
         STC   RE,ELTBILD1+1       INSERT INTO ELEMENT                          
         CLC   =C'##',ELTBILD1+2   JDS STANDARD COMMENT?                        
         BNE   CONR0305            NO                                           
         MVI   ELTBILD1+1,12       YES - CHANGE LENGTH TO 12                    
         MVC   ELTBILD1+2(2),=C'C='                                             
         L     R1,STDCMTCT                                                      
         LA    R1,1(R1)                                                         
         ST    R1,STDCMTCT                                                      
         CLC   STDCMTCT,=F'25'                                                  
         BH    CONR0305                                                         
**       MVC   P+1(10),=C'STD COMMT:'                                           
**       MVC   P+12(12),2(RF)      INSERT JDS SPECIAL COMMENT                   
**       MVC   P+26(12),ELTBILD1   INSERT DDS SPECIAL COMMENT                   
**       GOTO1 REPORT                                                           
CONR0305 EQU   *                                                                
         GOTO1 HELOCON1            INSERT ELEMENT, ERASE ELEMENT                
CONR0310 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)  GET NEXT ELEMENT                             
         BCT   R0,CONR0280         GO BACK AND CHECK IF FOUND                   
         B     CONR0340                                                         
CONR0320 EQU   *                                                                
         MVC   ELTBILD1+2(0),2(RF) INSERT COMMENT BY LENGTH                     
CONR0340 EQU   *                                                                
*                                                                               
*   BUDGET ORDERS WILL BE GENERATED, BUT $$ BUCKETS WILL BE SKIPPED.            
*        THIS WILL PREVENT THEM FROM INFLUENCING REPORTING.                     
*                                                                               
         CLC   RCONKAGY(2),=C'$$'  BUDGET ORDER?                                
         BNE   CONR0350            NO                                           
         CLC   RCONKADV(2),=C'$$'  DITTO                                        
         BNE   CONR0350            NO                                           
         CLC   RCONSAL(2),=C'$$'   DITTO                                        
         BE    CONR0460            YES - DON'T PUT OUT ANY $$ BUCKETS           
CONR0350 EQU   *                                                                
         MVI   JDSELT,X'B0'        GET ESTIMATE $$ ELEMENT                      
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     CONR0380                                                         
CONR0360 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
CONR0380 EQU   *                                                                
         BNZ   CONR0400            NOT FOUND                                    
         GOTO1 DOLBUCK,DMCB,(RC),(3,0)                                          
         B     CONR0360            GO BACK FOR NEXT ELEMENT                     
CONR0400 EQU   *                                                                
         MVI   JDSELT,X'C0'        GET INVOICE  ELEMENT                         
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     CONR0440                                                         
CONR0420 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
CONR0440 EQU   *                                                                
         BNZ   CONR0460            NOT FOUND                                    
         GOTO1 DOLBUCK,DMCB,(RC),(4,0)                                          
         B     CONR0420            GO BACK FOR NEXT ELEMENT                     
CONR0460 EQU   *                                                                
         GOTO1 =A(COMPELEM),DMCB,(RC),RR=Y                                      
*                                  GENERATE THE COMPETITIVE ELEMENT             
         MVI   ELTBILD1,9          BUILD EXTRA SPL ELEMENT                      
         MVI   ELTBILD1+1,11       SET ELEMENT LENGTH                           
         MVI   ELTBILD1+10,X'60'                                                
*                                  SET $0 FCST $0 BUDGET ENTERED                
         GOTO1 HELOCON1            INSERT ELEMENT                               
*                                                                               
         GOTO1 BUILDSAR,DMCB,(RC)                                               
*                                  BUILD/INSERT THE SAR ELEMENT                 
         CLI   SPLFLAG,C'Y'        SPL ELEMENT ADDED?                           
         BE    CONR0480            YES - NO 'ACTIVITY CMNTS' FOR SAR            
*                                                                               
         GOTO1 =A(REPCOMMT),DMCB,(RC),RR=Y                                      
*                                  PROCESS OLD X'02' ELEMENTS                   
         CLI   ACTCFLAG,C'Y'       X'02' ELEMENTS FOUND?                        
         BE    CONR0480            YES - DON'T LOOK FOR 66 RECORD               
         CLI   COMMSKIP,C'Y'       SKIP COMMENT LOOKUP?                         
         BE    CONR0480            YES                                          
*                                                                               
         GOTO1 =A(SARCOMMS),DMCB,(RC),RR=Y                                      
*                                  GENERATE THE SAR COMMENT ELEMENTS            
CONR0480 EQU   *                                                                
         CLI   COMMSKIP,C'Y'       SKIP COMMENT LOOKUP?                         
         BE    CONR0500            YES                                          
*                                                                               
         GOTO1 =A(ORDRECD),DMCB,(RC),RR=Y                                       
*                                  BUILD/INSERT ORDER COMMENTS                  
CONR0500 EQU   *                                                                
         GOTO1 =A(EXCONELT),DMCB,(RC),RR=Y                                      
*                                  BUILD/INSERT 1F/20 ELEMENTS                  
         GOTO1 EST#ELT,DMCB,(RC)                                                
*                                  BUILD/INSERT A2 ELEMENT                      
         GOTO1 PUTRECS,DMCB,(RC),RECORD                                         
         GOTO1 DISPPUT,DMCB,(RC)                                                
*                                                                               
*    TEST                                                                       
*        CLI   SHRGOAL,C'Y'        SHARE GOAL ORDER?                            
*        BNE   TEST1000            NO  -                                        
*        MVC   P+1(8),=C'SHRGOAL ORDER'                                         
*        GOTO1 HEXOUT,DMCB,RCONKCON,P+12,4,=C'TOG'                              
*        GOTO1 REPORT                                                           
*        LA    R6,RCONREC                                                       
*        MVI   ELCODE,X'12'        FIND SAR ELEMENT                             
*        BAS   RE,GETEL                                                         
*        BNE   TEST1000            NO X'08'                                     
*        LA    RF,128              SET LENGTH                                   
*        GOTO1 =V(PRNTBL),DMCB,(0,(R6)),(R6),C'DUMP',(RF),=C'1D'                
*        GOTO1 REPORT              INSERT A BLANK LINE                          
TEST1000 EQU   *                                                                
*    TEST END                                                                   
*                                                                               
CONR0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DOLBUCK:  CONVERT EST/INV ELEMENTS TO CORRESPONDING.                        
*             P2  =  ELEMENT TYPE FOR OUTPUT                                    
*                                                                               
DOLBUCK  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   ELTBILD1(1),4(R1)   INSERT ELEMENT TYPE                          
         MVI   ELTBILD1+1,10       INSERT ELEMENT LENGTH                        
         L     R3,JDSADDR          SET A(ELEMENT BEING CONVERTED)               
         USING JCONESEL,R3                                                      
*                                                                               
         GOTO1 JDSDATE,DMCB,JCONESYM,ELTBILD1+2,(1,0)                           
         GOTO1 JDSDATE,DMCB,JCONESAC,WORK+20,(2,0)                              
         GOTO1 DATCON,DMCB,(3,WORK+20),(2,ELTBILD1+4)                           
*                                  CONVERT ACT DATE TO COMPRESSED               
         CLI   JDSLEN+3,5          JDS ELT HAVE PENNIES?                        
         BNE   DOLB0060            YES                                          
         ZICM  RF,JCONESAM,2       NO  - NEED TO DECIMAL ALIGN                  
         SLL   RF,16               SHIFT LOGICAL TO HIGH BYTES                  
         SRA   RF,16               SHIFT BACK, PROPAGATE SIGN                   
*                                     FOR NEGATIVE NUMBERS                      
         SR    RE,RE                                                            
         M     RE,=F'100'          INSERT PENNIES                               
         STCM  RF,15,ELTBILD1+6    INSERT VALUE INTO ELEMENT                    
         B     DOLB0120                                                         
DOLB0060 EQU   *                                                                
         MVC   ELTBILD1+6(4),JCONESAM INSERT JDS $$ WITH PENNIES                
*                                  INSERT JDS $$.00                             
DOLB0120 EQU   *                                                                
         GOTO1 HELOCON1            INSERT ELEMENT W/ERASE                       
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  BUILDSAR:  BUILD THE X'12' ELEMENT FROM JDS INFORMATION.      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
BUILDSAR NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,ELTBILD1                                                      
         USING RSARXEL,R6                                                       
*                                                                               
         LA    R7,RSARXDEM         SET A(1ST DEMO)                              
         MVC   RSARXEL(2),=X'1280' SET CODE/LENGTH OF ELEMENT                   
         MVC   RSARXEDT,RCONCREA   INSERT ORDER CREATE DATE AS                  
*                                     ENTRY DATE:  NO JDS EQUIV                 
         MVI   SAVETRGT,C' '       CLEAR SAVED TARGET DEMO                      
         MVI   JDSELT,X'90'        GET TARGET DEMO ELEMENT                      
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0010            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         MVC   SAVETRGT,1(R3)      SAVE TARGET DEMO                             
BSAR0010 EQU   *                                                                
*                                                                               
*   NOW DO REST OF DEMOS                                                        
*                                                                               
         MVI   JDSELT,X'80'        GET DEMO ELEMENT                             
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0040            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,1(R3)            SET TO 1ST DEMO                              
*                                                                               
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
*                                                                               
BSAR0020 EQU   *                                                                
         CLI   0(R3),0             ANYTHING IN BYTE?                            
         BE    BSAR0060            NO  - FINISHED                               
*                                     JDS INSERTS A FILL BYTE TO                
*                                        PRODUCE WHOLE-WORD ELEMENTS            
         MVC   FULL(1),0(R3)       NO  - GET DEMO CODE                          
         LA    R2,CJDSDEMO         SET A(DEMO TABLE)                            
         LA    R4,DMOCOUNT         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,FULL,(R2),(R4),3,1,(R4),RR=RELO                 
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   BSAR0024            NO  - NO DEMO NOT FOUND                      
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
         MVC   2(1,R7),1(R2)       INSERT DEMO CODE OF CONVERSION               
         MVC   1(1,R7),2(R2)       INSERT QUALIFIER OF DEMO                     
         CLC   SAVETRGT,FULL       IS DEMO THE TARGET DEMO?                     
         BNE   BSAR0022            NO                                           
         OI    0(R7),X'40'         YES - SET TARGET INDICATOR                   
BSAR0022 EQU   *                                                                
         LA    R3,1(R3)            BUMP TO NEXT JDS DEMO                        
         LA    R7,3(R7)            BUMP TO NEXT DDS DEMO                        
         BCT   R0,BSAR0020         GO BACK FOR NEXT                             
         B     BSAR0060            FINISHED WITH DEMOS                          
BSAR0024 EQU   *                                                                
         MVI   2(R7),1             INSERT DEMO CODE OF HOMES!!                  
         MVI   1(R7),C' '          INSERT QUALIFIER                             
         LA    R3,1(R3)            BUMP TO NEXT JDS DEMO                        
         LA    R7,3(R7)            BUMP TO NEXT DDS DEMO                        
         BCT   R0,BSAR0020         GO BACK FOR NEXT                             
         B     BSAR0060                                                         
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE REJDSDMO                                                       
         EJECT                                                                  
BSAR0040 EQU   *                   NO DEMO CODE ELT:  DEFAULT TO HOMES          
        MVI   RSARXDEM+2,1         INSERT DEMO CODE OF HOMES!!                  
        MVI   RSARXDEM+1,C' '      INSERT QUALIFIER                             
BSAR0060 EQU   *                                                                
         CLI   SAVETRGT,C' '       ANY TARGET DEMO SPECIFIED?                   
         BNE   BSAR0062            YES                                          
         OI    RSARXDEM,X'40'      NO  - SET 1ST DEMO AS TARGET                 
BSAR0062 EQU   *                                                                
         MVC   RSARXSRC,RCONRTGS   INSERT RATING SERVICE                        
*                                                                               
         MVC   SAR0BELT+2(30),SPACES                                            
*                                  CLEAR BOOK RENAME ELEMENT                    
         XC    BKRENCTR,BKRENCTR   CLEAR BOOK RENAME COUNTER                    
         MVI   JDSELT,X'70'        GET BOOK ELEMENT                             
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0140            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,1(R3)            SET TO 1ST BOOK                              
         CLI   JDSLEN+3,6          SIX BOOKS = MAX                              
         BNH   BSAR0080                                                         
         MVI   JDSLEN+3,6          FORCE MAX OF SIX                             
BSAR0080 EQU   *                                                                
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
         LA    R7,RSARXBKS         SET A(SAR ELT BOOKS)                         
BSAR0100 EQU   *                                                                
         MVC   WORK+12(1),0(R3)    UNLOAD DATE                                  
         GOTO1 JDSDATE,DMCB,WORK+12,WORK,(1,0)                                  
*                                  CONVERT JDS 1-BYTE TO DDS 2-BYTE             
         MVC   1(2,R7),WORK        INSERT BOOK DATE INTO ELT                    
         L     RF,BKRENCTR         INCREMENT BOOK COUNTER                       
         LA    RF,1(RF)                                                         
         ST    RF,BKRENCTR                                                      
         ZIC   RF,0(R3)            TEST FOR PROJ/EST BOOKS                      
         SRL   RF,4                DROP LOW-ORDER NYBBLE                        
         CH    RF,=H'14'           PROJECTION/ESTIMATE BOOK?                    
         BL    BSAR0115            NO                                           
         LA    R2,SAR0BELT+2       YES - SET A(SAR 0B ELT)                      
         L     RE,BKRENCTR         CALCULATE DISPLACEMENT FOR BOOK              
         BCTR  RE,0                MAKE ZERO RELATIVE                           
         MH    RE,=H'5'            MULT BY LENGTH OF ENTRY                      
         AR    R2,RE               SET DISPLACEMENT                             
         CH    RF,=H'15'           PROJECTION BOOK?                             
         BNE   BSAR0105            NO                                           
         MVC   3(2,R2),=C'PJ'      INSERT PJ BOOK RENAME                        
         B     BSAR0110                                                         
BSAR0105 EQU   *                                                                
         MVC   3(2,R2),=C'ES'      INSERT ES BOOK RENAME                        
BSAR0110 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,WORK),(6,WORK+3)                                  
*                                  CONVERT DATE TO MMM/YY                       
         MVC   0(3,R2),WORK+3      MOVE MMM TO 0B ELT                           
BSAR0115 EQU   *                                                                
         CLI   RCONRTGS,C'N'       SERVICE = NIELSEN?                           
         BNE   BSAR0120            NO                                           
         OI    0(R7),X'40'         YES - TURN ON SERVICE BIT FOR NSI            
BSAR0120 EQU   *                                                                
         LA    R7,3(R7)            A(NEXT DDS BOOK FIELD)                       
         LA    R3,1(R3)            A(NEXT JDS BOOK FIELD)                       
         BCT   R0,BSAR0100         GO BACK FOR NEXT                             
         B     BSAR0140                                                         
SAR0BELT DC    X'0B00'             ELEMENT CODE/LENGTH                          
         DC    30X'00'                                                          
BSAR0140 EQU   *                                                                
         CLC   SAR0BELT+2(30),SPACES                                            
*                                  ANYTHING IN RENAME ELT?                      
         BE    BSAR0150            NO                                           
         L     RF,BKRENCTR         YES - GENERATE A RENAME ELEMENT              
         MH    RF,=H'5'            MULTIPLY BY LENGTH OF ELEMENTS               
         LA    RF,2(RF)            ADD CONTROL LENGTH                           
         STC   RF,SAR0BELT+1       INSERT INTO ELEMENT                          
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,SAR0BELT,0             
*                                  INSERT INTO RECORD                           
BSAR0150 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(08),=C'SAR ELT:'                                             
*        MVC   P+9(64),RSARXEL                                                  
*        GOTO1 REPORT                                                           
*        MVC   P+9(64),RSARXEL+64                                               
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVI   JDSELT,X'50'        GET DAYPART ELEMENT                          
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0200            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,1(R3)            SET TO 1ST DYPT                              
         CLI   JDSLEN+3,6          SIX DYPTS = MAX                              
         BNH   BSAR0160                                                         
         MVI   JDSLEN+3,6          FORCE MAX OF SIX                             
BSAR0160 EQU   *                                                                
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
         LA    R7,RSARXDPT         SET A(SAR ELT DYPTS)                         
BSAR0180 EQU   *                                                                
         MVC   0(1,R7),0(R3)       INSERT DYPT INTO ELT                         
         CLI   0(R7),C'U'          DAYPART SWITCH? U -> R                       
         BNE   BSAR0181            NO                                           
         MVI   0(R7),C'R'          YES - SWITCH IT                              
         B     BSAR0199                                                         
BSAR0181 EQU   *                                                                
         CLI   0(R7),C'V'          DAYPART SWITCH? V -> T                       
         BNE   BSAR0182            NO                                           
         MVI   0(R7),C'T'          YES - SWITCH IT                              
         B     BSAR0199                                                         
BSAR0182 EQU   *                                                                
         CLI   0(R7),C'X'          DAYPART SWITCH? X -> S                       
         BNE   BSAR0183            NO                                           
         MVI   0(R7),C'S'          YES - SWITCH IT                              
         B     BSAR0199                                                         
BSAR0183 EQU   *                                                                
         CLI   0(R7),C'S'          DAYPART SWITCH? S -> J                       
         BNE   BSAR0184            NO                                           
         MVI   0(R7),C'J'          YES - SWITCH IT                              
         B     BSAR0199                                                         
BSAR0184 EQU   *                                                                
         CLI   0(R7),C'Q'          DAYPART SWITCH? Q -> G                       
         BNE   BSAR0185            NO                                           
         MVI   0(R7),C'G'          YES - SWITCH IT                              
         B     BSAR0199                                                         
BSAR0185 EQU   *                                                                
         CLI   0(R7),C'R'          DAYPART SWITCH? R -> Q                       
         BNE   BSAR0186            NO                                           
         MVI   0(R7),C'R'          YES - SWITCH IT                              
         B     BSAR0199                                                         
BSAR0186 EQU   *                                                                
BSAR0199 EQU   *                                                                
         LA    R7,3(R7)            A(NEXT DDS DYPT FIELD)                       
         LA    R3,1(R3)            A(NEXT JDS DYPT FIELD)                       
         BCT   R0,BSAR0180         GO BACK FOR NEXT                             
BSAR0200 EQU   *                                                                
         MVI   JDSELT,X'40'        GET BUDGET  ELEMENT                          
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0260            NOT FOUND                                    
         GOTO1 GETJDSNX,DMCB,(RC)                                               
         BNZ   BSAR0260            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         MVC   RSARXBGT,2(R3)      INSERT BUDGET                                
BSAR0260 EQU   *                                                                
*                                                                               
         MVI   JDSELT,X'60'        GET LENGTH  ELEMENT                          
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0320            NOT FOUND                                    
         GOTO1 =A(CALCLENS),DMCB,(RC),RSARXRFR,RR=Y                             
BSAR0320 EQU   *                                                                
         MVI   RSARXFLG,X'E0'      SET BDGT $ = MKT $                           
*                                      $0 BDGT ENTERED                          
*                                      $0 SHARE ENTERED                         
*                                                                               
         MVI   SHRGOAL,C'N'                                                     
         MVI   JDSELT,X'08'        GET SHARE GOAL ELEMENT                       
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0340            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         CLI   1(R3),X'02'         SHARE GOAL ELEMENT LENGTH?                   
         BNE   BSAR0340            NO  - DON'T MOVE SHARE GOAL                  
         MVC   RSARXSHG,2(R3)      INSERT SHARE GOAL FROM '08' ELT              
         MVI   SHRGOAL,C'Y'                                                     
*                                                                               
         DROP  R6                                                               
*                                                                               
BSAR0340 EQU   *                                                                
*                                                                               
         GOTO1 HELOCON1            INSERT ELEMENT W/ERASE                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   EST#ELT:  RETRIEVE THE ESTIMATE NUMBER ELEMENT, AND BUILD                   
*        X'A2' ELEMENT TO INSERT IT                                             
*                                                                               
EST#ELT  NTR1                                                                   
         XC    ELTBILD1,ELTBILD1                                                
         MVC   ELTBILD1(2),=X'A220'                                             
         MVI   JDSELT,X'04'        GET ACTIVITY COMMENT ELEMENT                 
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   EST#0200            NOT FOUND:  EXIT                             
         L     RF,JDSADDR          SET A(JDS ELT)                               
         CLI   1(RF),2             OLD FORMAT EST #?  (LEN=2WORDS)              
         BNE   EST#0020            NO  - NEW FORMAT                             
         ZICM  R6,2(RF),2          YES - EDIT OLD FORMAT                        
         EDIT  (R6),(4,ELTBILD1+10),FILL=0                                      
         B     EST#0040                                                         
EST#0020 EQU   *                                                                
         MVC   ELTBILD1+10(4),2(RF)                                             
*                                  INSERT NEW FORMAT: 4-BYTE ALPHA              
EST#0040 EQU   *                                                                
         GOTO1 HELOCON1            INSERT ELEMENT W/ERASE                       
EST#0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  BUYRPROC:  PROCESS THE BUY       RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
BUYRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     RF,BUYSREAD         INCREMENT BUY COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYSREAD                                                      
         CLC   =C'PT',JBUYKREP     'PT' BUYLINE?                                
         BNE   BUYR0680            NO  - SKIP THIS RECORD                       
*                                                                               
*   DROP 'CANCELLED JDS BUYS' - MAY NOT BE FINAL DECISION!!                     
*                                                                               
         CLI   JBUYCHGC,C'C'       CANCELLED?                                   
         BE    BUYR0680            YES - SKIP IT                                
         CLI   JBUYCHGC+1,C'C'     CANCELLED?                                   
         BE    BUYR0680            YES - SKIP IT                                
         TM    JBUYCNTL,X'80'      DELETE SET?                                  
         BO    BUYR0680            YES - SKIP IT                                
*                                                                               
*   FOLLOWING CODE IS NOT - REPEAT, NOT - DELETED:  IT IS A COMBO               
*        OF DAY CHANGE AND EFFECTIVE DATE CHANGE.  CODE HAS BEEN                
*        LEFT HERE FOR CLARIFICATION OF THE PROBLEM ENCOUNTERED                 
*        DURING THE ORIGINAL PETRY CONVERSION.                                  
*                                                                               
****     CLC   JBUYCHGC(2),=C'DE'  DELETED?                                     
****     BE    BUYR0680            YES - SKIP IT                                
*                                                                               
*   TEST                                                                        
         CLC   JBUYKCON,=X'97686243'                                            
*        BE    TBUY0100                                                         
*        CLC   JBUYKCON,=X'97678868'                                            
         BNE   BUYR0680                                                         
TBUY0100 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         XCEFL RECORD,1008         CLEAR THE RECORD BUILD AREA                  
         L     RF,BUYCTR           INCREMENT BUY COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
*                                                                               
*   TEST                                                                        
*        CLC   BUYDCTR,=F'50'      DISPLAY EVERY 50   RECORDS                   
*        BNE   BUYR0020                                                         
*        CLC   BUYCTR,=F'447703'   **TEST START CON COUNT                       
*        BL    BUYR0020            NOT REACHED: DON'T DISPLAY                   
*        MVC   P+1(11),=C'TOT/CON/BUY'                                          
*        EDIT  TOTCTR,(7,P+15)     DISPLAY TOTAL COUNTER                        
*        EDIT  CONCTR,(7,P+25)     DISPLAY CONTRACT COUNTER                     
*        EDIT  BUYCTR,(7,P+35)     DISPLAY BUY      COUNTER                     
*        GOTO1 REPORT                                                           
*        XC    BUYDCTR,BUYDCTR                                                  
BUYR0020 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 =A(DISPIPUT),DMCB,(RC),RR=Y                                      
*                                                                               
*   TEST                                                                        
*        GOTO1 =A(DISPIBUY),DMCB,(RC),RR=Y                                      
*   TEST END                                                                    
*                                                                               
         MVC   RBUYLEN(2),=X'004D' SET INITIAL LENGTH                           
         MVI   RBUYKTYP,X'0B'      INSERT RECORD TYPE                           
         MVC   RBUYKREP,PETRYREP   INSERT REP CODE                              
         MVC   RBUYKPLN,=X'FFFFFF' INSERT PLAN                                  
         MVI   JDSELT,X'60'        GET PLAN ELEMENT                             
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0000                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0000 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0040            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         MVC   RBUYKPLN,1(RF)      INSERT PLAN FROM PLAN ELEMENT                
BUYR0040 EQU   *                                                                
         MVC   RBUYKMLN,JBUYKMLI   INSERT MASTER LINE NUMBER                    
         MVC   RBUYKLIN,JBUYKLIN   INSERT DETAIL LINE NUMBER                    
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),JBUYKCON                                              
*                                  RETRIEVE JDS CONTRACT NUMBER                 
*        ZAP   WORK+5(5),=P'99999999'                                           
*        SP    WORK+5(5),WORK+15(5)                                             
*        MVO   WORK+15(5),WORK+5(5)                                             
*                                                                               
*   ASSUME JDS # IS ALREADY COMPLIMENTED                                        
*                                                                               
*                                  REVERSE THE COMPLIMENT                       
*        PACK  RBUYKCON+0(1),WORK+18(1)                                         
*        PACK  RBUYKCON+1(1),WORK+17(1)                                         
*        PACK  RBUYKCON+2(1),WORK+16(1)                                         
*        PACK  RBUYKCON+3(1),WORK+15(1)                                         
*                                                                               
         PACK  RBUYKCON+0(1),JBUYKCON+3(1)                                      
         PACK  RBUYKCON+1(1),JBUYKCON+2(1)                                      
         PACK  RBUYKCON+2(1),JBUYKCON+1(1)                                      
         PACK  RBUYKCON+3(1),JBUYKCON+0(1)                                      
*                                                                               
*   TEST                                                                        
*        CLC   BUYCTR,LOWCTR       CHECK RANGE OF RECORDS                       
*        BL    BTST0029                                                         
*        CLC   BUYCTR,HIGHCTR                                                   
*        BH    BTST0029                                                         
*        MVC   P+1(11),=C'DDS/JDS #S:'                                          
*        GOTO1 HEXOUT,DMCB,RBUYKCON,P+14,4,=C'TOG'                              
*        GOTO1 HEXOUT,DMCB,JBUYKCON,P+24,4,=C'TOG'                              
*        GOTO1 REPORT                                                           
BTST0029 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
*                                                                               
*   WITH CON # COMPLIMENTED/REVERSED, CHECK BACKUP AREA TO SEE IF               
*        THERE IS A BUYREC WHICH HAS TO BE PUT OUT.  THIS ORDER WAS             
*        RETAINED TO PERMIT INSERTION OF CROSS-REFERENCE MG ELEMENTS,           
*        IF NECESSARY.                                                          
*                                                                               
         CLI   BUYREC4,C'Y'        BUY RECORD IN REC4?                          
         BNE   BUYR0100            NO                                           
         L     R6,ARECORD4         SET A(RECORD4)                               
*                                                                               
*   TEST                                                                        
*        CLC   BUYCTR,LOWCTR       CHECK RANGE OF RECORDS                       
*        BL    BUYR0060                                                         
*        CLC   BUYCTR,HIGHCTR                                                   
*        BH    BUYR0060                                                         
*        MVC   P+1(4),RBUYKCON-RBUYKEY(R6)                                      
*        MVC   P+10(4),RBUYKCON                                                 
*        GOTO1 REPORT                                                           
BUYR0060 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         CLC   RBUYKCON-RBUYKEY(4,R6),RBUYKCON                                  
*                                  SAME CONTRACT NUMBER?                        
         BNE   BUYR0080            NO                                           
         CLC   RBUYKMLN,RBUYKLIN   YES - NEW LINE A MG?                         
         BNE   BUYR0100            YES - LEAVE RECORD IN ALT AREA               
BUYR0080 EQU   *                                                                
*                                  WRITE OUT RECORD IN ALT AREA                 
         MVI   BUYREC4,C'N'        SET 'NO RECORD IN REC4'                      
         GOTO1 PUTRECS,DMCB,(RC),(R6)                                           
*                                  NO  - PUT RECORD4 TO OUTPUT                  
         GOTO1 DISPPUTB,DMCB,(RC)                                               
         PRINT GEN                                                              
         L     RE,ARECORD4         SET A(RECORD4)                               
         XCEFL (RE),1024           CLEAR THE RECORD AREA                        
         PRINT NOGEN                                                            
BUYR0100 EQU   *                                                                
         MVC   RBUYELEM(2),=X'012B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         MVC   RBUYNW,JBUYNPW      INSERT NUMBER PER WEEK                       
         MVC   RBUYCOS,JBUYRATE    INSERT BUYLINE COST                          
*                                                                               
         MVI   JDSELT,X'40'        GET CLASS ELEMENT                            
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0001                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0001 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0120            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         MVC   RBUYCLS,1(RF)       INSERT CLASS FROM ELEMENT                    
BUYR0120 EQU   *                                                                
*                                                                               
         MVI   JDSELT,X'50'        GET SECTN ELEMENT                            
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0002                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0002 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0140            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         MVC   RBUYSEC,1(RF)       INSERT SECTN FROM ELEMENT                    
BUYR0140 EQU   *                                                                
         MVC   RBUYCREA,RCONCREA   INSERT CONTRACT CREATION DATE                
*                                     AS BUY CREATION DATE: NO JDS              
         GOTO1 JDSDATE,DMCB,JBUYACTD,RBUYCHGD,(2,0)                             
         MVC   RBUYKMOD,JBUYMODN   INSERT MOD #                                 
         MVC   RBUYCHGI,JBUYCHGC   INSERT CHANGE CODE(S)                        
*                                                                               
*  NEED TO CALCULATE RBUYTSPT, RBUYTCOS, RBUYTWKS FROM DETAILS                  
*                                                                               
         MVC   RBUYSTED,JBUYSTED   INSERT START/END DAYS                        
         TM    JBUYTYPE,X'80'      HIGH ORDER TURNED ON?                        
         BNO   BUYR0150            NO                                           
         OI    RBUYDUR,X'80'       YES - TURN ON IN NEW BUY                     
BUYR0150 EQU   *                                                                
         MVC   RBUYDUR+1(1),JBUYTYPE                                            
*                                  INSERT COMM'L LENGTH + QUALIFIER             
         NI    RBUYDUR+1,X'FF'-X'80'                                            
*                                  TURN OFF HIGH ORDER BIT                      
*                                     IN LENGTH BYTE                            
         CLI   JBUYELEM,X'18'      JDS BASE ELEMENT 16 BYTES?                   
         BNE   BUYR0152            NO  - SHORTER, OLDER STYLE                   
*                                     NO VERSION NUMBER IN ELEMENT              
         MVC   RBUYVER,JBUYVERS    INSERT VERSION NUMBER                        
*                                                                               
BUYR0152 EQU   *                                                                
         MVI   JDSELT,X'70'        GET DAYPART ELEMENT                          
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0003                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0003 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0160            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         MVC   RBUYDPT,1(RF)       INSERT DYPT FROM ELEMENT                     
BUYR0160 EQU   *                                                                
*                                                                               
*   GENERATE ONE OR MORE DAY/TIME ELEMENTS FROM BUY RECORD                      
*                                                                               
         MVI   JDSELT,X'20'        GET DAY/TIME ELEMENT                         
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0004                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0004 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0200                                                         
BUYR0180 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0200 EQU   *                                                                
         BNZ   BUYR0220            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         USING JBUYDYEL,RE                                                      
         LA    R7,ELTBILD1                                                      
         USING RBUYDYEL,R7                                                      
*                                                                               
         MVC   ELTBILD1(2),=X'0209'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         MVC   RBUYDYIN,JBUYDYSE   INSERT START/END DAY                         
         ZIC   RF,JBUYDYDY         RETRIEVE DAYS OF WEEK                        
         SRL   RF,1                DROP LOW-ORDER, SHIFT 1 RIGHT                
         STC   RF,RBUYDAYS         INSERT DAYS OF WEEK                          
         MVC   RBUYDYT1(4),JBUYDYST                                             
*                                  INSERT START/END TIMES                       
         MVI   RBUYDYWT,1          INSERT 1 IN WEIGHTING                        
         GOTO1 HELOBUY1            INSERT ELEMENT INTO BUY RECORD               
         B     BUYR0180            GO BACK FOR NEXT                             
*                                                                               
         DROP  RE,R7                                                            
*                                                                               
BUYR0220 EQU   *                                                                
*                                                                               
*   GENERATE ONE OR MORE EFF DATE ELEMENTS FROM BUY RECORD                      
*                                                                               
         MVI   JDSELT,X'30'        GET DAY/TIME ELEMENT                         
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0005                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0005 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0260                                                         
BUYR0240 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0260 EQU   *                                                                
         BNZ   BUYR0360            NOT FOUND                                    
         L     R6,JDSADDR          SET A(JDS ELT)                               
         USING JBUYDTEL,R6                                                      
         MVC   ELTBILD1(2),=X'030B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 JDSDATE,DMCB,JBUYDTST,ELTBILD1+2,(2,0)                           
*                                  INSERT START DATE                            
         MVC   ELTBILD1+5(3),ELTBILD1+2                                         
*                                  SET END DATE TO START                        
         CLI   JBUYDTEL,X'32'      SHORT JDS ELEMENT?                           
         BE    BUYR0280            YES - LEAVE END = START                      
         GOTO1 JDSDATE,DMCB,JBUYDTED,ELTBILD1+5,(2,0)                           
*                                  INSERT END DATE FROM JDS                     
BUYR0280 EQU   *                                                                
         OI    ELTBILD1+8,X'80'    SET 'RUNS EVERY WEEK'                        
         TM    JBUYDTNW,X'80'      ALTERNATE WEEK RUN?                          
         BNO   BUYR0300            NO                                           
         OI    ELTBILD1+8,X'40'    YES - SET 'RUNS EVERY OTHER WEEK'            
         NI    ELTBILD1+8,X'FF'-X'80'                                           
*                                  DROP 'RUNS EVERY WEEK'                       
BUYR0300 EQU   *                                                                
         MVC   ELTBILD1+9(1),JBUYDTNW                                           
*                                  INSERT JDS NUMBER PER WEEK                   
         NI    ELTBILD1+9,X'FF'-X'C0'                                           
*                                  TURN OFF HIGH-ORDER 2 BITS                   
         GOTO1 DATCON,DMCB,(3,ELTBILD1+2),(0,WORK)                              
*                                  CONVERT EFF START DATE TO EBCDIC             
         GOTO1 DATCON,DMCB,(3,ELTBILD1+5),(0,WORK+6)                            
*                                  CONVERT EFF END   DATE TO EBCDIC             
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
*                                  DETERMINE # EFFECTIVE WEEKS                  
         ZICM  RF,DMCB+12,2        GET NUMBER OF WKS QUOTIENT                   
         OC    DMCB+10(2),DMCB+10  ANY REMAINDER?                               
         BZ    BUYR0320            NO                                           
         LA    RF,1(RF)            YES - ADD 1 TO NUMBER OF WEEKS               
BUYR0320 EQU   *                                                                
         TM    ELTBILD1+8,X'80'    RUNS EVERY WEEK?                             
         BO    BUYR0340            YES                                          
         SRL   RF,1                NO  - ALT WEEKS: DIVIDE BY 2                 
BUYR0340 EQU   *                                                                
         STC   RF,ELTBILD1+10      INSERT NUMBER OF WEEKS                       
*                                                                               
         L     RE,TWEEKCTR         ACCUMULATE TOTAL WEEKS                       
         AR    RE,RF                                                            
         ST    RE,TWEEKCTR         SAVE TOTAL WEEKS                             
         SR    RE,RE                                                            
         ZIC   R1,ELTBILD1+9       NUMBER SPOTS PER WEEK                        
         MR    RE,R1               #WKS X SPTS/WEEK = TTL SPOTS                 
         L     RE,TSPTCTR          ACCUMULATE TOTAL SPOTS                       
         AR    RE,RF                                                            
         ST    RE,TSPTCTR          SAVE TOTAL SPOTS                             
         SR    RE,RE                                                            
         ZICM  R1,RBUYCOS,4        GET BUYLINE COST                             
         MR    RE,R1               TOT SPOTS X COST = TOTAL COST                
         L     RE,TCOSCTR                                                       
         AR    RE,RF                                                            
         ST    RE,TCOSCTR                                                       
         GOTO1 HELOBUY1            INSERT ELEMENT INTO BUY RECORD               
         B     BUYR0240            GO BACK FOR NEXT                             
*                                                                               
         DROP  R6                                                               
*                                                                               
BUYR0360 EQU   *                                                                
         MVC   RBUYTSPT,TSPTCTR+2  INSERT 2 BYTES FROM SPOT COUNTER             
         MVC   RBUYTCOS,TCOSCTR    INSERT 4 BYTES FROM COST COUNTER             
         MVC   RBUYTWKS,TWEEKCTR+3 INSERT 1 BYTE  FROM WEEK COUNTER             
         XC    TSPTCTR,TSPTCTR                                                  
         XC    TCOSCTR,TCOSCTR                                                  
         XC    TWEEKCTR,TWEEKCTR                                                
*                                                                               
         LA    R0,2                LOOK FOR TWO COMMENTS                        
         MVI   JDSELT,X'90'        GET PROGRAM NAME ELEMENT                     
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0006                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0006 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0400            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         L     RF,JDSLEN           SET L(JDS DATA)                              
*                                                                               
         MVI   ELTBILD1,4          INSERT ELEMENT TYPE                          
         BCTR  RF,0                DECREMENT FOR MOVE BY LENGTH                 
         MVC   ELTBILD1+2(2),=C'P='                                             
         EX    RF,BUYR0380         MOVE PROGRAM NAME BY LENGTH                  
         GOTO1 HELOBUY1            INSERT ELEMENT W/ERASE                       
         LA    R0,1                ONLY LOOK FOR 1 COMMENT                      
         B     BUYR0400                                                         
*                                                                               
BUYR0380 MVC   ELTBILD1+4(0),2(RE) MOVE PROG NAME BY LENGTH                     
*                                                                               
BUYR0400 EQU   *                                                                
         MVI   JDSELT,X'01'        GET BUY COMMENT  ELEMENT                     
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0007                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0007 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0440                                                         
BUYR0420 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0440 EQU   *                                                                
         BNZ   BUYR0480            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         L     RF,JDSLEN           SET L(JDS DATA)                              
*                                                                               
         MVI   ELTBILD1,4          INSERT ELEMENT TYPE                          
         BCTR  RF,0                DECREMENT FOR MOVE BY LENGTH                 
         EX    RF,BUYR0460         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            SET LENGTH                                   
         STC   RF,ELTBILD1+1       INSERT LENGTH                                
         GOTO1 HELOBUY1            INSERT ELEMENT W/ERASE                       
         BCT   R0,BUYR0420         LOOK FOR ANOTHER COMMENT                     
         B     BUYR0480                                                         
*                                                                               
BUYR0460 MVC   ELTBILD1+2(0),2(RE) MOVE BUY COMENT BY LENGTH                    
*                                                                               
BUYR0480 EQU   *                                                                
*                                                                               
*   GENERATE ONE OR MORE BUY M/G REFERENCE ELEMENTS                             
*                                                                               
         MVI   MAKEGOOD,C'N'       SET 'NO MAKEGOOD'                            
         XC    MGINFO,MGINFO       CLEAR MAKEGOOD INFO SAVE AREA                
         MVI   JDSELT,X'D0'        GET M/G REFERENCE ELEMENT                    
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0008                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0008 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0520                                                         
BUYR0500 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0520 EQU   *                                                                
         BNZ   BUYR0540            NOT FOUND                                    
         MVI   MAKEGOOD,C'Y'       SET 'MAKEGOOD'                               
         L     R6,JDSADDR          SET A(JDS ELT)                               
         USING JBUYMGEL,R6                                                      
         OC    MGINFO,MGINFO       FIRST PASS?                                  
         BNZ   BUYR0530            NO  - DON'T DO AGAIN                         
         MVC   MGLINE,JBUYLIN#     SAVE LINE NUMBER MISSED                      
         GOTO1 JDSDATE,DMCB,JBUYMSDT,MGDATE,(2,0)                               
*                                  SAVE MISSED DATE FROM JDS                    
         MVC   MGSPOTS,JBUY#MDS    SAVE # MISSED SPOTS                          
BUYR0530 EQU   *                                                                
         MVC   ELTBILD1(2),=X'050A'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         MVC   ELTBILD1+2(1),JBUYLIN#                                           
*                                  INSERT LINE # OF MISSED SPOT                 
         GOTO1 JDSDATE,DMCB,JBUYMSDT,ELTBILD1+3,(2,0)                           
*                                  INSERT MISSED DATE FROM JDS                  
         MVC   ELTBILD1+9(1),JBUY#MDS                                           
*                                  INSERT NUMBER OF MISSED SPOTS                
         CLI   JBUY#MDS,0          JDS # SPOTS MISSED = 0?                      
         BNE   BUYR0535            NO                                           
         MVI   ELTBILD1+9,1        YES - SET TO 1                               
BUYR0535 EQU   *                                                                
         GOTO1 HELOBUYA            INSERT X'05' W/O ERASE                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVC   ELTBILD1(2),=X'5607'                                             
*                                  INSERT XREF ELEMENT CODE/LENGTH              
         MVC   ELTBILD1+2(3),ELTBILD1+3                                         
*                                  SLIDE DATE OVER 1 BYTE                       
         MVC   ELTBILD1+5(1),RBUYKLIN                                           
*                                  INSERT REFERENCE LINE NUMBER                 
         MVC   ELTBILD1+6(1),ELTBILD1+9                                         
*                                  SLIDE OVER NUMBER OF SPOTS                   
         GOTO1 HELOREC4            INSERT ELEMENT INTO RBUYREC                  
*                                     IN RECORD4 AREA                           
*                                        AND ERASE ELEMENT                      
         B     BUYR0500            GO BACK FOR NEXT ELEMENT                     
BUYR0540 EQU   *                                                                
         CLI   MAKEGOOD,C'Y'       MAKEGOOD IN BUY?                             
         BNE   BUYR0550            NO                                           
         GOTO1 =A(MGCOMM),DMCB,(RC),RR=Y                                        
*                                  YES - ADD INFO TO FIRST BUY COMMENT          
BUYR0550 EQU   *                                                                
         LA    R0,2                SET MAX OF 2                                 
         MVI   JDSELT,X'03'        GET BUY ORDER COMMENT  ELEMENT               
*                                                                               
*   TEST                                                                        
*        CLC   JBUYKCON,=X'97809409'                                            
*        BNE   BTST0009                                                         
*        MVC   P+1(08),=C'ENTERING'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0009 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0580                                                         
BUYR0560 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0580 EQU   *                                                                
         BNZ   BUYR0620            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         L     RF,JDSLEN           SET L(JDS DATA)                              
*                                                                               
         MVI   ELTBILD1,X'84'      INSERT ELEMENT TYPE                          
         BCTR  RF,0                DECREMENT FOR MOVE BY LENGTH                 
         EX    RF,BUYR0600         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            SET ELEMENT LENGTH                           
         STC   RF,ELTBILD1+1       INSERT LENGTH INTO ELEMENT                   
         GOTO1 HELOBUY1            INSERT ELEMENT W/ERASE                       
         BCT   R0,BUYR0560         LOOK FOR ANOTHER COMMENT                     
         B     BUYR0620                                                         
*                                                                               
BUYR0600 MVC   ELTBILD1+2(0),2(RE) MOVE BUY ORD COM BY LENGTH                   
*                                                                               
BUYR0620 EQU   *                                                                
         CLC   RBUYKMLN,RBUYKLIN   MASTER = DETAIL LINE NUMBER?                 
         BE    BUYR0640            YES - MOVE TO ALTERNATE AREA                 
*                                  NO  - MAKEGOOD RECORD CAN BE                 
*                                     WRITTEN TO OUTPUT                         
         GOTO1 PUTRECS,DMCB,(RC),RECORD                                         
*                                  NO  - PUT RECORD  TO OUTPUT                  
         GOTO1 DISPPUT,DMCB,(RC)                                                
         B     BUYR0660                                                         
BUYR0640 EQU   *                                                                
         PRINT GEN                                                              
         L     RF,ARECORD4                                                      
         MOVE  ((RF),1024),RBUYREC                                              
*                                  SAVE BUY REC FOR POSSIBLE MG XREFS           
         MVI   BUYREC4,C'Y'        SET 'RECORD IN ALT AREA'                     
         PRINT NOGEN                                                            
BUYR0660 EQU   *                                                                
BUYR0680 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CATRPROC:  PROCESS THE CATEGORY  RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
CATRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CLSRPROC:  PROCESS THE CLASS     RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
CLSRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XIT1                                                                   
         EJECT                                                                  
*   GETJDSEL:  RETRIEVE A(ELEMENT SOUGHT) BASED ON ELEMENT TYPE                 
*   GETJDSNX:  RETRIEVE NEXT ELEMENT OF TYPE.  JDSELT MUST BE SET,              
*        AND JDSADDR MUST CONTAIN ADDRESS OF LAST ELEMENT FOUND.                
*                                                                               
GETJDSNX NTR1                                                                   
*                                                                               
*   TEST                                                                        
*        CLC   JCONKCON,=X'01159201'                                            
*        BNE   GTST0001                                                         
*        MVC   P+1(08),=C'NX SEEK:'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
GTST0001 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LA    R1,RECORD2+24       SET A(RECORD IN PROCESS: LENGTH)             
         LR    R2,R1               CALCULATE END OF RECORD                      
         ZICM  RF,JCONLEN,2        GET RECORD LENGTH                            
         AR    R2,RF               FIND END OF RECORD                           
         L     R1,JDSADDR          SET A(LAST ELEMENT)                          
         TM    0(R1),X'F0'         DETERMINE COMPRESSED/UNCOMPRESSED            
         BNZ   GJDS0400            COMPRESSED: SKIP THIS ELEMENT                
         B     GJDS0100            UNCOMPRESSED: SKIP THIS ELEMENT              
GETJDSEL NTR1                                                                   
*                                                                               
*   TEST                                                                        
*        CLC   JCONKCON,=X'01159201'                                            
*        BNE   GTST0010                                                         
*        MVC   P+1(08),=C'EL SEEK:'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
GTST0010 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LA    R1,RECORD2+24       SET A(RECORD LENGTH)                         
         LR    R2,R1               CALCULATE END OF RECORD                      
         ZICM  RF,JCONLEN,2        GET RECORD LENGTH                            
         AR    R2,RF               FIND END OF RECORD                           
         LA    R1,RECORD2+66       SET A(DESCRIPTOR ELEMENT)                    
         CLI   0(R1),0             EMPTY ELEMENT FOUND?                         
         BE    GJDS0800            YES - END OF RECORD REACHED                  
*                                                                               
         TM    0(R1),X'F0'         DETERMINE COMPRESSED/UNCOMPRESSED            
         BNZ   GJDS0200            COMPRESSED:                                  
GJDS0020 EQU   *                                                                
         CR    R1,R2               END OF RECORD REACHED?                       
         BNL   GJDS0800            YES - NO ELT: RETURN CC NOT ZERO             
*                                                                               
*   TEST                                                                        
*        MVC   P+1(04),=C'ELT='                                                 
*        MVC   P+5(20),0(R1)                                                    
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   0(R1),0             EMPTY ELEMENT FOUND?                         
         BE    GJDS0800            YES - END OF RECORD REACHED                  
*                                                                               
         TM    0(R1),X'F0'         ELT COMPRESSED/UNCOMPRESSED?                 
         BNZ   GJDS0200            COMPRESSED                                   
*                                  UNCOMPRESSED                                 
         CLC   JDSELT,0(R1)        COMPARE ELEMENT TYPE                         
         BNE   GJDS0100            NOT EQUAL - GO TO NEXT                       
         ST    R1,JDSADDR          SAVE A(ELEMENT FOUND)                        
         ZIC   RF,1(R1)            GET ELEMENT LENGTH                           
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         SH    RF,=H'2'            SUBTRACT TWO BYTES FOR CONTROL               
         ST    RF,JDSLEN           SAVE ELEMENT LENGTH                          
         B     GJDS1000            EXIT: RETURN CC = ZERO                       
GJDS0100 EQU   *                   BUMP PAST UNCOMPRESSED ELT                   
         ZIC   RF,1(R1)            RETRIEVE ELEMENT LENGTH                      
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         AR    R1,RF               ADD TO ELEMENT ADDR                          
         B     GJDS0020            GO BACK FOR NEXT ELEMENT                     
GJDS0200 EQU   *                                                                
         ZIC   RE,0(R1)            UNLOAD ELT/LENGTH                            
         SRL   RE,4                DROP LENGTH NYBBLE                           
         SLL   RE,4                RESTORE TO ORIG PLACE                        
         STC   RE,JDSELT2                                                       
         CLC   JDSELT,JDSELT2      COMPARE ELT SOUGHT VS THIS ELT               
         BNE   GJDS0400            NOT FOUND -                                  
         ST    R1,JDSADDR          SAVE A(ELEMENT FOUND)                        
         MVC   JDSELT2,0(R1)       RETRIEVE ELT CODE/LNGTH AGAIN                
         NI    JDSELT2,X'FF'-X'F0' TURN OFF ELT CODE, LEAVING LENGTH            
         ZIC   RF,JDSELT2          PROCESS LENGTH                               
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         BCTR  RF,0                SUBTRACT 1 BYTE FOR CONTROL                  
         ST    RF,JDSLEN           SAVE ELEMENT LENGTH                          
         B     GJDS1000            EXIT: RETURN CC = ZERO                       
GJDS0400 EQU   *                                                                
         MVC   JDSELT2,0(R1)       RETRIEVE ELT CODE/LNGTH AGAIN                
         NI    JDSELT2,X'FF'-X'F0' TURN OFF ELT CODE, LEAVING LENGTH            
         ZIC   RF,JDSELT2          PROCESS LENGTH                               
         SLL   RF,1                DOUBLE # WDS INTO BYTES                      
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         B     GJDS0020            GO BACK FOR NEXT ELEMENT                     
GJDS0800 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLC   JCONKCON,=X'01159201'                                            
*        BNE   BTST0800                                                         
*        MVC   P+1(08),=C'EXIT N/F'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST0800 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LTR   RB,RB               SET CC NOT ZERO                              
         B     GJDS1200                                                         
GJDS1000 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLC   JCONKCON,=X'01159201'                                            
*        BNE   BTST1000                                                         
*        MVC   P+1(08),=C'EXIT FND'                                             
*        GOTO1 HEXOUT,DMCB,JDSELT,P+10,1,=C'TOG'                                
*        GOTO1 REPORT                                                           
BTST1000 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         SR    R0,R0                                                            
GJDS1200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   JDSDATE:  CONVERT JDS FORMAT DATES TO A DDS FORMAT DATE                     
*        P1  =   ADDR(INPUT DATE)                                               
*        P2  =   ADDR(OUTPUT DATE)                                              
*        P3  =   DATE TYPE:  2  = YMD 2 BYTE FORMAT                             
*                            1  = YM  1 BYTE FORMAT                             
*        P4  =                                                                  
*                                                                               
JDSDATE  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         MVC   DATEFLAG,8(R1)                                                   
         ZIC   RF,0(R2)            GET YR/MON BYTE                              
         SRL   RF,4                DROP MONTH NYBBLE                            
         A     RF,BASEYEAR         ADD BASE YEAR TO YEAR BYTE                   
         STC   RF,0(R3)            INSERT YEAR INTO OUTPUT                      
         ZIC   RF,0(R2)            GET YR/MON BYTE AGAIN                        
         SLL   RF,28               DROP YEAR NYBBLE                             
         SRL   RF,28               REALIGN MONTH                                
         STC   RF,1(R3)            INSERT MONTH INTO OUTPUT                     
         CLI   DATEFLAG,1          YEAR/MONTH ONLY?                             
         BE    JDSD0100            YES - FINISHED                               
         MVC   2(1,R3),1(R2)       NO  - INSERT DAY INTO OUTPUT                 
JDSD0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   HELLO CALLS:  LABEL NAME INDICATES RECORD AND ELEMENT ADDRESS:              
*        HELO  =  HELLO CALL                                                    
*        CON   =  CONTRACT RECORD, BUY   =  BUY RECORD                          
*        1     =  ELTBILD1, ETC                                                 
*                                                                               
HELOCON1 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD1,0             
         XC    ELTBILD1,ELTBILD1                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCONA NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD1,     X        
               =C'ADD=CODE'                                                     
*                                  INSERT ON CODE SEQUENCE ONLY, AND            
*                                     DON'T CLEAR THE ELEMENT                   
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCON2 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD2,0             
         XC    ELTBILD2,ELTBILD2                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCON3 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD3,0             
         XC    ELTBILD3,ELTBILD3                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUY1 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD1,0             
         XC    ELTBILD1,ELTBILD1                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUY2 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD2,0             
         XC    ELTBILD2,ELTBILD2                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUY3 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD3,0             
         XC    ELTBILD3,ELTBILD3                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUYA NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD1,0             
*                                  DON'T CLEAR THE ELEMENT                      
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOREC4 NTR1                                                                   
         L     RF,ARECORD4                                                      
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),(RF),ELTBILD1,0                
         XC    ELTBILD3,ELTBILD3                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P+1(8),=C'CONTRACT'                                              
         CLI   REC,X'0C'           CONTRACT?                                    
         BNE   DIPU0010            YES                                          
         CLI   QUESTOR+3,C'Y'      DISPLAY CONTRACT OUTPUT?                     
         BNE   DIPU0090            NO  - SKIP THIS RECORD                       
         CLC   CONCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   CONCTR,HIGHCTR                                                   
         BH    DIPU0090                                                         
         B     DIPU0020            DISPLAY THE RECORD                           
DIPU0010 EQU   *                                                                
         MVC   P+1(8),=C'BUY REC '                                              
         CLI   REC,X'0B'           BUY ?                                        
         BNE   DIPU0015            NO                                           
*                                                                               
*   TEST                                                                        
****     CLC   JBUYKCON,=X'97955980'                                            
***      CLC   JBUYKCON,=X'97932451'                                            
*        BE    DIPU0020                                                         
*   TEST END                                                                    
*                                                                               
         CLI   QUESTOR+5,C'Y'      DISPLAY BUY      OUTPUT?                     
         BNE   DIPU0090            NO  - SKIP THIS RECORD                       
         CLC   BUYCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   BUYCTR,HIGHCTR                                                   
         BH    DIPU0090                                                         
         B     DIPU0020            DISPLAY THE RECORD                           
DIPU0015 EQU   *                                                                
         MVC   P+1(8),=C'ADV REC '                                              
         CLI   REC,X'08'           ADVERTISER?                                  
         BNE   DIPU0090            YES                                          
         CLI   QUESTOR+1,C'Y'      DISPLAY ADVERT   OUTPUT?                     
         BNE   DIPU0090            NO  - SKIP THIS RECORD                       
         CLC   ADVCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   ADVCTR,HIGHCTR                                                   
         BH    DIPU0090                                                         
         B     DIPU0020            DISPLAY THE RECORD                           
DIPU0020 EQU   *                                                                
         MVC   P+10(06),=C'OUTPUT'                                              
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              INSERT A BLANK LINE                          
DIPU0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
******************************************************************              
*  DISPPUTB:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.               *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUTB NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P+1(8),=C'BUY REC '                                              
*                                                                               
*   TEST                                                                        
****     CLC   JBUYKCON,=X'97955980'                                            
*        CLC   JBUYKCON,=X'97932451'                                            
*        BE    DIPB0020                                                         
*   TEST END                                                                    
*                                                                               
         CLI   QUESTOR+5,C'Y'      DISPLAY BUY      OUTPUT?                     
         BNE   DIPB0090            NO  - SKIP THIS RECORD                       
         CLC   BUYCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPB0090                                                         
         CLC   BUYCTR,HIGHCTR                                                   
         BH    DIPB0090                                                         
DIPB0020 EQU   *                                                                
         MVC   P+10(06),=C'OUTPUT'                                              
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              INSERT A BLANK LINE                          
DIPB0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            SET A(RECORD BEING OUTPUT)                   
*                                                                               
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           PUT IT BACK                                  
*                                                                               
*        MVC   P+1(07),=C'PUTREC:'                                              
*        MVC   P+20(27),REC                                                     
*        EDIT  PUTCTR,(5,P+10)                                                  
*        GOTO1 REPORT                                                           
*                                                                               
         LA    RF,REC                                                           
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(R2)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RCONLEN-RCONKEY(R2)                                     
*                                  INSERT LENGTH, USING COMMON RCONLEN          
*                                                                               
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
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
ASTAAREA DS    A                   STATION/GROUP-SUBGROUP AREA                  
ASTAEND  DS    A                   A(LAST ENTRY IN TABLE)                       
ACMPAREA DS    A                   STATION COMPETITIVE AREA                     
ACMPNXT  DS    A                   A(NEXT AVAILABLE COMP SLOT)                  
ASALAREA DS    A                   A(SALESPERSON AREA)                          
ASALEND  DS    A                   A(LAST ENTRY IN TABLE)                       
A2SPAREA DS    A                   A(2ND S/P AREA)                              
A2SPEND  DS    A                   A(LAST ENTRY IN TABLE)                       
AMISAREA DS    A                   A(STATION MISSING AREA)                      
ANEXTMIS DS    A                                                                
AAGYAREA DS    A                   AGENCY CONVERSION AREA                       
AAGYEND  DS    A                   A(LAST ENTRY IN TABLE)                       
AAG2AREA DS    A                   MISSING AGENCY AREA                          
AAG2END  DS    A                   A(LAST ENTRY IN TABLE)                       
ANEWSALS DS    A                                                                
ANXTSAL  DS    A                                                                
ASPNAREA DS    A                                                                
ASPNEND  DS    A                                                                
ANEWPPRS DS    A                                                                
ANXTPPR  DS    A                                                                
ARECORD4 DS    A                                                                
ARECORD5 DS    A                                                                
ACATAREA DS    A                                                                
NEWPPRCT DS    F                                                                
LBLDAREA DS    F                                                                
MGCMAREA DS    F                                                                
TOTCTR   DS    F                                                                
DISPCTR  DS    F                                                                
CONDCTR  DS    F                                                                
BUYDCTR  DS    F                                                                
CONCTR   DS    F                                                                
CONSREAD DS    F                                                                
BKRENCTR DS    F                                                                
NAMCTR   DS    F                                                                
NOPRDCTR DS    F                                                                
SHORTCTR DS    F                                                                
BUYCTR   DS    F                                                                
BUYSREAD DS    F                                                                
ADVCTR   DS    F                                                                
AGYCTRIN DS    F                                                                
AGYCTR   DS    F                                                                
AGYCTR2  DS    F                                                                
AGYCTR3  DS    F                                                                
AG2CTR   DS    F                                                                
TSPTCTR  DS    F                                                                
TCOSCTR  DS    F                                                                
TWEEKCTR DS    F                                                                
CMPCOMCT DS    F                                                                
SARCOMCT DS    F                                                                
REPCOMCT DS    F                                                                
STDCMTCT DS    F                                                                
STACOMCT DS    F                                                                
NEWSPCTR DS    F                                                                
NEWSTATS DS    F                                                                
LOWCTR   DC    F'99999'            LOW DISPLAY COUNT                            
HIGHCTR  DC    F'99999'            HIGH COUNTER                                 
PUTCTR   DS    F                                                                
STACTR   DS    F                                                                
CMPCTR   DS    F                                                                
COMPCTR  DS    F                                                                
SALCTR   DS    F                                                                
SP2CTR   DS    F                                                                
SALMISS  DS    F                                                                
MISSTAT  DS    F                                                                
AIOAREA  DS    F                                                                
BUCKWORK DS    4F                  BUCKET UPDATE ARGUMENTS                      
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
ELTBILD1 DS    CL128                                                            
ELTBILD2 DS    CL128                                                            
ELTBILD3 DS    CL128                                                            
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
COMMLEN  DS    F                                                                
WORK2    DS    CL256                                                            
*                                                                               
KEYSAV2  DS    CL27                                                             
STAWORK  DS    CL6                                                              
PETRYREP DS    CL2                                                              
PETRYRP2 DS    CL2                                                              
COMMSKIP DS    CL1                                                              
COMTFLAG DS    CL1                                                              
DATEFLAG DS    CL1                                                              
SVJEFLAG DS    CL1                                                              
SVJPFLAG DS    CL1                                                              
SAVJCNTL DS    CL1                                                              
SAVJCODE DS    CL1                                                              
SAVJPEFF DS    CL1                                                              
SAVCELEM DS    CL28                                                             
SAVETRGT DS    CL1                                                              
SVCONMOD DS    CL1                                                              
SVCONF   DS    CL1                                                              
SVSENF   DS    CL1                                                              
SVCONSRV DS    CL1                                                              
CMPFLAG  DS    CL1                                                              
SHRGOAL  DS    CL1                                                              
SPLFLAG  DS    CL1                                                              
ACTCFLAG DS    CL1                                                              
ACCTPROD DS    CL1                                                              
ACEGRAPH DS    XL1                 ACE/GRAPHNET INDICATOR                       
JDSELT   DS    CL1                 JDS ELEMENT SEARCH ARGUMENT                  
JDSELT2  DS    CL1                 JDS ELEMENT SEARCH ARGUMENT                  
BLKFLAG  DS    CL1                 BLOCK EMPTY FLAG                             
BLK#1    DS    CL1                 BLOCK 'FIRST PASS' FLAG                      
NAMEDUMP DS    CL1                                                              
LOADALFA DS    CL1                                                              
MAKEGOOD DS    CL1                                                              
MGINFO   DS    0CL08               MAKEGOOD INFO SAVE AREA                      
MGLINE   DS    XL1                 M/G LINE #                                   
MGDATE   DS    XL3                 M/G MISSED DATE                              
MGSPOTS  DS    XL1                 M/G # MISSED                                 
MGSPARE  DS    CL3                 SPARE                                        
CONFLAG  DS    CL1                                                              
SAVER2   DS    A                                                                
JDSADDR  DS    F                                                                
JDSLEN   DS    F                                                                
BLKADDR  DS    F                   ADDRESS OF CURRENT RECORD IN BLOCK           
BLKEND   DS    F                   ADDRESS OF CURRENT RECORD EOR                
BASEYEAR DC    F'84'               BASE YEAR IS UNDETERMINED!!                  
BUYREC4  DS    CL1                 RECORD IN BUYREC4 AREA FLAG                  
TEAMOFF  DS    CL2                 JDS OFFICE FOR TEAM DEVELOPMENT              
TEAMDIV  DS    CL2                 JDS DIV/TEAM FOR TEAM DEVEL                  
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=1024,             X        
               BLKSIZE=30720,MACRF=GM,EODAD=GETT0100                            
****>>>>       BLKSIZE=20480,MACRF=GM,EODAD=GETT0100                            
*                                                                               
         SPACE 3                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ISOLATE REC AFTER LTORG TO PRESERVE ADDRESSABILITY                          
*                                                                               
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL2048              AREA FOR RECORD                              
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL2048                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY2         AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERT      RECORD                           
         EJECT                                                                  
         ORG                                                                    
RECORD2  DS    CL1024                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCLS          CLASS RECORD                                 
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSCTG          CATEGORY RECORD                              
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSADV          ADVERTISER RECORD                            
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSAGY          AGENCY RECORD                                
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REJDSSTA          STATION RECORD                               
         EJECT                                                                  
*  OTHER JDS RECORD DSECTS GET ORG'D HERE                                       
         EJECT                                                                  
         ORG                                                                    
RECORD3  DS    CL1024                                                           
         ORG                                                                    
RECORD4  DS    CL1024                                                           
RECORD5  DS    CL1024                                                           
         ORG   RECORD5                                                          
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD5                                                          
       ++INCLUDE REGENXXX          COMMENT RECORD                               
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         EJECT                                                                  
*********************************************************************           
         CSECT                                                                  
*                                                                               
*   REPCOMMT:                                                                   
*                                                                               
REPCOMMT NMOD1 0,*RCOM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   ACTCFLAG,C'N'       TURN 02 ACTIVITY COMMENT FLAG OFF            
         MVI   JDSELT,X'02'        GET ACTIVITY COMMENT ELEMENT                 
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   REPC0080            FINISHED                                     
         B     REPC0040                                                         
REPC0020 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
REPC0040 EQU   *                                                                
         BNZ   REPC0080            NOT FOUND                                    
*                                                                               
*   TEST                                                                        
*        MVC   P+1(12),=C'02 ACT CMTS:'                                         
*        GOTO1 REPORT                                                           
*        GOTO1 =A(DISPCOMS),DMCB,(RC),0,RR=Y                                    
*   TEST END                                                                    
*                                                                               
         L     RF,JDSADDR          SET A(JDS ELT)                               
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,REPC0060         MOVE ACTIVITY COMMT BY LENGTH                
         LA    RE,3(RE)            INCREMENT FOR LENGTH                         
         STC   RE,ELTBILD1+1       INSERT LENGTH                                
         MVI   ELTBILD1,X'11'      SET 'REP ORDER COMMENT'                      
         GOTO1 HELOCONA            INSERT ELEMENT, W/O ERASE                    
         MVI   ACTCFLAG,C'Y'       TURN 02 ACT COMMT FLAG ON                    
         B     REPC0020            LOOK FOR NEXT ELEMENT                        
REPC0060 EQU   *                                                                
         MVC   ELTBILD1+2(0),2(RF)                                              
REPC0080 EQU   *                                                                
         XC    ELTBILD1,ELTBILD1   CLEAR ELEMENT AT END                         
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ORDRECD:  CHECK FOR AN ACTIVITY COMMENT RECORD ON THE FILE FOR              
*        THIS CONTRACT NUMBER.  IF FOUND, ADD ITS COMMENTS TO THE               
*        RECORD AS TYPE 82 ELEMENTS.                                            
ORDRECD  NMOD1 0,*SORD*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R6,ARECORD5         SET A(IO AREA)                               
         ST    R6,AIOAREA                                                       
         USING RXXXREC,R6                                                       
         MVI   COMTFLAG,C'N'       TURN OFF 'COMMENT EXISTS' FLAG               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'47'           INSET RECORD TYPE                            
         MVC   KEY+20(2),PETRYREP  INSERT REP CODE FROM CARD                    
         MVC   KEY+22(4),RCONKCON  INSERT CONTRACT NUMBER                       
*                                                                               
*   1.   BRING IN REP ORDER COMMENTS FIRST                                      
*                                                                               
         MVI   KEY+26,1            SET 'REP ORDER COMMENT'                      
         GOTO1 HIGH                READ KEY                                     
*                                                                               
*   TEST                                                                        
*        MVC   P+1(27),KEY                                                      
*        MVC   P+30(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BNE   ORDR0060            NO                                           
         GOTO1 GREC                                                             
         LA    R5,RXXXELEM         SET A(DESCRIPTIVE ELEMENT)                   
ORDR0020 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    ORDR0060            YES - FINISHED                               
         CLI   0(R5),3             COMMENT?                                     
         BNE   ORDR0040            NO                                           
         MVI   COMTFLAG,C'Y'                                                    
         MVI   0(R5),X'82'         INSERT ELEMENT CODE ID                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,(R5),         X        
               =C'ADD=CODE'                                                     
*                                  ADD FROM COMMENT RECORD                      
ORDR0040 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     ORDR0020            GO BACK FOR NEXT                             
ORDR0060 EQU   *                                                                
         CLI   COMTFLAG,C'Y'       COMMENT EXISTS?                              
         BNE   ORDR0080            NO                                           
*                                                                               
*   TEST                                                                        
*        MVC   P+1(12),=C'KEY/KEYSAVE='                                         
*        MVC   P+13(27),KEY                                                     
*        MVI   P+40,C'/'                                                        
*        MVC   P+41(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         L     RF,REPCOMCT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,REPCOMCT                                                      
         CLC   REPCOMCT,=F'00'     DISPLAY FIRST N CONTRACTS                    
         BH    ORDR0080                                                         
         MVC   P+1(17),=C'REPCOM: JCONEFLG='                                    
         GOTO1 HEXOUT,DMCB,JCONEFLG,P+18,1,=C'TOG'                              
         GOTO1 REPORT                                                           
         GOTO1 =A(DISPCOMS),DMCB,(RC),0,RR=Y                                    
*                                                                               
ORDR0080 EQU   *                                                                
*                                                                               
*   2.   BRING IN STATION COMMENTS NEXT                                         
*                                                                               
         MVI   COMTFLAG,C'N'       TURN OFF 'COMMENT EXISTS' FLAG               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'47'           INSET RECORD TYPE                            
         MVC   KEY+20(2),PETRYREP  INSERT REP CODE FROM CARD                    
         MVC   KEY+22(4),RCONKCON  INSERT CONTRACT NUMBER                       
         MVI   KEY+26,2            SET 'STA ORDER COMMENT'                      
         GOTO1 HIGH                READ KEY                                     
*                                                                               
*   TEST                                                                        
*        MVC   P+1(27),KEY                                                      
*        MVC   P+30(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BNE   ORDR0160            NO                                           
         GOTO1 GREC                                                             
         LA    R5,RXXXELEM         SET A(DESCRIPTIVE ELEMENT)                   
ORDR0120 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    ORDR0160            YES - FINISHED                               
         CLI   0(R5),3             COMMENT?                                     
         BNE   ORDR0140            NO                                           
         MVI   COMTFLAG,C'Y'                                                    
         MVI   0(R5),X'92'         INSERT ELEMENT CODE ID                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,(R5),         X        
               =C'ADD=CODE'                                                     
*                                  ADD FROM COMMENT RECORD                      
ORDR0140 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     ORDR0120            GO BACK FOR NEXT                             
ORDR0160 EQU   *                                                                
         CLI   COMTFLAG,C'Y'       COMMENT EXISTS?                              
         BNE   ORDR0180            NO                                           
*                                                                               
*   TEST                                                                        
*        MVC   P+1(12),=C'KEY/KEYSAVE='                                         
*        MVC   P+13(27),KEY                                                     
*        MVI   P+40,C'/'                                                        
*        MVC   P+41(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         L     RF,STACOMCT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STACOMCT                                                      
         CLC   STACOMCT,=F'00'     DISPLAY FIRST N CONTRACTS                    
         BH    ORDR0180                                                         
         MVC   P+1(17),=C'STACOM: JCONEFLG='                                    
         GOTO1 HEXOUT,DMCB,JCONEFLG,P+18,1,=C'TOG'                              
         GOTO1 REPORT                                                           
         GOTO1 =A(DISPCOMS),DMCB,(RC),0,RR=Y                                    
ORDR0180 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  EXCONELT:  BUILD THE X'1F'/X'20'S ELEMENT FROM JDS INFO       *              
*                                                                *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
EXCONELT NMOD1 0,*EXCO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   SAVCELEM,JCONCODE   SAVE ENTIRE ELEMENT                          
         MVC   SAVJCODE,JCONCODE   SAVE ELEMENT CODE                            
         MVC   SAVJCNTL,JCONCNTL   SAVE JDS CONTROL BYTE                        
         MVC   SVJEFLAG,JCONEFLG   SAVE JDS EOP FLAG                            
         MVC   SVJPFLAG,JCONPFLG   SAVE JDS PREVIOUS EOP FLAG                   
         MVC   SAVJPEFF,JCONEFF    SAVE JDS EOP FLAG LAST CONFIRM               
         MVC   SVCONMOD,JCONMOD    SAVE JDS MOD NUMBER                          
         LA    R2,ELTBILD1                                                      
         LA    R3,ELTBILD2                                                      
         USING RCONXEL,R2          BUILD THE X'1F' ELT                          
         USING RCONSEND,R3         BUILD THE X'20' ELT                          
         MVC   RCONXEL(2),=X'1F18'                                              
         MVC   RCONSNCO(2),=X'2029'                                             
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(3,RCONCREA),(2,RCONSRDT)                            
*                                  INSERT SEND DATE INTO ELEMENT                
         PRINT NOGEN                                                            
         MVC   RCONSRTI,=C'120000' INSERT DUMMY TIME                            
*                                                                               
*   BIT SETTINGS:   RCONSENF                                                    
*        X'80'  =  LAST SENT BY REP                                             
*        X'40'  =  LAST SENT BY STA                                             
*        X'20'  =  REP VERSION # NOT ADVANCED                                   
*        X'10'  =  STA VERSION # NOT ADVANCED                                   
*        X'08'  =                                                               
*        X'04'  =                                                               
*        X'02'  =  LAST CONFIRMED BY STATION                                    
*        X'01'  =  LAST CONFIRMED BY TAKEOVER                                   
*                                                                               
*   CHECK SIZE OF ELEMENT:  IF 1E, CAN CHECK ALL FLAGS.  IF 19, JUST            
*        CHECK CNTL FIELD FOR CONFIRMED, UNCONFIRMED.                           
*                                                                               
         CLI   ACCTPROD,C'Y'       'ACC-' PRODUCT CONTRACT?                     
         BNE   EXCO0020            NO                                           
         MVC   RCONMOD,SVCONMOD    YES - FORCE SETUP                            
         MVI   RCONCONF,X'40'      SET 'CONFIRMED NOW'                          
         MVC   RCONSENF(2),=X'1003'                                             
*                                  FORCE TO 'CFX'/VERSION 3 WIP                 
*                                     AT NEXT CHANGE                            
         MVI   RCONCFX#,X'FF'      TURN ON CFX                                  
         B     EXCO0280                                                         
EXCO0020 EQU   *                                                                
         CLI   SAVJCODE,X'1E'      FULL SET OF FLAGS?                           
         BE    EXCO0100            YES                                          
         CLI   SAVJCODE,X'19'      SHORT ELEMENT?                               
         BE    EXCO0040            YES                                          
         CLI   SAVJCODE,X'1A'      SHORT ELEMENT?                               
         BNE   EXCO0080            NO  - DISPLAY LENGTH OF ELEMENT              
EXCO0040 EQU   *                                                                
         L     RF,SHORTCTR         KEEP COUNT OF SHORT ELTS                     
         LA    RF,1(RF)                                                         
         ST    RF,SHORTCTR                                                      
*                                                                               
         TM    SAVJCNTL,X'20'      'UNCONFIRMED' JDS ORDER?                     
         BO    EXCO0060            YES - UNCONFIRMED                            
*                                  NO  - SET TO 'CONFIRMED'                     
         MVC   RCONMOD,SVCONMOD    USE THIS MOD #                               
         MVI   RCONCONF,X'40'      SET 'CONFIRMED NOW'                          
         MVI   RCONSENF,X'72'      SET 'LST SENT/CFD BY STATION'                
         B     EXCO0240            GO SET VERSION NUMBER                        
EXCO0060 EQU   *                                                                
         MVC   RCONMOD,SVCONMOD    NO  - USE THIS MOD                           
         MVI   RCONCONF,X'80'      SET 'UNCONFIRMED'                            
         CLI   SAVJPEFF,0          WAS IT PREVIOUSLY CONFIRMED?                 
         BZ    EXCO0070            NO  - LEAVE ONLY 'UNCF'D'                    
         OI    RCONCONF,X'20'      YES - SET 'PREVIOUSLY CONFIRMED'             
         MVI   RCONSENF,X'B2'      SET 'LST SENT/CFD BY STATION                 
*                                     BUT NOT CONFIRMED NOW                     
         MVC   RCONSSDT,RCONSRDT   SET 'STA SENT DATE/TIME'                     
         MVC   RCONSSTI,RCONSRTI                                                
         B     EXCO0240            GO SET VERSION NUMBER                        
EXCO0070 EQU   *                                                                
         MVC   RCONSENF(2),=X'1001'                                             
         TM    SVJEFLAG,X'60'      SENT BY REP?                                 
         BZ    EXCO0280            NO  - LEAVE AS SET                           
         OI    RCONSENF,X'A0'      YES - SET 'SENT BY REP/REP V# N/ADV'         
EXCO0075 EQU   *                                                                
         B     EXCO0240            GO SET VERSION NUMBER                        
EXCO0080 EQU   *                                                                
         MVC   P+1(08),=C'LENGTH?='                                             
         MVC   P+9(28),SAVCELEM                                                 
         GOTO1 REPORT              DISPLAY UNKNOWN LENGTH ELEMENT               
EXCO0100 EQU   *                                                                
         TM    SAVJCNTL,X'20'      'UNCONFIRMED' JDS ORDER?                     
         BNO   EXCO0140            NO  - CONFIRMED/CONFIRMED PREV               
         MVI   RCONCONF,X'80'      YES - SET 'UNCONFIRMED'                      
         CLI   SVCONMOD,0          ORIGINAL MOD?                                
         BNE   EXCO0140            NO                                           
*                                                                               
*   CHECK PRIOR FLAG FOR ORIGINAL ENTRY                                         
*                                                                               
         TM    SVJPFLAG,X'78'      ORIG ENTRY (NO BITS SET EXC X'80')?          
         BZ    EXCO0120            ORIGINAL ENTRY                               
         B     EXCO0140            NOT ORIG ENTRY                               
EXCO0120 EQU   *                   ORIGINAL ENTRY                               
         MVI   RCONMOD,X'FF'       SET MOD = -1                                 
         MVI   RCONSENF,X'10'                                                   
         MVI   RCONSRV,1           SET VERSION NUMBER = 1                       
         B     EXCO0280            GO INSERT ELEMENTS                           
EXCO0140 EQU   *                                                                
*                                                                               
*   OFF-LINE STATION:  TEST SPECIFIC SETUP FIRST:  CHECK FOR                    
*        X'2X' SETUP WITH 0/1 BITS OFF                                          
*                                                                               
         TM    SVJEFLAG,X'C0'      HIGH-ORDER BITS ON?                          
         BZ    EXCO0150            BOTH BITS ARE OFF                            
         B     EXCO0160            ALL ON, OR MIXED                             
EXCO0150 EQU   *                                                                
         TM    SVJEFLAG,X'20'      OFF-LINE REP CONFIRM?                        
         BNO   EXCO0160            NO                                           
*                                                                               
*   CHECK PRIOR FLAG FOR 'REP SENT LAST'                                        
*                                                                               
         CLI   SVJPFLAG,X'E0'      YES - REP SENT LAST?                         
         BNE   EXCO0160            NO                                           
         MVC   RCONMOD,SVCONMOD    USE THIS MOD #                               
         MVI   RCONCONF,X'40'      SET 'CONFIRMED NOW'                          
         MVI   RCONSENF,X'72'      SET 'LST SENT/CFD BY STATION'                
         B     EXCO0240            GO SET VERSION NUMBER                        
EXCO0160 EQU   *                                                                
*                                                                               
*   STATION SENT AT LEAST ONCE: SET STATION DATE AND TIME                       
*                                                                               
         MVC   RCONSSDT,RCONSRDT                                                
         MVC   RCONSSTI,RCONSRTI                                                
*                                                                               
         MVC   RCONMOD,SVCONMOD    NO  - USE THIS MOD                           
         OI    RCONCONF,X'20'      SET 'PREVIOUSLY CONFIRMED'                   
         TM    SVJEFLAG,X'18'      STATION TR'D?  (CONFIRMED?)                  
         BZ    EXCO0180            NO  - NOT STATION SIDE SENDING               
         TM    SAVJCNTL,X'20'      YES - MARKED 'UNCONFIRMED'?                  
         BNO   EXCO0220            NO  - SET 'STATION/CONFIRMED'                
         MVI   RCONSENF,X'72'      SET 'LST SENT/CFD BY STATION'                
*                                     BUT NOT CONFIRMED NOW                     
         B     EXCO0240            GO SET VERSION NUMBER                        
EXCO0180 EQU   *                                                                
         TM    SVJEFLAG,X'60'      REP TR'D?                                    
         BNZ   EXCO0200            YES -                                        
*                                  NO  - ONLY X'80' MUST BE SET                 
         TM    SVJPFLAG,X'18'      STATION TR'D LAST?                           
         BZ    EXCO0200            NO                                           
         OI    RCONCONF,X'80'      SET 'NOT CONFIRMED NOW'                      
         MVI   RCONSENF,X'52'      SET 'STATION SENT LAST'                      
         B     EXCO0240            GO SET VERSION NUMBER                        
EXCO0200 EQU   *                                                                
         MVC   RCONSRTI(2),=C'13'  SET REP TIME AFTER STA TIME                  
         OI    RCONCONF,X'80'      SET 'NOT CONFIRMED NOW'                      
         MVI   RCONSENF,X'B2'                                                   
         B     EXCO0240            GO SET VERSION NUMBER                        
EXCO0220 EQU   *                                                                
         MVC   RCONSSTI(2),=C'13'  SET STA TIME AFTER REP TIME                  
         OI    RCONCONF,X'40'      SET 'CONFIRMED NOW'                          
         MVI   RCONSENF,X'72'      SET 'LST SENT/CFD BY STATION'                
         B     EXCO0240            GO SET VERSION NUMBER                        
EXCO0240 EQU   *                                                                
         ZIC   RF,RCONMOD          CALCULATE VERSION NUMBER                     
         SLL   RF,1                DOUBLE MOD NUMBER                            
         LA    RF,1(RF)            ADD 1                                        
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BO    EXCO0260            YES                                          
         TM    RCONCONF,X'20'      PREVIOUSLY CONFIRMED?                        
         BZ    EXCO0260            NO                                           
         LA    RF,2(RF)            YES - INCREMENT VERSION NUMBER               
EXCO0260 EQU   *                                                                
         STC   RF,RCONSRV          INSERT VERSION #                             
*                                                                               
EXCO0280 EQU   *                                                                
         MVC   SVCONSRV,RCONSRV    SAVE FOR DISPLAY PURPOSES                    
         MVC   SVCONF,RCONCONF                                                  
         MVC   SVSENF,RCONSENF                                                  
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         GOTO1 HELOCON1            INSERT X'1F' ELEMENT                         
         GOTO1 HELOCON2            INSERT X'20' ELEMENT                         
*                                                                               
         MVI   ELTBILD1,X'77'      SAVE DEFINING INFORMATION                    
         MVI   ELTBILD1+1,7        SET LENGTH                                   
         MVC   ELTBILD1+2(1),JCONCNTL                                           
         MVC   ELTBILD1+3(1),JCONMOD                                            
         CLI   SAVJCODE,X'19'      SHORT ELEMENT INPUT?                         
         BNE   EXCO0300            NO                                           
         MVC   ELTBILD1+4(3),=X'FFFFFF'                                         
*                                  YES - SET INDICATORS                         
         B     EXCO0320                                                         
EXCO0300 EQU   *                                                                
         MVC   ELTBILD1+4(1),JCONEFLG                                           
         MVC   ELTBILD1+5(1),JCONPFLG                                           
         MVC   ELTBILD1+6(1),JCONEFF                                            
EXCO0320 EQU   *                                                                
         GOTO1 HELOCON1            INSERT INTO RECORD                           
         B     EXCO0360                                                         
*                                                                               
*   TO ACTIVATE DISPLAY, REMOVE PRECEDING BRANCH!!                              
*                                                                               
EXCO0340 EQU   *                                                                
         MVC   P+1(22),=C'CNTL/MOD/EOP/PEOP/EFF='                               
         GOTO1 HEXOUT,DMCB,JCONCNTL,P+23,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,JCONMOD,P+27,1,=C'TOG'                               
***>>    GOTO1 HEXOUT,DMCB,JCONMVER,P+31,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,JCONEFLG,P+31,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,JCONPFLG,P+35,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,JCONEFF,P+39,1,=C'TOG'                               
         GOTO1 HEXOUT,DMCB,JCONKCON,P+45,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         MVC   P+1(18),=C'MOD/VER/CONF/SENF='                                   
         GOTO1 HEXOUT,DMCB,RCONMOD,P+23,1,=C'TOG'                               
         GOTO1 HEXOUT,DMCB,SVCONSRV,P+27,1,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,SVCONF,P+31,1,=C'TOG'                                
         GOTO1 HEXOUT,DMCB,SVSENF,P+35,1,=C'TOG'                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
EXCO0360 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CALCLENS:  DEVELOP THE LENGTH SECTION OF THE SAR ELEMENT                    
*        P2  =  A(RSARXRFR FIELD)                                               
*                                                                               
CALCLENS NMOD1 0,*CLEN*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R7,4(R1)            RESET A(RSARXRFR FIELD)                      
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,1(R3)            SET TO 1ST LENGTH                            
         CLI   JDSLEN+3,6          SIX LGTHS = MAX                              
         BNH   CLEN0280                                                         
         MVI   JDSLEN+3,6          FORCE MAX OF SIX                             
CLEN0280 EQU   *                                                                
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
CLEN0300 EQU   *                                                                
         LA    R5,LENTABLE         SET A(START OF TABLE)                        
         CLI   0(R3),0             FIELD EMPTY?                                 
         BE    CLEN0400            YES - FINISHED                               
CLEN0320 EQU   *                                                                
         CLI   0(R5),0             END OF TABLE REACHED?                        
         BE    CLEN0360            YES - SKIP THIS ENTRY                        
         CLC   0(1,R5),0(R3)       TABLE VS LENGTH:  EQUAL?                     
         BE    CLEN0340            YES                                          
         LA    R5,1(R5)            NO  - BUMP TO NEXT TABLE ENTRY               
         B     CLEN0320            GO BACK FOR NEXT                             
CLEN0340 EQU   *                                                                
         MVC   1(1,R7),1(R5)       INSERT CONVERT LEN FROM TABLE                
         LA    R7,2(R7)            A(NEXT DDS LGTH FIELD)                       
CLEN0360 EQU   *                                                                
         LA    R3,1(R3)            A(NEXT JDS LGTH FIELD)                       
         BCT   R0,CLEN0300         GO BACK FOR NEXT                             
CLEN0400 EQU   *                                                                
         XIT1                                                                   
*                                                                               
LENTABLE DC    C'1',X'0A'          10 SECONDS                                   
LLNTABLE EQU   *-LENTABLE                                                       
         DC    C'5',X'0F'          15 SECONDS                                   
         DC    C'3',X'1E'          30 SECONDS                                   
         DC    C'4',X'2D'          45 SECONDS                                   
         DC    C'6',X'3C'          60 SECONDS                                   
         DC    C'9',X'5A'          90 SECONDS                                   
         DC    C'A',X'78'          120 SECONDS                                  
         DC    C'B',X'85'          5 MINUTES                                    
         DC    C'C',X'8F'          15 MINUTES                                   
         DC    C'D',X'9E'          30 MINUTES                                   
         DC    C'E',X'BC'          60 MINUTES                                   
         DC    C'F',X'BC'          3 HOURS: CAN'T SET-> 60 MINUTES              
         DC    C'G',X'DA'          90 MINUTES                                   
****>>   DC    C'S',X'DA'          S30 ???                                      
         DC    C'0',X'09'          09 SECONDS                                   
         DC    C'H',X'05'          05 SECONDS                                   
         DC    X'0000'             DELIMITER                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  MGCOMM:  INSERT MG= INSTRUCTIONS INTO THE BUY RECORD BEING    *              
*           GENERATED.  IF A COMMENT RECORD DOES NOT EXIST,      *              
*           CREATE IT.  IF ONE OR MORE COMMENT RECORDS EXIST,    *              
*           REMOVE THEM FROM THE RECORD, DELETE THEM, INSERT THE *              
*           MAKEGOOD INSTRUCTIONS INTO THE FIRST, THEN REINSERT  *              
*           THE COMMENTS                                         *              
******************************************************************              
*                                                                               
         DS    0F                                                               
MGCOMM   NMOD1 0,*MGCM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   1.   SET UP THE MAKEGOOD INSTRUCTION                                        
*                                                                               
         XC    MGWORK,MGWORK                                                    
         MVC   MGWORK(3),=C'MG='                                                
         GOTO1 DATCON,DMCB,(3,MGDATE),(7,MGWORK+3)                              
*                                  INSERT DATE AS MMMDD                         
         LA    R3,MGWORK+7         SET A(SEPARATOR)                             
         CLI   MGWORK+7,0          'DAY' > 9?                                   
         BE    MGCO0020            NO                                           
         LA    R3,1(R3)            YES - ADD 1 TO ADDRESS                       
MGCO0020 EQU   *                                                                
         MVI   0(R3),C'-'          INSERT SEPARATOR                             
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
         EDIT  MGLINE,(2,(R3)),ALIGN=LEFT                                       
         AR    R3,R0               INCREMENT BY LENGTH                          
         CLI   MGSPOTS,1           ONE SPOT, OR MORE?                           
         BNH   MGCO0040            1                                            
         MVI   0(R3),C'('                                                       
         LA    R3,1(R3)            BUMP TO NEXT POSITION                        
         EDIT  MGSPOTS,(2,(R3)),ALIGN=LEFT                                      
         AR    R3,R0               INCREMENT BY LENGTH                          
         MVI   0(R3),C')'                                                       
MGCO0040 EQU   *                                                                
*                                                                               
*   1A.  COUNT LENGTH OF MG= INSTRUCTION TO CALCULATE LENGTH FIELD              
*                                                                               
         LA    R2,MGWORK                                                        
         LA    R7,0                CLEAR COUNTER                                
         LA    R0,15               LOOP CONTROL                                 
MGCO0042 EQU   *                                                                
         CLI   0(R2),0             END OF DATA?                                 
         BE    MGCO0044            YES                                          
         LA    R7,1(R7)            NO  - ADD 1 TO COUNT                         
         LA    R2,1(R2)            BUMP TO NEXT POSITION                        
         BCT   R0,MGCO0042         GO BACK FOR NEXT                             
MGCO0044 EQU   *                                                                
         LA    R7,2(R7)            INCREMENT FOR CONTROL                        
         STC   R7,MGWKCNTL+1       INSERT LENGTH INTO ELEMENT                   
*                                                                               
*   2.   RETRIEVE EXISTING BUY COMMENTS, IF ANY                                 
*                                                                               
         L     R2,MGCMAREA                                                      
         XC    0(240,R2),0(R2)     CLEAR THE AREA                               
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,4            RETRIEVE ALL BUY COMMENTS                    
         BAS   RE,GETEL            RETRIEVE THE FIRST                           
         B     MGCO0080                                                         
MGCO0060 EQU   *                                                                
         BAS   RE,NEXTEL           RETRIEVE THE NEXT ONE                        
MGCO0080 EQU   *                                                                
         BNZ   MGCO0120            NOT FOUND                                    
         ZIC   RF,1(R6)            MOVE ELEMENT TO STORAGE                      
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,MGCO0900         MOVE BY LENGTH                               
         MVI   0(R6),X'FF'         SET ELEMENT CODE FOR DELETE                  
         LA    R2,80(R2)           BUMP FOR NEXT ENTRY                          
         B     MGCO0060                                                         
MGCO0120 EQU   *                                                                
         L     R2,MGCMAREA         RESET A(COMMENT ELEMENTS)                    
         LR    R7,R2               SET A(1ST COMMENT ELEMENT)                   
*                                                                               
*   TEST                                                                        
****     CLC   JBUYKCON,=X'97955980'                                            
*        CLC   JBUYKCON,=X'97932451'                                            
*        BNE   MGTEST01                                                         
*        MVC   P+1(10),=C'MGCOMMENTS'                                           
*        GOTO1 REPORT                                                           
*        MVC   P+1(80),0(R2)                                                    
*        GOTO1 REPORT                                                           
*        MVC   P+1(80),80(R2)                                                   
*        GOTO1 REPORT                                                           
MGTEST01 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         OC    0(60,R2),0(R2)      ANY DATA THERE?                              
         BNZ   MGCO0200            YES - MASSAGE IT                             
*                                                                               
*   3.   NO COMMENTS:  INSERT ONE BASED ON MG= INSTRUCTION ONLY                 
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,MGWKCNTL,0             
         XC    MGWORK,MGWORK       CLEAR OUT THE AREA                           
         B     MGCO0800            DONE - EXIT                                  
*                                                                               
*   4.   COMMENTS EXIST:  INSERT MG= INSTRUCTION INTO FIRST ELEMENT             
*           DELETE OLD X'04' ELEMENTS (SET TO X'FF')                            
*           INSERT NEW X'04' ELEMENTS FROM STORAGE                              
*                                                                               
MGCO0200 EQU   *                                                                
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'FF',RBUYREC),0,0            
*                                  DELETE THE OLD COMMENT ELEMENTS              
         ZIC   RF,1(R2)            CALC LENGTH OF 1ST COMMENT                   
         SH    RF,=H'3'            SUBTRACT CNTL(2) + 1(EX)                     
         EX    RF,MGCO0910         MOVE COMMENT TO STORAGE                      
         XC    2(75,R2),2(R2)      CLEAR OLD COMMENT                            
         MVC   0(25,R2),MGWKCNTL   INSERT MG= INSTRUCTION                       
*                                                                               
*   TEST                                                                        
****     CLC   JBUYKCON,=X'97955980'                                            
*        CLC   JBUYKCON,=X'97932451'                                            
*        BNE   MGTEST02                                                         
*        ST    RF,DUB                                                           
*        MVC   P+1(10),=C'MGWKCNTL  '                                           
*        MVC   P+11(25),MGWKCNTL                                                
*        GOTO1 REPORT                                                           
*        MVC   P+1(10),=C'1ST COMMT:'                                           
*        MVC   P+11(25),0(R2)                                                   
*        GOTO1 REPORT                                                           
*        MVC   P+1(10),=C'SAV COMMT:'                                           
*        MVC   P+11(25),160(R2)                                                 
*        GOTO1 REPORT                                                           
*        L     RF,DUB                                                           
MGTEST02 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         ZIC   RE,MGWKCNTL+1       CALC LEN TO REINSERT COMMENT                 
*                                                                               
         LR    R6,RE               SAVE LENGTH OF MG= INST + SPACES             
         LA    R6,2(R6)               ADDED FOR PADDING                         
         LNR   R6,R6               NEGATE REGISTER                              
         A     R6,=F'62'           ADD MAXIMUM SIZE                             
*                                     R6 = ROOM LEFT FOR COMMENT                
*                                                                               
         AR    R7,RE               BUMP TO END OF ELEMENT                       
         MVC   0(2,R7),SPACES      INSERT 2 SPACES                              
         LA    R7,2(R7)            BYPASS NEW SPACES                            
         CR    RF,R6               COMMT LEN VS ROOM LEFT                       
         BNH   MGCO0240            STILL HAVE ROOM                              
         LR    RF,R6               COMMENT TOO LONG:  ONLY USE                  
*                                     WHAT SPACE IS LEFT                        
         BCTR  RF,0                SET FOR EX                                   
MGCO0240 EQU   *                                                                
         EX    RF,MGCO0920         MOVE COMMENT BACK TO ELEMENT                 
         ST    RF,COMMLEN                                                       
*                                                                               
*   TEST                                                                        
****     CLC   JBUYKCON,=X'97955980'                                            
*        CLC   JBUYKCON,=X'97932451'                                            
*        BNE   MGTEST03                                                         
*        ST    RF,DUB                                                           
*        MVC   P+1(10),=C'REBUILT:  '                                           
*        MVC   P+11(80),0(R2)                                                   
*        GOTO1 REPORT                                                           
*        MVC   P+1(10),=C'R6=       '                                           
*        EDIT  (R6),(8,P+12)                                                    
*        GOTO1 REPORT                                                           
*        L     RF,DUB                                                           
MGTEST03 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         ZIC   RE,1(R2)            GET LENGTH OF ELEMENT                        
         LA    RE,2(RE)            ADD FOR TWO NEW SPACES                       
         A     RE,COMMLEN          ADD FOR COMMENT BACK IN                      
         STC   RE,1(R2)            REINSERT LENGTH                              
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,0(R2),        X        
               =C'ADD=CODE'                                                     
*                                  INSERT ELEMENT FROM STORAGE                  
         LA    R2,80(R2)           BUMP TO NEXT STORAGE                         
         OC    0(60,R2),0(R2)      ANYTHING IN NEXT STORAGE?                    
         BZ    MGCO0800            NO  - FINISHED                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,0(R2),        X        
               =C'ADD=CODE'                                                     
*                                  INSERT ELEMENT FROM STORAGE                  
MGCO0800 EQU   *                                                                
         XIT1                                                                   
*                                                                               
MGCO0900 MVC   0(0,R2),0(R6)       SAVE ELEMENT                                 
MGCO0910 MVC   160(0,R2),2(R2)     SAVE COMMENT IN STORAGE                      
MGCO0920 MVC   0(0,R7),160(R2)     MOVE COMMENT BACK TO ELEMENT                 
MGWKCNTL DC    X'0400'             CONTROL FOR ELEMENT                          
MGWORK   DS    CL20                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  AGYRPROC:  PROCESS THE AGENCY RECORD.  CHECK TO SEE IF AGENCY *              
*             /AGENCY OFF CODE IS IN 'MISSING AGENCY' TABLE.  IF *              
*             FOUND, GENERATE AN AGENCY RECORD FOR IT.           *              
*                                                                *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
         DS    0F                                                               
AGYRPROC NMOD1 0,*AGYR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   =C'PT',JAGYKREP     'PT' AGENCY?                                 
         BNE   AGYR0200            NO  - SKIP THIS RECORD                       
*                                                                               
         GOTO1 =A(DISPIPUT),DMCB,(RC),RR=Y                                      
*                                  DISPLAY AGENCY INPUT RECORDS                 
         L     R2,AAG2AREA         A(MISSING AGENCY AREA)                       
         L     R6,AG2CTR           SET VALUE OF COUNTER                         
         GOTO1 =V(BINSRCH),DMCB,JAGYKAGY,(R2),(R6),6,(0,6),20000,      X        
               RR=RELO                                                          
*                                  FIND AGY/AGY OFF.  IF NOT FOUND,             
*                                     INSERT NEW CODE INTO TABLE                
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   AGYR0200            NO  - NOT 'MISSING' - NO O/P                 
*                                                                               
*                                  FOUND:  MISSING FROM FILE                    
*                                                                               
         L     RF,AGYCTR2          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR2                                                       
*                                                                               
*   TEST                                                                        
*        EDIT  AGYCTR2,(6,P+1)                                                  
*        MVC   P+10(15),=C'AGENCY MISSING:'                                     
*        MVC   P+30(6),JAGYKAGY                                                 
*        MVI   P+36,C'/'                                                        
*        ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
*        MVC   P+37(6),0(R2)                                                    
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         XCEFL RECORD,1008         CLEAR THE RECORD BUILD AREA                  
         MVC   RAGYLEN(2),=X'009C'                                              
         MVI   RAGYKTYP,X'0A'      INSERT RECORD TYPE                           
         MVC   RAGYKAGY(6),JAGYKAGY                                             
*                                  INSERT AGENCY/AGENCY OFFICE                  
         MVC   RAGYKREP,PETRYREP   INSERT REP CODE                              
*                                                                               
         MVC   RAGYELEM(2),=X'017A'                                             
*                                  INSERT ELT CODE/LENGTH                       
         MVC   RAGYNAM1,JAGYNAM1   INSERT NAME 1                                
         MVC   RAGYNAM2,JAGYNAM2   INSERT NAME 2                                
         MVC   RAGYADD1,JAGYADD1   INSERT ADDRESS 1                             
         MVC   RAGYADD2,JAGYADD2   INSERT ADDRESS 2                             
*                                  ADDRESSES MAY BE TRUNCATED                   
         OI    RAGYFLAG,X'80'      SET 'PETRY: ADDED BY CONVERT'                
*                                                                               
         GOTO1 PUTRECAG,DMCB,(RC),RECORD                                        
         CLI   QUESTOR+6,C'A'      DISPLAY AGY TBL + NEW O/P?                   
         BE    AGYR0020            YES                                          
         CLI   QUESTOR+6,C'B'      DISPLAY NEW O/P?                             
         BNE   AGYR0040            NO                                           
AGYR0020 EQU   *                                                                
         CLC   AGYCTR2,=F'25'      DISPLAY FIRST N ONLY                         
         BH    AGYR0040                                                         
         GOTO1 DISPPTAG,DMCB,(RC),0                                             
AGYR0040 EQU   *                                                                
         XC    RAGYELEM(200),RAGYELEM                                           
*                                  CLEAR LOWER PART OF RECORD                   
         MVC   RAGYLEN(2),=X'0056'                                              
         MVI   RAGYKTYP,X'1A'      INSERT NEW RECORD TYPE                       
         MVC   RAGYELEM(2),=X'1034'                                             
*                                  INSERT ELT CODE/LENGTH                       
         MVC   ELTBILD1(2),=X'2046'                                             
*                                  ELT CODE = 20, LENGTH = 70                   
         MVC   ELTBILD1+2(34),JAGYADD1                                          
*                                  INSERT 1ST LINE OF ADDRESS                   
         MVC   ELTBILD1+36(34),JAGYADD2                                         
*                                  INSERT 2ND LINE OF ADDRESS                   
         GOTO1 HELOAGY1            INSERT ELEMENT W/ERASE                       
*                                                                               
         GOTO1 PUTRECAG,DMCB,(RC),RECORD                                        
         L     RF,AGYCTR3                                                       
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR3                                                       
         CLI   QUESTOR+6,C'A'      DISPLAY AGY TBL + NEW O/P?                   
         BE    AGYR0160            YES                                          
         CLI   QUESTOR+6,C'B'      DISPLAY NEW O/P?                             
         BNE   AGYR0200            NO                                           
AGYR0160 EQU   *                                                                
         CLC   AGYCTR3,=F'25'      DISPLAY FIRST N ONLY                         
         BH    AGYR0200                                                         
         GOTO1 DISPPTAG,DMCB,(RC),1                                             
AGYR0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
HELOAGY1 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RAGY2REC,ELTBILD1,0            
         XC    ELTBILD1,ELTBILD1                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
*                                                                               
*   DISPAGYS:  DISPLAY THE 'MISSING AGENCY TABLE'                               
*                                                                               
DISPAGYS NTR1                                                                   
         L     R2,AAG2AREA         SET A(1ST ENTRY IN MISSING)                  
         LA    R2,6(R2)            SKIP FIRST ENTRY (ZEROS)                     
         SR    R3,R3                                                            
DAGY0020 EQU   *                                                                
         CLC   0(2,R2),=X'FFFF'    END OF TABLE?                                
         BE    DAGY0060            YES                                          
         LA    R3,1(R3)            INCREMENT COUNTER                            
         MVC   P+20(6),0(R2)       MOVE AGENCY CODE TO PRINT                    
         EDIT  (R3),(6,P+1)        DISPLAY COUNT                                
         GOTO1 REPORT                                                           
         LA    R2,6(R2)            BUMP TO NEXT ENTRY                           
         B     DAGY0020            GO BACK FOR NEXT                             
DAGY0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECAG: GENERATE OUTFILE ENTRIES FOR AGENCY RECORDS         *              
******************************************************************              
*                                                                               
PUTRECAG NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            SET A(RECORD BEING OUTPUT)                   
*                                                                               
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           PUT IT BACK                                  
*                                                                               
*        MVC   P+1(07),=C'PUTREC:'                                              
*        MVC   P+20(27),REC                                                     
*        EDIT  PUTCTR,(5,P+10)                                                  
*        GOTO1 REPORT                                                           
*                                                                               
         LA    RF,REC                                                           
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),(R2)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RCONLEN-RCONKEY(R2)                                     
*                                  INSERT LENGTH, USING COMMON RCONLEN          
*                                                                               
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
******************************************************************              
*  DISPPTAG:  DISPLAY AGENCY RECORD JUST 'PUT' TO OUTPUT.        *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPTAG NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            SAVE FLAG                                    
                                                                                
         MVC   P+1(14),=C'AGENCY  OUTPUT'                                       
         LTR   R2,R2               TEST FLAG                                    
         BZ    DITG0020            AGENCY RECORD                                
         MVC   P+1(14),=C'AGENCY2 OUTPUT'                                       
DITG0020 EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*  CONVAGY:  FIND AGENCY IN CONVERSION LIST.  USE CONVERSION VALUE AS           
*        OUTPUT.  IF NOT FOUND, USE VALUE FROM RECORD, AND INSERT               
*        THIS VALUE INTO 'MISSING AGENCY CODE' TABLE.  AT END, RECORDS          
*        FOR MISSING AGENCIES WILL BE GENERATED ON A SECOND PASS OF             
*        THE INPUT FILE.                                                        
*                                                                               
         DS    0F                                                               
CONVAGY  NMOD1 0,*CAGY*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,AAGYAREA         SET A(AGENCY TABLE)                          
         L     R4,AGYCTRIN         CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,JCONKAGY,(R2),(R4),12,6,10000,RR=RELO           
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    COVA0020            YES                                          
*                                                                               
         MVC   RCONKAGY(6),JCONKAGY                                             
*                                  NOT IN TABLE:  USE JDS VALUES                
         L     R2,AAG2AREA         A(AGENCY 2 TABLE AREA)                       
         L     R6,AG2CTR           SET VALUE OF COUNTER                         
         GOTO1 =V(BINSRCH),DMCB,(1,JCONKAGY),(R2),(R6),6,(0,6),20000,  X        
               RR=RELO                                                          
*                                  FIND DEV SP CODE.  IF NOT FOUND,             
*                                     INSERT NEW CODE INTO TABLE                
         CLI   DMCB,0              RECORD FOUND?                                
         BE    COVA0100            YES                                          
         MVC   AG2CTR,DMCB+8       NO  - SAVE NEW COUNT                         
         CLC   AG2CTR,=F'100'      ONLY DISPLAY 1ST N MISSING                   
         BH    COVA0005                                                         
         MVC   P+1(20),=C'AGENCY NOT IN TABLE:'                                 
         MVC   P+21(6),JCONKAGY                                                 
         EDIT  AG2CTR,(6,P+30)                                                  
         GOTO1 REPORT                                                           
COVA0005 EQU   *                                                                
         B     COVA0100                                                         
COVA0020 EQU   *                                                                
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
         MVC   RCONKAGY(6),6(R2)   INSERT AGENCY/AGY OFF FROM TABLE             
COVA0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  MISTSTAT:  ENTER STATION MISSING INTO TABLE, FROM WHICH TO GENERATE          
*        'DUMMY' STATION RECORDS AT END                                         
*                                                                               
         DS    0F                                                               
MISTSTAT NMOD1 0,*NSTA*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         L     RF,AMISAREA         ADD STATION TO TABLE                         
MIST0020 EQU   *                                                                
         OC    0(4,RF),0(RF)       ANYTHING IN FIELD?                           
         BZ    MIST0060            NO  - ADD STATION HERE                       
         CLC   0(4,RF),JCONKSTA    TABLE VS STATION                             
         BE    MIST0080            IN TABLE:  EXIT                              
         LA    RF,6(RF)            BUMP TO NEXT ENTRY                           
         B     MIST0020            GO BACK FOR NEXT                             
MIST0060 EQU   *                                                                
         MVC   0(4,RF),JCONKSTA    LOAD ENTRY TO TABLE                          
         MVC   4(2,RF),RCONKREP    LOAD REP CODE TO TABLE                       
         L     RF,MISSTAT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,MISSTAT          INCREMENT COUNTER                            
MIST0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*                                                                               
*  COMPELEM:  GENERATE THE COMPETITIVE ELEMENT FROM THE JDS                     
*        X'F0' ELEMENT                                                          
*        THERE ARE NO STATIONS IN THIS ELEMENT.  STATION TABLE                  
*        WITH COMPETITIVES WILL BE BINARY SEARCHED TO INSERT                    
*        STATION COMPETITIVE CALL LETTERS.  DATA IS SET UP:                     
*            1.  BASE STATION                                                   
*            2.  COMPETITIVE STATIONS IN EXACT ORDER (MAX EIGHT)                
*                                                                               
         DS    0F                                                               
COMPELEM NMOD1 0,*COMP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   SPLFLAG,C'N'        SET 'SPL FLAG' TO NO                         
         L     R2,ACMPAREA         SET A(COMPETITIVE TABLE)                     
         L     R4,CMPCTR           CURRENT # OF TABLE ENTRIES                   
         XC    COMPCTR,COMPCTR     CLEAR COMP STATIONS COUNTER                  
         MVC   STAWORK(4),JCONKSTA                                              
         MVC   STAWORK+4(2),JCONKGRP                                            
*                                                                               
*   SEARCH TABLE FOR STATION + GROUP/SUBGROUP                                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,STAWORK,(R2),(R4),63,6,2000,RR=RELO             
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    COMP0010            YES                                          
         MVC   P+1(26),=C'STATION NOT IN COMP TABLE:'                           
         MVC   P+28(4),JCONKSTA                                                 
         GOTO1 REPORT                                                           
*                                                                               
*   TEST                                                                        
*        L     R2,ACMPAREA                                                      
*        SR    R1,R1                                                            
*        LA    R3,2000                                                          
*        MVC   P+1(09),=C'COMP DUMP'                                            
*        GOTO1 REPORT                                                           
LOOP0010 EQU   *                                                                
*        LA    R1,1(R1)                                                         
*        EDIT (R1),(5,P+1)                                                      
*        MVC   P+10(49),0(R2)                                                   
*        GOTO1 REPORT                                                           
*        LA    R2,63(R2)                                                        
*        BCT   R3,LOOP0010                                                      
*        DC    H'0'                DUMP AT END                                  
*   TEST END                                                                    
*                                                                               
         SR    R2,R2               CLEAR R2:  NO ADDRESS FOUND                  
         B     COMP0012                                                         
COMP0010 EQU   *                                                                
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         ST    R2,SAVER2           SAVE A(1ST STATION IN LIST)                  
*                                                                               
*   TEST                                                                        
*        MVC   P+1(11),=C'COMP STNS:='                                          
*        MVC   P+12(63),0(R2)                                                   
*        GOTO1 REPORT                                                           
*  TEST END                                                                     
*                                                                               
COMP0012 EQU   *                                                                
         MVI   JDSELT,X'F0'        GET COMPETITIVE ELEMENT                      
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   COMP0400            NOT FOUND                                    
         MVI   SPLFLAG,C'Y'        SET 'SPL FLAG' TO YES                        
         L     R3,JDSADDR          SET A(JDS ELT)                               
         L     R4,JDSLEN           SET L(JDS DATA)                              
         USING JCONCDEL,R3                                                      
         MVI   ELTBILD1,6          INSERT ELEMENT ID                            
         GOTO1 JDSDATE,DMCB,JCONCDDT,WORK,(2,0)                                 
*                                  CONVERT JDS 2-BYTE TO DDS 3-BYTE             
         GOTO1 DATCON,DMCB,(3,WORK),(2,ELTBILD1+2)                              
*                                  CONVERT TO 2-BYTE COMPRESSED                 
         GOTO1 DATCON,DMCB,(3,WORK),(3,ELTBILD1+5)                              
*                                  CONVERT TO 3-BYTE ENTRY DATE                 
         MVI   ELTBILD1+4,X'0C'    TURN ON %, COMP DATE                         
         TM    JCONCDTO,X'80'      ESTIMATED?                                   
         BNO   COMP0020            NO  - NOT ESTIMATED                          
         OI    ELTBILD1+4,X'01'    YES - TURN ON ESTIMATED                      
COMP0020 EQU   *                                                                
         SH    R4,=H'6'            CALC # OF PERCENTS                           
         LTR   R4,R4               ANY VALUE?                                   
         BZ    COMP0400            NO  - EXIT ROUTINE                           
         LA    RF,JCONCDPC         SET A(1ST PERCENT)                           
         LA    RE,ELTBILD1+9       SET A(1ST RECEIVING FIELD)                   
COMP0040 EQU   *                                                                
*                                                                               
         ZIC   R7,0(RF)            RETRIEVE PERCENT                             
         LTR   R7,R7               ANY VALUE?                                   
         BNZ   COMP0050            YES - INSERT DATA                            
         LTR   R2,R2               ANY VALUE IN R2?                             
         BZ    COMP0070            NO  - DON'T BUMP STATION ADDRESS             
         LA    R2,7(R2)            YES - BUMP TO NEXT STATION CALLS             
         B     COMP0070                                                         
COMP0050 EQU   *                                                                
         L     R6,COMPCTR          INCREMENT COMP COUNTER                       
         LA    R6,1(R6)                                                         
         ST    R6,COMPCTR          SAVE COUNTER                                 
         SR    R6,R6                                                            
         M     R6,=F'100'          DECIMAL ALIGN TO 2 PLACES                    
         STCM  R7,15,5(RE)         INSERT INTO ELEMENT                          
         LTR   R2,R2               ANY VALUE IN R2?                             
         BZ    COMP0060            NO  - DON'T INSERT CALL LETTERS              
         MVC   0(4,RE),0(R2)       INSERT STATION CALL LETTERS                  
         MVI   4(RE),C' '          INSERT MEDIA                                 
         LA    R2,7(R2)            BUMP TO NEXT STATION                         
COMP0060 EQU   *                                                                
         LA    RE,9(RE)            BUMP TO NEXT DDS STATION                     
COMP0070 EQU   *                                                                
         LA    RF,1(RF)            BUMP TO NEXT JDS PERCENT                     
                                                                                
         BCT   R4,COMP0040         GO BACK FOR NEXT                             
         CLI   COMPCTR+3,0         ANY STATIONS IN COMP?                        
         BNZ   COMP0090            YES                                          
         MVI   COMPCTR+3,1         SET COUNTER TO 1                             
         MVC   DUB,=F'10000'       INSERT 100.00% INTO FIRST STATION            
         MVC   5(4,RE),DUB         INSERT INTO ELEMENT                          
         MVI   4(RE),C' '          INSERT MEDIA                                 
         LTR   R2,R2               ANY VALUE IN R2?                             
         BZ    COMP0090            NO  - DON'T INSERT CALL LETTERS              
         L     R2,SAVER2           RESET A(1ST STATION IN LIST)                 
         MVC   0(4,RE),0(R2)       INSERT REPD STATION CALL LETTERS             
COMP0090 EQU   *                                                                
         L     R1,COMPCTR          CALCULATE LENGTH OF ELEMENT                  
         SR    RE,RE                                                            
         LA    RF,9                                                             
         MR    RE,R1               CALCULATE LENGTH OF STA %S                   
         A     RF,=F'9'            ADD LENGTH OF CONTROL, ETC                   
         STC   RF,ELTBILD1+1       INSERT LENGTH INTO ELEMENT                   
         MVC   ELTBILD1+8(1),COMPCTR+3                                          
*                                  INSERT NUMBER OF MINI-ELTS                   
*   TEST                                                                        
*        MVC   P+1(14),=C'ELTBILD: COMP='                                       
*        MVC   P+15(72),ELTBILD1                                                
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 HELOCON1            INSERT ELEMENT W/ERASE                       
         GOTO1 COMPCOMT,DMCB,(RC)                                               
*                                  GENERATE THE COMPET COMMENT ELT              
COMP0400 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*   COMPCOMT:  RETRIEVE JDS 03 ELEMENT, CONSTRUCT UP TO FOUR                    
*        COMPETITIVE COMMENT ELEMENTS                                           
*                                                                               
COMPCOMT NTR1  0,*COMT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R0,4                SET LOOP CONTROL: 4 MAX                      
         MVI   JDSELT,X'03'        GET COMP COMMENT ELT                         
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BZ    COCO0040            FOUND:  ADD COMMENTS                         
         CLI   COMMSKIP,C'Y'       SKIP COMMENT LOOKUP?                         
         BE    COCO0100            YES                                          
         BAS   RE,CMPRECD          NOT FOUND: COMPETITIVE COMMT RECD?           
         B     COCO0100            EXIT                                         
COCO0020 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
COCO0040 EQU   *                                                                
         BNZ   COCO0100            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         CLC   JDSLEN,=F'60'       COMMENT EXCEEDS 60 CHARS?                    
         BNH   COCO0060            NO                                           
         LA    RE,60               YES - SET MAX TO 60                          
         ST    RE,JDSLEN           YES                                          
COCO0060 EQU   *                                                                
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,COCO0080         MOVE PRODUCT NAME BY LENGTH                  
         MVI   ELTBILD1,X'07'      INSERT CONTRACT COMMENT ELT CODE             
         LA    RE,3(RE)            CALCULATE L(ELEMENT)                         
         STC   RE,ELTBILD1+1       INSERT INTO ELEMENT                          
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD1,     X        
               =C'ADD=CODE'                                                     
*                                  ADD BY ELEMENT CODE ONLY                     
         XC    ELTBILD1,ELTBILD1                                                
         BCT   R0,COCO0020         GO BACK AND CHECK IF FOUND                   
         B     COCO0100                                                         
COCO0080 EQU   *                                                                
         MVC   ELTBILD1+2(0),2(RF) INSERT COMMENT BY LENGTH                     
COCO0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CMPRECD:  CHECK FOR A COMPETITIVE COMMENT RECORD ON THE FILE FOR            
*        THIS CONTRACT NUMBER.  IF FOUND, ADD ITS COMMENTS TO THE               
*        RECORD AS TYPE 07 ELEMENTS.                                            
*        COMPETITIVE COMMENT RECORD IS TYPE ZERO (0).                           
*                                                                               
CMPRECD  NTR1                                                                   
         L     R6,ARECORD5         SET A(IO AREA)                               
         ST    R6,AIOAREA                                                       
         USING RXXXREC,R6                                                       
         MVI   COMTFLAG,C'N'       TURN OFF 'COMMENT EXISTS' FLAG               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'47'           INSET RECORD TYPE                            
         MVC   KEY+20(2),PETRYREP  INSERT REP CODE FROM CARD                    
         MVC   KEY+22(4),RCONKCON  INSERT CONTRACT NUMBER                       
         GOTO1 HIGH                READ KEY                                     
*                                                                               
*   TEST                                                                        
*        MVC   P+1(27),KEY                                                      
*        MVC   P+30(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BNE   CMPR0060            NO                                           
         GOTO1 GREC                                                             
         LA    R5,RXXXELEM         SET A(DESCRIPTIVE ELEMENT)                   
CMPR0020 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    CMPR0060            YES - FINISHED                               
         CLI   0(R5),3             COMMENT?                                     
         BNE   CMPR0040            NO                                           
         MVI   COMTFLAG,C'Y'                                                    
         MVI   0(R5),7             YES - CHANGE ITS IDENTIFIER                  
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,(R5),         X        
               =C'ADD=CODE'                                                     
*                                  ADD FROM COMMENT RECORD                      
CMPR0040 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     CMPR0020            GO BACK FOR NEXT                             
CMPR0060 EQU   *                                                                
         CLI   COMTFLAG,C'Y'       COMMENT EXISTS?                              
         BNE   CMPR0080            NO                                           
         L     RF,CMPCOMCT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,CMPCOMCT                                                      
         CLC   CMPCOMCT,=F'00'     DISPLAY FIRST N CONTRACTS                    
         BH    CMPR0080                                                         
         MVC   P+1(07),=C'CMPCOM:'                                              
         GOTO1 REPORT                                                           
         GOTO1 =A(DISPCOMS),DMCB,(RC),0,RR=Y                                    
CMPR0080 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*  DISPCOMS: DISPLAY RECORD WITH COMMENTS                        *              
*                                                                *              
******************************************************************              
*                                                                               
DISPCOMS NMOD1 0,*DCOM*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            SET INDICATOR                                
         LA    R4,RCONREC          A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RCONLEN,2        GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              INSERT A BLANK LINE                          
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SARCOMMS: CHECK FOR AN ACTIVITY   COMMENT RECORD ON THE FILE FOR            
*        THIS CONTRACT NUMBER.  IF FOUND, ADD ITS COMMENTS TO THE               
*        RECORD AS TYPE 11 ELEMENTS.                                            
*        SAR ACTIVITY RECORD TYPE IS ZERO (0).                                  
*                                                                               
SARCOMMS NMOD1 0,*SRCM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   COMTFLAG,C'N'       TURN OFF 'COMMENT EXISTS' FLAG               
         L     R6,ARECORD5         SET A(IO AREA)                               
         ST    R6,AIOAREA                                                       
         USING RXXXREC,R6                                                       
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'47'           INSET RECORD TYPE                            
         MVC   KEY+20(2),PETRYREP  INSERT REP CODE FROM CARD                    
         MVC   KEY+22(4),RCONKCON  INSERT CONTRACT NUMBER                       
         GOTO1 HIGH                READ KEY                                     
         CLC   KEY(27),KEYSAVE     FOUND?                                       
         BNE   SRCM0100            NO                                           
         GOTO1 GREC                                                             
         LA    R5,RXXXELEM         SET A(DESCRIPTIVE ELEMENT)                   
SRCM0020 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    SRCM0100            YES - FINISHED                               
         CLI   0(R5),3             COMMENT?                                     
         BNE   SRCM0040            NO                                           
         MVI   COMTFLAG,C'Y'       TURN ON 'COMMENT EXISTS' FLAG                
         MVI   0(R5),11            YES - CHANGE ITS IDENTIFIER                  
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,(R5),         X        
               =C'ADD=CODE'                                                     
*                                  ADD FROM COMMENT RECORD                      
SRCM0040 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     SRCM0020            GO BACK FOR NEXT                             
SRCM0100 EQU   *                                                                
         CLI   COMTFLAG,C'Y'       COMMENT EXISTS?                              
         BNE   SRCM0120            NO                                           
         L     RF,SARCOMCT                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SARCOMCT                                                      
         CLC   SARCOMCT,=F'25'     DISPLAY FIRST N CONTRACTS                    
         BH    SRCM0120                                                         
         MVC   P+1(07),=C'SARCOM:'                                              
         GOTO1 REPORT                                                           
         GOTO1 =A(DISPCOMS),DMCB,(RC),1,RR=Y                                    
SRCM0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  GETTPREC:  UNBLOCK PETRY RECORD INPUT.  EXPAND KEY.  MOVE     *              
*             RECORD TO RECORD2, WHERE IT WILL BE PROCESSED.     *              
*             UPON EOF, SET CC = ZERO, WHICH WILL END JOB        *              
*             ELSE SET CC NOT ZERO.                              *              
******************************************************************              
*                                                                               
*                                                                               
*                                                                               
GETTPREC NMOD1 0,*GTTP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*  TAPE READ IS TO BE DONE WHEN A BLOCK IS REQUIRED.  WHEN A RECORD             
*        REMAINS IN THE BLOCK, IT IS TO BE UNBLOCKED, AND DELIVERED             
*        TO THE RECORD2 AREA                                                    
*                                                                               
GETT0005 EQU   *                                                                
         CLI   BLKFLAG,C'Y'        EMPTY BLOCK?                                 
         BNE   GETT0020            NO  - BUMP TO NEXT RECORD                    
GETT0010 EQU   *                                                                
         GET   INTAPE,RECORD3      READ TAPE RECORD INTO RDA                    
*                                     END OF FILE -> GETT0100 (DCB)             
         CLI   BLK#1,C'Y'          SKIP FIRST RECORD?                           
         BNE   GETT0015            NO                                           
         MVI   BLK#1,C'N'          YES - SET 'NOT FIRST BLOCK'                  
         B     GETT0010            GO BACK AND READ NEXT                        
GETT0015 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        CLI   CONFLAG,C'Y'        CONTRACTS BEGUN?                             
*        BNE   GETT0017            NO                                           
*        CLC   CONCTR,=F'27500'    START CONTRACT?                              
*        BL    GETT0017            LOW: DON'T DISPLAY                           
*        GOTO1 REPORT                                                           
*        MVC   P+1(10),=C'PETRY I/P:'                                           
*        GOTO1 REPORT                                                           
*        LA    R4,RECORD3          A(RECORD LENGTH FIELD)                       
*        SR    R6,R6                                                            
*        ZICM  R6,RECORD3,2        GET LENGTH OF ENTRY                          
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(R6),=C'1D'                
*        GOTO1 REPORT              DISPLAY A BLANK LINE                         
GETT0017 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LA    R6,RECORD3          CALCULATE EOR, ETC                           
         ZICM  R5,RECORD3,2        GET BLOCK LENGTH FROM RECORD                 
         AR    R5,R6               CALCULATE END OF BLOCK                       
         ST    R5,BLKEND           SAVE A(END OF RECORD)                        
         LA    R6,12(R6)           BUMP TO L(FIRST RECORD)                      
         ST    R6,BLKADDR          SAVE A(REC IN PROCESS)                       
*                                                                               
*   TEST                                                                        
*        CLI   CONFLAG,C'Y'        CONTRACTS BEGUN?                             
*        BNE   GETT0040            NO                                           
*        GOTO1 REPORT                                                           
*        MVC   P+1(11),=C'BLOCK INFO:'                                          
*        GOTO1 HEXOUT,DMCB,BLKADDR,P+15,4,=C'TOG'                               
*        GOTO1 HEXOUT,DMCB,BLKEND,P+25,4,=C'TOG'                                
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         B     GETT0040            DON'T BUMP TO NEXT RECORD                    
GETT0020 EQU   *                                                                
         MVI   BLKFLAG,C'N'        SET 'BLOCK NOT EMPTY'                        
         L     R6,BLKADDR          LOAD A(RECORD IN PROCESS)                    
         PRINT GEN                                                              
         ZICM  R5,0(R6),2          GET RECORD LENGTH                            
         PRINT NOGEN                                                            
         AR    R6,R5               BUMP TO NEXT RECORD                          
         L     R5,BLKEND           GET EOR                                      
         CR    R6,R5               END OF RECORD REACHED?                       
         BNL   GETT0010            YES - READ ANOTHER BLOCK                     
         ST    R6,BLKADDR          NO  - STORE A(NEW REC IN PROCESS)            
GETT0040 EQU   *                                                                
         MVI   BLKFLAG,C'N'        SET 'BLOCK NOT EMPTY'                        
*                                                                               
*   TEST                                                                        
*        CLI   CONFLAG,C'Y'        CONTRACTS BEGUN?                             
*        BNE   GETT0045            NO                                           
*        CLC   CONCTR,=F'27500'    START CONTRACT?                              
*        BL    GETT0045            LOW: DON'T DISPLAY                           
*        GOTO1 REPORT                                                           
*        MVC   P+1(12),=C'RECORD INFO:'                                         
*        GOTO1 REPORT                                                           
*        LR    R4,R6               A(RECORD LENGTH FIELD)                       
*        SR    RF,RF                                                            
*        ZICM  RF,0(R6),2          GET LENGTH OF ENTRY                          
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*        GOTO1 REPORT              DISPLAY A BLANK LINE                         
*                                                                               
GETT0045 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LR    R1,R6               SAVE A(CONTROL BYTES)                        
         MVC   RECORD2+24(6),0(R1) INSERT CONTROL INFORMATION                   
         LA    R6,6(R6)            SET A(FIRST CHARACTER TO MOVE)               
         LA    R5,RECORD2+30       SET A(RECEIVING RECORD)                      
*                                     SKIP CONTROL 15 JDS WORDS                 
*                                  SET DISPLACEMENT INTO RECVING RECORD         
         LA    R1,4(R1)            SET A(COMPRESSED BYTE COUNT)                 
         PRINT GEN                                                              
         ZIC   R2,0(R1)            GET COMPRESSED BYTE COUNT                    
         PRINT NOGEN                                                            
         AR    R5,R2               DISPLACE INTO RECEIVING REC                  
         L     R1,BLKADDR          SET A(RECORD IN PROCESS)                     
         ZICM  R2,0(R1),2          RETRIEVE RECORD LENGTH                       
*                                                                               
*   TEST                                                                        
*        CLI   CONFLAG,C'Y'        CONTRACTS BEGUN?                             
*        BNE   GETT0055            NO                                           
*        CLC   CONCTR,=F'27500'    START CONTRACT?                              
*        BL    GETT0055            LOW: DON'T DISPLAY                           
*        MVC   P+1(22),=C'BLKADDR/LEN/REC2/SEND/'                               
*        GOTO1 REPORT                                                           
*        GOTO1 HEXOUT,DMCB,BLKADDR,P+3,4,=C'TOG'                                
*        EDIT  (R2),(4,P+12),FILL=0                                             
*        GOTO1 REPORT                                                           
*        MVC   P+1(60),0(R5)                                                    
*        GOTO1 REPORT                                                           
*        MVC   P+1(60),0(R6)                                                    
*        GOTO1 REPORT                                                           
*        GOTO1 REPORT                                                           
GETT0055 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
****     EX    R2,GETT0060                                                      
*                                                                               
*   RECORDS CAN EXCEED 255 CHARACTERS: NEED MOVE INSTRUCTION                    
*                                                                               
         LR    RF,R5               SET A(RECEIVING FIELD)                       
         LR    RE,R6               SET A(SENDING FIELD)                         
         LR    R1,R2               SET L(RECEIVING FIELD)                       
         PRINT GEN                                                              
         MOVE  ((RF),(R1)),(RE)                                                 
         PRINT NOGEN                                                            
         ZICM  R6,RECORD2+24,2     GET RECORD LENGTH                            
         ZIC   R5,RECORD2+28       GET COMPRESSED LENGTH                        
         AR    R6,R5               CALCULATE NEW RECORD LEN =                   
*                                     ORIG LEN + L(DUPE KEY PORTION)            
         STCM  R6,3,RECORD2+24     RESET RECORD LENGTH                          
*                                                                               
*   TEST                                                                        
*        CLI   CONFLAG,C'Y'        CONTRACTS BEGUN?                             
*        BNE   GETT0045            NO                                           
*        CLC   CONCTR,=F'1785'     START CONTRACT?                              
*        BL    GETT0057            LOW: DON'T DISPLAY                           
*        GOTO1 REPORT                                                           
*        MVC   P+1(12),=C'RECORD MOVE:'                                         
*        GOTO1 REPORT                                                           
*        LA    R4,RECORD2+24       A(RECORD LENGTH FIELD)                       
*        SR    RF,RF                                                            
*        ZICM  RF,RECORD2+24,2     GET LENGTH OF ENTRY                          
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*        GOTO1 REPORT              DISPLAY A BLANK LINE                         
*                                                                               
GETT0057 EQU   *                                                                
*   TEST END                                                                    
*                                                                               
         LTR   RB,RB               SET CC NOT ZERO                              
         B     GETT0200            EXIT ROUTINE                                 
GETT0060 EQU   *                                                                
         MVC   0(0,R5),0(R6)       MOVE DATA BY LENGTH                          
GETT0100 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO = EOF                          
GETT0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPIPUT:  DISPLAY PETRY RECORD INPUT.                        *              
******************************************************************              
*                                                                               
DISPIPUT NMOD1 0,*DIPT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P+1(8),=C'CONTRACT'                                              
         CLC   RECORD2+30(2),=X'0010'                                           
*                                  CONTRACT RECORD?                             
         BNE   DIPI0010            YES                                          
         CLI   QUESTOR+2,C'Y'      DISPLAY CONTRACT INPUT?                      
         BNE   DIPI0090            NO  - SKIP THIS RECORD                       
         CLC   CONCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPI0090                                                         
         CLC   CONCTR,HIGHCTR                                                   
         BH    DIPI0090                                                         
         EDIT  CONCTR,(6,P+40)                                                  
         B     DIPI0030            DISPLAY THE RECORD                           
DIPI0010 EQU   *                                                                
         MVC   P+1(8),=C'BUY REC '                                              
         CLC   RECORD2+30(2),=X'0020'                                           
*                                  BUY RECORD?                                  
         BNE   DIPI0015            YES                                          
*                                                                               
*   TEST                                                                        
****     CLC   JBUYKCON,=X'97955980'                                            
*        CLC   JBUYKCON,=X'97932451'                                            
*        BE    DIPI0030                                                         
*   TEST END                                                                    
*                                                                               
         CLI   QUESTOR+4,C'Y'      DISPLAY BUY      INPUT?                      
         BNE   DIPI0090            NO  - SKIP THIS RECORD                       
         CLC   BUYCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPI0090                                                         
         CLC   BUYCTR,HIGHCTR                                                   
         BH    DIPI0090                                                         
         EDIT  BUYCTR,(6,P+40)                                                  
         B     DIPI0030            DISPLAY THE RECORD                           
DIPI0015 EQU   *                                                                
         MVC   P+1(8),=C'ADV REC '                                              
         CLC   RECORD2+30(2),=X'000B'                                           
*                                  ADV RECORD?                                  
         BNE   DIPI0020            YES                                          
         CLI   QUESTOR+0,C'Y'      DISPLAY ADVERT   INPUT?                      
         BNE   DIPI0090            NO  - SKIP THIS RECORD                       
         CLC   ADVCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPI0090                                                         
         CLC   ADVCTR,HIGHCTR                                                   
         BH    DIPI0090                                                         
         EDIT  ADVCTR,(6,P+40)                                                  
         B     DIPI0030            DISPLAY THE RECORD                           
DIPI0020 EQU   *                                                                
         MVC   P+1(8),=C'AGY REC '                                              
         CLC   RECORD2+30(2),=X'000A'                                           
*                                  ADV RECORD?                                  
         BNE   DIPI0090            YES                                          
         CLI   QUESTOR+6,C'6'      DISPLAY ADVERT   INPUT?                      
         BE    DIPI0090            NO  - SKIP THIS RECORD                       
         CLC   AGYCTR2,LOWCTR      DISPLAY RANGE OF RECORDS                     
         BL    DIPI0090                                                         
         CLC   AGYCTR2,HIGHCTR                                                  
         BH    DIPI0090                                                         
         EDIT  AGYCTR2,(6,P+40)                                                 
         B     DIPI0030            DISPLAY THE RECORD                           
DIPI0030 EQU   *                                                                
         MVC   P+10(06),=C'INPUT '                                              
         MVC   P+30(06),=C'COUNT='                                              
         GOTO1 REPORT                                                           
         LA    R4,RECORD2+24       A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RECORD2+24,2     GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              DISPLAY A BLANK LINE                         
DIPI0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPIBUY:  DISPLAY PETRY BUY RECORD INPUT.                    *              
******************************************************************              
*                                                                               
DISPIBUY NMOD1 0,*DIPT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P+1(8),=C'BUY REC '                                              
         GOTO1 REPORT                                                           
         LA    R4,RECORD2+24       A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RECORD2+24,2     GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              DISPLAY A BLANK LINE                         
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*  DISPITST:  DISPLAY PETRY RECORD INPUT.                        *              
******************************************************************              
*                                                                               
DISPITST NMOD1 0,*DTST*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   P+10(06),=C'TEST  '                                              
         GOTO1 REPORT                                                           
         LA    R4,RECORD2+24       A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,RECORD2+24,2     GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT              DISPLAY A BLANK LINE                         
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPTOTS NMOD1 0,**DTOT**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
DITO0200 EQU   *                                                                
*                                                                               
         MVC   P+40(20),=C'**** RUN TOTALS ****'                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'TOTAL RECORDS   READ   :'                             
         EDIT  TOTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TOTAL CONTRACTS READ   :'                             
         EDIT  CONSREAD,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS GENERATED    :'                             
         EDIT  CONCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TOTAL BUYS READ        :'                             
         EDIT  BUYSREAD,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUYS GENERATED         :'                             
         EDIT  BUYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ADVERTISERS GENERATED  :'                             
         EDIT  ADVCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'RECORDS         WRITTEN:'                             
         EDIT  PUTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'COMPETITIVE STATIONS:   '                             
         EDIT  CMPCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MISSING STATIONS:       '                             
         EDIT  MISSTAT,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STATION RECORDS ADDED:  '                             
         EDIT  NEWSTATS,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'S/P RECORDS ADDED:      '                             
         EDIT  NEWSPCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'ORDERS W/O PRODUCT NAME:'                             
         EDIT  NOPRDCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'19/1A CONTRACTS        :'                             
         EDIT  SHORTCTR,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
*                                                                               
         L     RF,AG2CTR           SUBTRACT 2 PRIMER RECORDS                    
         BCTR  RF,0                   FROM MISSING AGENCY COUNT                 
         BCTR  RF,0                                                             
         ST    RF,AG2CTR           RESTORE ADJUSTED VALUE                       
*                                                                               
         MVC   P+1(24),=C'MISSING AGENCIES:       '                             
         EDIT  AG2CTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'NEW AGENCY  RECORDS:    '                             
         EDIT  AGYCTR2,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'NEW AGENCY2 RECORDS:    '                             
         EDIT  AGYCTR3,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTRT,P+20,4,=C'TOG'                               
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SHOWSPS :  DISPLAY RESULTING SALESPERSON EQUATIONS                          
*                                                                               
         DS    0F                                                               
SHOWSPS  NMOD1 0,*SSPS*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1          SET SUBPROGRAM HEADING                       
         L     R2,ASALAREA                                                      
         LA    R2,16(R2)           SKIP FIRST ENTRY                             
         L     R3,SALCTR                                                        
         BCTR  R3,0                SUBTRACT 2 FROM COUNT                        
         BCTR  R3,0                   FOR LOW/HIGH ENTRIES                      
         LTR   R3,R3               ANY VALUE IN COUNTER?                        
         BZ    SSPS0040            NO  - EXIT                                   
SSPS0020 EQU   *                                                                
         MVC   P+1(4),0(R2)        DISPLAY JDS CODE                             
         MVC   P+6(2),4(R2)        DISPLAY JDS DIV/TEAM                         
         MVC   P+9(2),6(R2)        DISPLAY JDS OFFICE                           
         MVC   P+15(3),8(R2)       DISPLAY DDS CODE                             
         MVC   P+19(2),11(R2)      DISPLAY DDS DIV/TEAM                         
         MVC   P+22(2),13(R2)      DISPLAY DDS OFFICE                           
         GOTO1 REPORT                                                           
         LA    R2,16(R2)           BUMP TO NEXT ENTRY                           
         BCT   R3,SSPS0020         GO BACK FOR NEXT                             
SSPS0040 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*   CATSMISS:  DISPLAY MISSING CATEGORY CODES                                   
*                                                                               
         DS    0F                                                               
CATSMISS NMOD1 0,*CMIS*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,ACATAREA                                                      
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(28),=C'CATEGORY CODES UNRECOGNIZED:'                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         SR    R1,R1               CLEAR COUNTER                                
CMIS0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    CMIS0200            YES - FINISHED                               
         LA    R1,1(R1)            INCREMENT COUNTER                            
         EDIT  (R1),(3,P+1)                                                     
         MVC   P+10(4),0(R2)       MOVE TABLE TO PRINT                          
         GOTO1 REPORT                                                           
         LA    R2,4(R2)                                                         
         B     CMIS0020            GO BACK FOR NEXT                             
CMIS0200 EQU   *                                                                
         XIT1                                                                   
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GENSTATS:  GENERATE MISSING STATION RECORDS                                 
*                                                                               
         DS    0F                                                               
GENSTATS NMOD1 0,*GSTA*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   RCSUBPRG,2          SET SUBPROGRAM HEAD                          
         MVI   FORCEHED,C'Y'                                                    
         L     R7,AMISAREA         SET A(MISSING STATION AREA)                  
GENS0040 EQU   *                                                                
         XCEFL RSTAREC,1000        CLEAR STATION RECORD                         
         MVI   RSTAREC,2           INSERT RECORD TYPE                           
         MVC   RSTALEN(2),=X'0075' INSERT RECORD LENGTH                         
         MVC   RSTAELEM(2),=X'0153' SET DESCRIPTOR ELEMENT                      
         MVC   RSTAMKT,=C'MISSING STATION     '                                 
         GOTO1 DATCON,DMCB,(5,0),(3,RSTASTRT)                                   
*                                  INSERT TODAY'S DATE AS JOINED                
         MVC   RSTACHAN,=X'0123'   INSERT DUMMY STATION                         
         MVC   RSTAAFFL,=C'IND'    INSERT AFFILIATION                           
         MVC   RSTAGRUP,=C'P*'     INSERT GROUP/SUBGROUP                        
***>>>   MVC   RSTAGRUP,=C'P '     INSERT GROUP/SUBGROUP                        
         MVI   RSTATRAF,C' '       SET TRAFFIC TO ' '                           
         XC    ELTBILD1,ELTBILD1                                                
*                                  ADD X'05' EXTENDED DESCRIP ELEMENT           
         MVI   ELTBILD1,5          INSERT ELEMENT CODE                          
         MVI   ELTBILD1+1,20       INSERT ELEMENT LENGTH                        
         MVC   ELTBILD1+2(8),=C'GRAPH   '                                       
*                                  DEFAULT TO GRAPHNET                          
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,ELTBILD1,0             
*                                  ADD X'8' ELEMENT TO RECORD                   
         XC    ELTBILD1,ELTBILD1                                                
         MVI   ELTBILD1,8          INSERT ELEMENT CODE                          
         MVI   ELTBILD1+1,80       INSERT ELEMENT LENGTH                        
         MVC   ELTBILD1+2(9),=C'NNNNNNNNN'                                      
         OI    ELTBILD1+55,X'20'   SET 'CONVERTED RECORD'                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,ELTBILD1,0             
*                                  ADD X'8' ELEMENT TO RECORD                   
         XC    ELTBILD1,ELTBILD1                                                
GENS0120 EQU   *                                                                
         OC    0(4,R7),0(R7)       STATION PRESENT?                             
         BZ    GENS0200            NO  - FINISHED                               
         MVC   P+1(16),=C'MISSING STATION:'                                     
         MVC   P+20(4),0(R7)       INSERT STATION                               
         GOTO1 REPORT              PRINT STATION MISSING                        
         MVC   RSTAKSTA(4),0(R7)   INSERT STATION INTO KEY                      
         MVI   RSTAKSTA+4,C' '     INSERT MEDIA AS SPACE                        
         MVC   RSTAKREP,PETRYREP   INSERT REP CODE INTO RECORD                  
         BAS   RE,COMPSIN          INSERT COMPETITIVE STATIONS                  
         LA    RF,REC                                                           
         LA    R2,1000                                                          
         LA    RE,RSTAREC                                                       
         ST    R7,DUB              SAVE A(TABLE)                                
         PRINT GEN                                                              
         MOVE  ((RF),(R2)),(RE)    MOVE RECORD TO OUTPUT                        
         PRINT NOGEN                                                            
         L     R7,DUB              RESET A(TABLE)                               
         MVC   REC-4(2),RSTALEN    INSERT LENGTH INTO OUTPUT                    
         GOTO1 PUTRECS2            GENERATE O/P REC FOR STATIONS                
         L     RF,NEWSTATS                                                      
         LA    RF,1(RF)                                                         
         ST    RF,NEWSTATS                                                      
         CLI   QUESTOR+7,C'A'      DISPLAY STATABLE + MISSING STNS?             
         BE    GENS0150            YES                                          
         CLI   QUESTOR+7,C'B'      NO  - DISPLAY MISSING STNS ONLY              
         BNE   GENS0160            NO                                           
GENS0150 EQU   *                                                                
         BAS   RE,DTOTSTAS         DISPLAY THE STATION RECORD                   
GENS0160 EQU   *                                                                
         LA    R7,6(R7)            BUMP ADDRESS IN ARRAY                        
         B     GENS0040            NO - GO BACK FOR NEXT                        
GENS0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   FIND STATION IN COMPETITIVE TABLE, INSERT COMPETITIVE STATIONS              
*                                                                               
COMPSIN  NTR1                                                                   
         L     R2,ACMPAREA         SET A(COMPETITIVE TABLE)                     
         L     R4,CMPCTR           CURRENT # OF TABLE ENTRIES                   
         XC    COMPCTR,COMPCTR     CLEAR COMP STATIONS COUNTER                  
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RSTAKSTA,(R2),(R4),63,4,2000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   COSI0200            NO  - NO COMPETITIVE STATIONS                
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         LA    R2,7(R2)            PASS THE REP'D STATION                       
         LA    R6,8                SET LOOP CONTROL                             
COSI0020 EQU   *                                                                
         OC    0(4,R2),0(R2)       ANY ENTRY?                                   
         BZ    COSI0200            NO  - FINISHED                               
         XC    ELTBILD1,ELTBILD1                                                
         MVI   ELTBILD1,2          INSERT ELEMENT CODE                          
         MVI   ELTBILD1+1,15       INSERT ELEMENT LENGTH                        
         MVC   ELTBILD1+2(4),0(R2) INSERT STATION CALLS                         
         MVI   ELTBILD1+6,C' '     INSERT MEDIA                                 
         MVC   ELTBILD1+7(3),4(R2) INSERT AFFILIATION                           
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,ELTBILD1,0             
*                                  ADD COMP ELT                                 
         XC    ELTBILD1,ELTBILD1                                                
         LA    R2,7(R2)            BUMP TO NEXT ENTRY                           
         BCT   R6,COSI0020         GO BACK FOR NEXT                             
COSI0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS2:  GENERATE OUTFILE ENTRIES                           *              
******************************************************************              
*                                                                               
PUTRECS2 NTR1                                                                   
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           PUT IT BACK                                  
*                                                                               
*        MVC   P+1(07),=C'PUTREC:'                                              
*        MVC   P+20(27),REC                                                     
*        EDIT  PUTCTR,(5,P+10)                                                  
*        GOTO1 REPORT                                                           
*                                                                               
         LA    RF,REC                                                           
         LA    R1,1000                                                          
         LA    RE,RECORD                                                        
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RCONLEN    INSERT RECORD LENGTH INTO OUTPUT             
*                                     ALL RECORDS ORG'D TO SAME ADDR            
*                                        USING COMMON RCONLEN                   
*                                                                               
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
*   DTOTSTAS                                                                    
*                                                                               
DTOTSTAS NTR1                                                                   
         MVC   P+10(14),=C'STATION ADDED:'                                      
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TEMCONV:  CONVERT THE JDS OFFICE/TEAM TO A DDS TEAM CODE.                   
*                                                                               
         DS    0F                                                               
TEMCONV  NMOD1 0,**TEMC**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            SET FLAG FOR RECORD INDICATOR                
         MVC   TEAMOFF,JCONKOFF                                                 
         MVC   TEAMDIV,JCONKDIV    INSERT DIVISION OF DIV/TEAM                  
         LA    R2,TEAMTABL         SET A(S/P TABLE)                             
         LA    R4,TEMTBL#          CURRENT # OF TABLE ENTRIES                   
         GOTO1 =V(BINSRCH),DMCB,TEAMOFF,(R2),(R4),6,4,(R4),RR=RELO              
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   TEMC0040            NO  - GENERATE NEW CODE                      
*                                  YES - USE CODE FROM TABLE                    
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                                                               
*   TEST                                                                        
*        CLC   CONCTR,=F'25'       FIRST N ONLY                                 
*        BH    TEST0010                                                         
*        MVC   P+1(11),=C'TEAM FOUND:'                                          
*        MVC   P+13(6),0(R2)                                                    
*        GOTO1 REPORT                                                           
TEST0010 EQU   *                                                                
*   TEST                                                                        
*                                                                               
         LTR   R3,R3               UPDATE CONTRACT RECORD?                      
         BNZ   TEMC0010            NO  - UPDATE S/P RECORD                      
         MVC   RCONTEM,TMDDSTEM(R2)                                             
*                                  YES - INSERT TEAM FROM TABLE                 
         B     TEMC0200            FINISHED                                     
TEMC0010 EQU   *                                                                
         L     R6,ARECORD5         UPDATE SALESPERSON RECORD                    
         USING RSALREC,R6                                                       
         MVC   RSALTEAM,TMDDSTEM(R2)                                            
*                                  INSERT TEAM FROM TABLE                       
*                                                                               
         DROP  R6                                                               
         B     TEMC0200            FINISHED                                     
TEMC0040 EQU   *                                                                
         LTR   R3,R3               UPDATE CONTRACT RECORD?                      
         BNZ   TEMC0050            NO  - UPDATE S/P RECORD                      
         MVC   RCONTEM,=C'T '      INSERT DEFAULT VALUE                         
         B     TEMC0200            FINISHED                                     
*                                                                               
TEMC0050 EQU   *                                                                
         L     R6,ARECORD5         UPDATE SALESPERSON RECORD                    
         USING RSALREC,R6                                                       
         MVC   RSALTEAM,=C'T '                                                  
*                                  INSERT DEFAULT VALUE                         
*                                                                               
         DROP  R6                                                               
TEMC0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*  TEAMTABL SETUP:                                                              
*        CHARS 1  -  2  =  JDS OFFICE                                           
*        CHAR  3  -  4  =  JDS DIVISION/TEAM                                    
*        CHARS 5  -  6  =  DDS TEAM CODE                                        
*                                                                               
TMJDSOFF EQU   0                                                                
TMJDSTEM EQU   2                                                                
TMDDSTEM EQU   4                                                                
*                                                                               
TEAMTABL DS    0CL6                                                             
         DC    C'ATY TN'                                                        
LTEMTABL EQU   *-TEAMTABL                                                       
         DC    C'ATZ TZ'                                                        
         DC    C'BOY TN'                                                        
         DC    C'BOZ TZ'                                                        
         DC    C'CBZ T '            CABLE NETWORK?? NEED OFFICE                 
         DC    C'CHB TO'                                                        
         DC    C'CHC TA'                                                        
         DC    C'CHD TR'                                                        
         DC    C'CHG T1'                                                        
         DC    C'CHL TB'                                                        
         DC    C'CHM TU'                                                        
         DC    C'CHO T2'                                                        
         DC    C'CHR TD'                                                        
         DC    C'CHS TS'                                                        
         DC    C'CHT T3'                                                        
         DC    C'CHW T4'                                                        
         DC    C'CHX TX'                                                        
         DC    C'CLZ T '                                                        
         DC    C'CVZ T '                                                        
         DC    C'DAY TN'                                                        
         DC    C'DAZ TZ'                                                        
         DC    C'DEY TN'                                                        
         DC    C'DEZ TZ'                                                        
         DC    C'DNZ TN'                                                        
         DC    C'DTZ TZ'                                                        
         DC    C'DVZ T '                                                        
         DC    C'HOZ T '                                                        
         DC    C'KPK T9'                                                        
         DC    C'KPZ T8'            KRPC NATIONAL                               
         DC    C'LAC TC'                                                        
         DC    C'LAD TR'                                                        
         DC    C'LAL TB'                                                        
         DC    C'LAM TM'                                                        
         DC    C'LAX TX'                                                        
         DC    C'LAZ T '                                                        
         DC    C'MNZ T '                                                        
         DC    C'MIZ T '                                                        
         DC    C'NYC T5'                                                        
         DC    C'NYD TR'                                                        
         DC    C'NYE TE'                                                        
         DC    C'NYF TF'                                                        
         DC    C'NYG TG'                                                        
         DC    C'NYH TH'                                                        
         DC    C'NYL TB'                                                        
         DC    C'NYN T7'                                                        
         DC    C'NYQ TQ'                                                        
         DC    C'NYR T6'                                                        
         DC    C'NYT TT'                                                        
         DC    C'NYV TV'                                                        
         DC    C'NYW TW'                                                        
         DC    C'NYX TX'                                                        
         DC    C'POZ T '                                                        
         DC    C'PHZ T '                                                        
         DC    C'SEZ T '                                                        
         DC    C'SLZ T '                                                        
         DC    C'SFZ T '                                                        
         DC    C'TAZ T '                                                        
         DC    C'UWZ T '                                                        
         DC    C'VDZ T '            TOTAL VIDEO??  NEED OFFICE                  
         DC    C'ZZZ T '                                                        
         DC    6X'FF'                                                           
LTEMTBL  EQU   *-TEAMTABL                                                       
TEMTBL#  EQU   (*-TEAMTABL)/L'TEAMTABL                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SALCONV:  CONVERT SALESPERSON CODES FROM TABLE EQUIVALENCY.                 
*      TWO TABLES ARE USED:                                                     
*        1ST:  CONVERSION VALUE TABLE:  JDS SP/DIVTEAM/OFF SEQUENCE,            
*              WITH NEW DDS SP/TEAM CODES TO USE.  THIS TABLE WILL              
*              ONLY INCLUDE THOSE ENTRIES EQUIVALENCED, SO IT WILL              
*              NOT SHOW ALL CODES IN USE.                                       
*        2ND:  ALL CODES IN USE:  DDS CODE ONLY                                 
*              WHEN A CODE IS 'CREATED', IT MUST BE ADDED TO BOTH               
*              TABLES, ONCE FOR EQUIVALENCY, ONCE FOR ACTIVITY.                 
*                                                                               
         DS    0F                                                               
SALCONV  NMOD1 0,**SPCV**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,ASALAREA         SET A(S/P TABLE)                             
         L     R4,SALCTR           CURRENT # OF TABLE ENTRIES                   
         XC    WORK2(16),WORK2     CLEAR THE WORK AREA                          
         MVC   WORK2(4),JCONKSAL    CONSTRUCT SEARCH ARGUMENT                   
         MVC   WORK2+4(2),JCONKDIV     SAL/DIVTEAM/OFF                          
         MVC   WORK2+6(2),JCONKOFF                                              
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,WORK2,(R2),(R4),16,8,2000,RR=RELO               
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   SPCV0040            NO  - GENERATE NEW CODE                      
*                                  YES - USE CODE FROM TABLE                    
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
         MVC   RCONSAL(3),8(R2)    INSERT S/P FROM TABLE                        
         MVC   RCONKOFF(2),13(R2)  INSERT S/P OFFICE FROM TABLE                 
         B     SPCV0100            FINISHED                                     
SPCV0040 EQU   *                                                                
         BAS   RE,SETNEWSP         CREATE NEW S/P CODE                          
*                                                                               
SPCV0100 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DETERMINE NEW SALESPERSON, BASED ON INPUT, AND ADD TO BINARY                
*        SEARCH TABLE FOR NEXT USE.                                             
*        WORK2 CONTAINS SEARCH ARGUMENT.  SEARCH ARGUMENT WILL BE               
*        USED TO FIND ENTRY IN NAME TABLE, FROM WHICH S/P CODE                  
*        MAY BE CONSTRUCTED.                                                    
*                                                                               
*                                                                               
SETNEWSP NTR1                                                                   
*                                                                               
*   1:   RETRIEVE NAME FOR JDS SP/DIVTEAM/OFF KEY                               
*                                                                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(11),=C'WORK2 SETUP:'                                         
*        MVC   P+16(16),WORK2                                                   
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         MVI   LOADALFA,C'N'       SET 'LOAD ALPHA TBL' OFF                     
         L     R2,ASPNAREA         SET A(NAME TABLE)                            
         L     R4,NAMCTR           SET A(TABLE COUNTER)                         
         GOTO1 =V(BINSRCH),DMCB,WORK2,(R2),(R4),50,8,3000,RR=RELO               
*                                                                               
*                                  MAX TABLE ENTRIES MAY CHANGE                 
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SNEW0120            YES                                          
*        *                                                                      
*   REMOVE BRANCH TO ACTIVATE DISPLAY                                           
*                                                                               
         B     SNEW0080            SKIP DISPLAYS                                
*                                                                               
         L     R2,ASPNAREA         SET A(NAME TABLE)                            
         L     R4,NAMCTR                                                        
         MVC   P+1(4),=C'WK2:'     ENTRY BEING SOUGHT                           
         MVC   P+6(50),WORK2                                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         CLI   NAMEDUMP,C'Y'       NAME DUMP ALREADY DONE?                      
         BE    SNEW0080            YES                                          
         MVI   NAMEDUMP,C'Y'       NO  - SET TO 'ALREADY DONE'                  
         LA    R7,1                SET COUNTER                                  
SNEW0040 EQU   *                                                                
         MVC   P+1(4),=C'TBL:'                                                  
         MVC   P+6(50),0(R2)                                                    
         EDIT  (R7),(5,P+40)                                                    
         LA    R7,1(R7)                                                         
         GOTO1 REPORT                                                           
         LA    R2,50(R2)           BUMP TO NEXT ENTRY                           
         BCT   R4,SNEW0040         DISPLAY ALL ENTRIES                          
SNEW0080 EQU   *                                                                
         MVI   LOADALFA,C'Y'       SET 'LOAD ALPHA TABLE'                       
*                                  FORCE IN ALPHABET/NUMBERS                    
         MVC   TELESAVE,SPACES                                                  
         MVC   NAMESAVE,SPACES                                                  
         MVC   NAMESAVE(16),=C'NAME NOT ON FILE'                                
         B     SNEW0160                                                         
*                                                                               
*   2:   REDUCE NAME BY COMPRESSING OUT VALUES < X'C1' (A)                      
*                                                                               
SNEW0120 EQU   *                                                                
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
         MVC   NAMESAVE(30),8(R2)  SAVE NAME FROM TABLE                         
         MVC   TELESAVE(12),38(R2) SAVE TELEPHONE NUMBER FROM TABLE             
*                                                                               
SNEW0160 EQU   *                                                                
         MVC   NAMEWORK,SPACES     CLEAR WORK AREA                              
         LA    RE,NAMEWORK                                                      
         MVC   0(1,RE),JCONKSAL    INSERT FIRST CHAR OF SP CODE                 
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         CLI   JCONKSAL+1,C' '     2ND CHAR A SPACE?                            
         BE    SNEW0200            YES - DON'T TAKE ANY MORE                    
         MVC   0(1,RE),JCONKSAL+1  INSERT 2ND   CHAR OF SP CODE                 
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         CLI   JCONKSAL+2,C' '     3RD CHAR A SPACE?                            
         BE    SNEW0200            YES - DON'T TAKE ANY MORE                    
         MVC   0(1,RE),JCONKSAL+2  INSERT 3RD   CHAR OF SP CODE                 
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         CLI   JCONKSAL+3,C' '     4TH CHAR A SPACE?                            
         BE    SNEW0200            YES - DON'T TAKE ANY MORE                    
         MVC   0(1,RE),JCONKSAL+3  INSERT 4TH   CHAR OF SP CODE                 
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
SNEW0200 EQU   *                                                                
         CLI   LOADALFA,C'Y'       LOAD ALPHA TABLE?                            
         BNE   SNEW0240            NO                                           
         MVC   0(LALFATBL,RE),ALFATABL                                          
*                                  LOAD ALPHA TABLE                             
         B     SNEW0360                                                         
SNEW0240 EQU   *                                                                
         LA    R3,8(R2)            SET A(1ST CHAR OF NAME)                      
         LA    RF,22               SET LOOP CONTROL                             
SNEW0280 EQU   *                                                                
         CLI   0(R3),C'A'          LOWER THAN C'A'?                             
         BL    SNEW0320            YES - COMPRESS IT OUT                        
         MVC   0(1,RE),0(R3)       NO  - MOVE IT TO OUTPUT                      
         LA    RE,1(RE)            BUMP TO NEXT NAMEWORK POS                    
SNEW0320 EQU   *                                                                
         LA    R3,1(R3)            BUMP TO NEXT TABLE NAME POS                  
         BCT   RF,SNEW0280         GO BACK FOR NEXT                             
         MVC   0(24,RE),SALNUMBR   STRING IN EXTRAS                             
SNEW0360 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(26),=C'S/P CODE GENERATION SETUP:'                           
*        GOTO1 REPORT                                                           
*        MVC   P+1(12),=C'NAME  ENTRY:'                                         
*        MVC   P+14(50),0(R2)                                                   
*        GOTO1 REPORT                                                           
*        MVC   P+1(12),=C'SEEK  ENTRY:'                                         
*        MVC   P+14(80),NAMEWORK                                                
*        GOTO1 REPORT                                                           
*        MVC   P+1(12),=C'WORK2 ENTRY:'                                         
*        MVC   P+14(16),WORK2                                                   
*        GOTO1 REPORT                                                           
*   TEST                                                                        
*                                                                               
*                                  NAME NOW IN WORK W/NO SPACES                 
*   3:   SET UP LETTER EXTRACT FROM NAME                                        
*                                                                               
         LA    R2,NAMEWORK         SET UP KEY POSITIONS                         
         LA    R3,NAMEWORK+1                                                    
         LA    R4,NAMEWORK+2                                                    
SNEW0400 EQU   *                                                                
         MVC   CODEWORK(1),0(R2)   BUILD CODE FROM LETTERS                      
         MVC   CODEWORK+1(1),0(R3)                                              
         MVC   CODEWORK+2(1),0(R4)                                              
*                                                                               
*   4:   RETRIEVE CODE FROM 2ND SP TABLE:  CODES ONLY                           
*           IF FOUND, TRY NEXT KEY SETUP                                        
*           IF NOT FOUND, NEW KEY IS INSERTED INTO 2ND SP TABLE                 
*              AND EQUIVALENCY ENTRY INSERTED INTO 1ST SP TABLE                 
*                                                                               
SNEW0440 EQU   *                                                                
         L     R6,A2SPAREA         SET A(CODE TABLE)                            
         L     R7,SP2CTR           SET A(TABLE COUNTER)                         
         GOTO1 =V(BINSRCH),DMCB,(1,CODEWORK),(R6),(R7),3,3,6000,RR=RELO         
*                                                                               
*                                  MAX TABLE ENTRIES MAY CHANGE                 
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SNEW0600            YES - SET NEXT KEY UP, TRY AGAIN             
*                                                                               
*   5:   NOT FOUND:  BUILD ENTRY FOR 1ST SP TABLE                               
*           INSERT NEW CODE INTO CONTRACT RECORD BEING BUILT                    
*                                                                               
         MVC   SP2CTR,DMCB+8       NO  - SAVE NEW COUNT, CODE INSERTED          
         MVC   RCONSAL,CODEWORK    INSERT CODE INTO RECORD                      
         MVC   RCONKOFF,JCONKOFF   INSERT OFFICE INTO RECORD                    
*                                                                               
         MVC   WORK2+8(3),CODEWORK  INSERT NEW CODE INTO EQUIV TABLE            
         MVC   WORK2+11(2),JCONKDIV INSERT TEAM/DIVISION                        
         MVC   WORK2+13(2),JCONKOFF INSERT OFFICE                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(14),=C'NEW SP1 TABLE:'                                       
*        MVC   P+15(16),WORK2                                                   
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
*                                  INSERT INTO EQUIV TABLE                      
         L     R6,ASALAREA         SET A(CODE TABLE)                            
         L     R7,SALCTR           SET A(TABLE COUNTER)                         
         GOTO1 =V(BINSRCH),DMCB,(1,WORK2),(R6),(R7),16,8,2000,RR=RELO           
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   SNEW0520            NO  - ADDED - FINISHED                       
         ZICM  R2,DMCB+1,3         YES - SHOULDN'T BE HERE                      
*                                                                               
*   TEST                                                                        
*        MVC   P+1(13),=C'ON FILE: SNB:'                                        
*        MVC   P+15(16),0(R2)                                                   
*        GOTO1 REPORT                                                           
*        L     R6,ASALAREA                                                      
*        L     R7,SALCTR                                                        
*        LA    R3,1                                                             
SNEW0480 EQU   *                                                                
*        EDIT  (R3),(3,P+1)                                                     
*        MVC   P+5(16),0(R6)                                                    
*        GOTO1 REPORT                                                           
*        LA    R3,1(R3)            BUMP COUNTER                                 
*        LA    R6,16(R6)           BUMP TO NEXT TABLE ENTRY                     
*        BCT   R7,SNEW0480                                                      
*   TEST END                                                                    
*                                                                               
         DC    H'0'                DUMP                                         
SNEW0520 EQU   *                                                                
         MVC   SALCTR,DMCB+8       SAVE NEW COUNT                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'NEW TABLE:'                                           
*        MVC   P+15(16),0(R2)                                                   
*        GOTO1 REPORT                                                           
*        L     R6,ASALAREA                                                      
*        L     R7,SALCTR                                                        
*        LA    R3,1                                                             
SNEW0560 EQU   *                                                                
*        EDIT  (R3),(3,P+1)                                                     
*        MVC   P+5(16),0(R6)                                                    
*        GOTO1 REPORT                                                           
*        LA    R3,1(R3)            BUMP COUNTER                                 
*        LA    R6,16(R6)           BUMP TO NEXT TABLE ENTRY                     
*        BCT   R7,SNEW0560                                                      
*   TEST END                                                                    
*                                                                               
         B     SNEW0640                                                         
*                                                                               
SNEW0600 EQU   *                                                                
         LA    R4,1(R4)            BUMP MINOR KEY                               
         CLI   0(R4),C' '          SPACE IN FIELD?                              
         BNE   SNEW0400            NO  - GO BACK AND SET NEW KEY                
         LA    R3,1(R3)            YES - BUMP MIDDLE KEY                        
         LR    R4,R3               SET MINOR TO MIDDLE                          
         LA    R4,1(R4)            BUMP MINOR KEY                               
         CLI   0(R4),C' '          MINOR KEY = SPACE?                           
         BNE   SNEW0400                                                         
         LA    R2,1(R2)            YES - BUMP HIGH KEY                          
         LR    R3,R2               SET MIDDLE TO HIGH                           
         LA    R3,1(R3)            BUMP MIDDLE KEY                              
         LR    R4,R2               SET MINOR TO HIGH                            
         LA    R4,2(R4)            BUMP MINOR KEY                               
         CLI   0(R4),C' '          MINOR KEY = SPACE?                           
         BNE   SNEW0400                                                         
         MVC   P+1(15),=C'NO UNIQUE CODE:'                                      
         MVC   P+16(30),NAMEWORK   STRING IN NAME                               
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
*                                                                               
SNEW0640 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(04),=C'S/P:'                                                 
*        MVC   P+7(10),WORK        INSERT ORIGINAL NUMBER,OFFICE,COMP           
*        MVC   P+17(18),RCONSAL                                                 
*        MVC   P+40(5),=C'CONV:'                                                
*        MVC   P+48(3),SAVCODE     INSERT NEW NUMBER                            
*        EDIT  SALCTR,(7,P+70)                                                  
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*                                                                               
*   NOW ADD A MISSING SALESPERSON RECORD                                        
*                                                                               
         L     R6,ARECORD5         SET ADDRESSABILITY                           
         USING RSALREC,R6                                                       
*                                                                               
         XCEFL RSALREC,1000        CLEAR SALESPERSON RECORD                     
         MVI   RSALREC,6           INSERT RECORD TYPE                           
         MVC   RSALKREP,RCONKREP   INSERT REP CODE                              
         MVC   RSALKSAL,CODEWORK   INSERT SALESPERSON CODE                      
         MVC   RSALLEN(2),=X'0078' INSERT RECORD LENGTH                         
         MVC   RSALELEM(2),=X'0156' SET DESCRIPTOR ELEMENT                      
         MVC   RSALNAME,SPACES                                                  
         MVC   RSALNAME(20),NAMESAVE                                            
*                                  INSERT SALESPERSON NAME                      
         MVC   RSALTEL(12),TELESAVE                                             
*                                  INSERT SALESPERSON TELEPHONE                 
         GOTO1 =A(TEMCONV),DMCB,(RC),1,RR=Y                                     
*                                                                               
         MVI   RSALPROF,C'0'                                                    
         MVC   RSALPROF+1(L'RSALPROF-1),RSALPROF                                
*                                                                               
         MVC   RSALOFF,RCONKOFF    INSERT OFFICE CODE                           
*                                                                               
         MVC   RSALEQU,WORK2       INSERT ORIGINAL CODE INTO RECORD             
         LA    RF,REC                                                           
         LA    R2,1000                                                          
         LA    RE,RSALREC                                                       
         MOVE  ((RF),(R2)),(RE)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RSALLEN    INSERT LENGTH INTO OUTPUT                    
         GOTO1 PUTRECSP,DMCB,(RC),RSALREC                                       
         L     RF,NEWSPCTR         INCREMENT NEW S/P COUNT                      
         LA    RF,1(RF)                                                         
         ST    RF,NEWSPCTR                                                      
         CLI   QUESTOR+8,C'B'      DISPLAY SALESPERSON REC O/P?                 
         BNE   SNEW0800            NO                                           
         CLC   SALCTR,=F'25'       DISPLAY FIRST 25 ONLY                        
         BH    SNEW0800                                                         
         BAS   RE,DTOTSALS         DISPLAY THE SALESPN RECORD                   
SNEW0800 EQU   *                                                                
         XIT1                                                                   
*                                                                               
******************************************************************              
*  PUTRECSP: GENERATE OUTFILE ENTRIES FOR S/P RECORDS            *              
******************************************************************              
*                                                                               
PUTRECSP NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            SET A(RECORD BEING OUTPUT)                   
*                                                                               
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           PUT IT BACK                                  
*                                                                               
*        MVC   P+1(09),=C'PUTRECSP:'                                            
*        MVC   P+20(27),REC                                                     
*        EDIT  PUTCTR,(5,P+10)                                                  
*        GOTO1 REPORT                                                           
*                                                                               
         LA    RF,REC                                                           
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),(R2)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RSALLEN-RSALKEY(R2)                                     
*                                  INSERT LENGTH, USING COMMON RCONLEN          
*                                                                               
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
DTOTSALS NTR1                                                                   
         MVC   P+1(14),=C'S/P     ADDED:'                                       
         MVC   P+16(3),RSALKSAL                                                 
         MVC   P+21(18),RSALNAME                                                
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
SALNUMBR DC    C'0123456789!@#$%/*()+"?><'                                      
ALFATABL DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
         DC    C'0123456789!@#$%/*()+"?><'                                      
LALFATBL EQU   *-ALFATABL                                                       
NAMEWORK DS    CL80                                                             
NAMESAVE DS    CL30                                                             
TELESAVE DS    CL12                                                             
CODEWORK DS    CL3                                                              
SALOFF   DS    CL1                                                              
SAVCODE  DS    CL3                                                              
NONAMLTR DS    CL1                                                              
SAVSPCDE DS    CL5                 CODE BEING CONVERTED                         
NONAMCTR DS    F                                                                
ASALNUM  DS    A                                                                
SAVNWCDE DS    A                   A(RECEIVING FIELD OF CODE)                   
SAVNWNAM DS    A                   A(SALESPERSON/NETWORK EXEC NAME)             
         EJECT                                                                  
**** INSERT                                                                     
*   INITIALIZATIONS ....                                                        
         DS    0F                                                               
INITIAL  NMOD1 0,**INIT**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   BLKFLAG,C'Y'        SET 'BLOCK EMPTY' FLAG                       
         MVI   BLK#1,C'Y'          SET 'FIRST PASS' FLAG                        
         MVI   NAMEDUMP,C'N'       SET 'NEED NAME DUMP' FLAG                    
         MVI   LOADALFA,C'N'       SET 'LOAD ALPHA TBL' FLAG                    
         OPEN  (INTAPE,(INPUT))                                                 
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1000000,1000000                                 
*                                  GET 1 MEG STORAGE SPACE                      
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'36000'        TAPE BUFFER AREA:                            
         ST    RF,MGCMAREA         SET A(MAKEGOOD COMMENT AREA)                 
         A     RF,=F'240'          SAVE ROOM FOR 240 BYTES                      
         ST    RF,AAGYAREA         SET A(AGENCY AREA)                           
         ST    RF,AAGYEND          SET A(NEXT AGENCY)                           
         A     RF,=F'500'          SAVE ROOM FOR 125 ENTRIES                    
         ST    RF,ACATAREA         SET A(CATEGORY AREA)                         
         A     RF,=F'120000'       SAVE ROOM FOR 10,000 ENTRIES                 
         ST    RF,AAG2AREA         SET A(MISSING AGENCY TABLE)                  
         ST    RF,AAG2END          SET A(NEXT MISSING AGENCY)                   
         A     RF,=F'120000'       SAVE ROOM FOR 20,000 ENTRIES                 
         ST    RF,ACMPAREA         SET A(STATION AREA)                          
         ST    RF,ACMPNXT          SET A(NEXT AVAILABLE SLOT)                   
         MVI   CMPFLAG,C'N'                                                     
*                                                                               
*   LEAVE SUFFICIENT SPACE FOR STATION COMPETITIVE TABLE                        
*        EACH ENTRY:                                                            
*        4 CHARS:  BASE STATION                                                 
*        7 CHARS:  EACH OF EIGHT COMPS (MAX) + REP'D                            
*        2000 STATIONS TOTAL                                                    
*        7 X 9 X 2000  =  126000                                                
*                                                                               
         A     RF,=F'126000'       SIZE OF COMP TABLE                           
         ST    RF,ASTAAREA         SET A(STATION GRP/SUBGRP AREA)               
*                                                                               
*   LEAVE SUFFICIENT SPACE FOR STATION GROUP/SUBGROUP TABLE                     
*        EACH ENTRY:                                                            
*        4 CHARS:  BASE STATION                                                 
*        2 CHARS:  GROUP/SUBGROUP                                               
*        1 CHAR :  ACE/GRAPHNET INDICATOR                                       
*        SPARE     5 CHARACTERS                                                 
*        12 X 400  =  4800                                                      
*                                                                               
         A     RF,=F'6000'         SIZE OF STATION GROUP/SUBGRP TABLE           
         ST    RF,ASALAREA         SET A(SALESPERSON AREA)                      
*                                                                               
*   LEAVE SUFFICIENT SPACE IN SALESPERSON TABLE FOR 2,000 ENTRIES.              
*        EACH ENTRY:  (16 CHARS)                                                
*        4 CHARS:  JDS CODE                                                     
*        2 CHARS:  JDS DIVISION/TEAM                                            
*        2 CHARS:  JDS OFFICE                                                   
*        3 CHARS:  DDS CODE                                                     
*        2 CHARS:  DDS DIVISION/TEAM                                            
*        3 CHARS:  SPARE                                                        
*                                                                               
         A     RF,=F'32000'        2ND S/P TABLE                                
         ST    RF,A2SPAREA         SET A(2ND S/P AREA)                          
         ST    RF,A2SPEND          SET A(NEXT SLOT)                             
*                                                                               
*   LEAVE SUFFICIENT SPACE IN 2ND S/P TABLE FOR 6,000 ENTRIES.                  
*        EACH ENTRY:  (3 CHARS)                                                 
*        3 CHARS:  DDS CODE                                                     
*                                                                               
         A     RF,=F'18000'        MISSING STATION TABLE                        
         ST    RF,AMISAREA         SET A(MISSING STATION AREA)                  
         ST    RF,ANEXTMIS         SET A(NEXT MISSING SLOT)                     
*                                                                               
*                                                                               
*   LEAVE SUFFICIENT SPACE IN MISSING STATION                                   
*        TABLE FOR 2000 BYTES.                                                  
         A     RF,=F'2000'         NEWLY ADDED SALESPERSON TABLE                
         ST    RF,ANEWSALS         SET A(NEWLY ADDED S/P AREA)                  
         ST    RF,ANXTSAL          SET A(NEXT ADDED SLOT)                       
*                                                                               
*   LEAVE SUFFICIENT SPACE IN NEW SALESPERSON                                   
*        TABLE FOR 10000 ENTRIES: 30,000 BYTES                                  
         A     RF,=F'30000'                                                     
         ST    RF,ASPNAREA         SET A(SALESPERSON NAME AREA)                 
         ST    RF,ASPNEND                                                       
*                                                                               
*   LEAVE SUFFICIENT SPACE IN SALESPERSON NAME TABLE FOR ??? ENTRIES            
*        OF 50 CHARS EACH:                                                      
*        CHARS 1 - 8 :  JDS S/P CODE - DIV/TEAM - OFFICE                        
*        CHAR  9 - 38:  JDS S/P NAME (1ST 30 CHARS)                             
*        CHAR 29 - 50:  JDS S/P TELEPHONE NUMBER                                
*                                                                               
*****>>>>>>>>   NEW TABLE ENTRIES GET ADDED HERE!!                              
*                                                                               
         MVC   COMMSKIP,QRECORD+34 SAVE 'COMMENT SKIP' FLAG                     
         MVC   PETRYREP,QRECORD+36                                              
*                                  SAVE OUTPUT NATIONAL REP                     
         MVC   PETRYRP2,QRECORD+38                                              
*                                  SAVE SOURCE REP                              
         MVC   PETRYRPL,QRECORD+40                                              
*                                  SAVE OUTPUT LOCAL REP                        
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
         MVC   BUCKWORK(4),GETBROAD                                             
*                                  SET UP BUCKET UPDATE ARGUMENTS               
         MVC   BUCKWORK+4(4),GETDAY                                             
         MVC   BUCKWORK+8(4),ADDAY                                              
         MVC   BUCKWORK+12(4),DATCON                                            
         CLC   QRECORD+20(12),SPACES                                            
*                                  ANY DISPLAY VALUES?                          
         BE    INIT0060            NO                                           
         PACK  DUB,QRECORD+20(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,LOWCTR           SAVE LOW VALUE                               
         PACK  DUB,QRECORD+26(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,HIGHCTR          SAVE HIGH VALUE                              
         LA    RF,RECORD3                                                       
         A     RF,=F'1024'         SET A(RECORD4)                               
         ST    RF,ARECORD4                                                      
         A     RF,=F'1024'         SET A(RECORD5)                               
         ST    RF,ARECORD5                                                      
*                                                                               
INIT0060 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TABLINIT:                                                                   
*              INSERT STATION CALLS + REP CODE + GROUP/SUBGROUP +               
*                ACE/GRAPHNET INDICATOR                                         
*        INTO TABLE FOR ASSIGNING PETRY CONTRACTS.                              
*                                                                               
         DS    0F                                                               
TABLINIT NMOD1 0,*TAB*                                                          
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         USING FILED,RC                                                         
*                                                                               
*                                                                               
*        MVC   P+1(17),=C'ENTERING STATABLE'                                    
*        GOTO1 REPORT                                                           
         LA    RF,RECORD           SET A(IO AREA FOR PROCEDURE)                 
         ST    RF,AIOAREA                                                       
         LA    R2,STAREPS          A(REPS TO SCAN)                              
         L     R3,ASTAAREA         STATION AREA                                 
STAT0020 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               SET RECORD TYPE                              
*                                                                               
*        MVC   P+1(16),=C'START OF COMPANY'                                     
*        MVC   P+18(2),0(R2)                                                    
*        GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY+20(2),PETRYRP2  INSERT REP CODE INTO KEY                     
         GOTO1 HIGH1               READ FIRST RECORD                            
         B     STAT0060                                                         
STAT0040 EQU   *                                                                
         GOTO1 SEQ1                READ NEXT RECORD                             
STAT0060 EQU   *                                                                
         CLI   QUESTOR+7,C'Y'      DISPLAY STATION CODES?                       
         BE    STAT0065            YES                                          
         CLI   QUESTOR+7,C'A'      DISPLAY STATION CODES+MISSING STNS?          
         BNE   STAT0070            NO                                           
STAT0065 EQU   *                                                                
         CLC   STACTR,=F'100'      CHECK 1ST 100 ENTRIES                        
         BH    STAT0070                                                         
*        MVC   P+1(8),=C'STA READ'                                              
*        MVC   P+10(34),KEY                                                     
*        GOTO1 REPORT                                                           
STAT0070 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME REC TYPE/REP?                           
         BNE   STAT0800            NO  - FINISHED                               
STAT0080 EQU   *                                                                
         GOTO1 GREC1               RETRIEVE RECORD                              
         MVC   0(4,R3),RSTAKSTA    INSERT STATION LETTERS                       
         MVC   4(2,R3),RSTAGRUP    INSERT GROUP/SUBGROUP                        
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   STAT0120            NO X'05' - NOT ACE/GRAPHNET                  
         OC    10(2,R6),10(R6)     IS THERE A RECEIVING ID?                     
         BZ    STAT0120            NO  - NOT ACE/GRAPHNET                       
         CLC   10(2,R6),=X'0406'   GRAPHNET?                                    
         BNE   *+12                                                             
         OI    6(R3),X'40'         YES - MARK STATION 'GRAPHNET'                
         B     STAT0120                                                         
         OI    6(R3),X'80'         NO  - MARK STATION 'ACE'                     
STAT0120 EQU   *                                                                
         CLI   QUESTOR+7,C'Y'      DISPLAY STATION CODES?                       
         BNE   STAT0160            NO                                           
         MVC   P+1(14),=C'STATION ENTRY:'                                       
         MVC   P+20(12),0(R3)                                                   
         GOTO1 REPORT                                                           
STAT0160 EQU   *                                                                
*                                                                               
         ST    R3,ASTAEND          SAVE LAST STATION ADDR                       
         LA    R3,12(R3)           BUMP STATION TABLE ADDR                      
         L     RF,STACTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,STACTR                                                        
         B     STAT0040            GO BACK FOR NEXT STATION                     
STAT0800 EQU   *                                                                
         MVC   P+1(17),=C'LEAVING  STATABLE'                                    
         MVC   P+20(06),=C'COUNT='                                              
         EDIT  STACTR,(5,P+27)                                                  
         GOTO1 REPORT                                                           
         B     SALTABLE                                                         
*                                                                               
*   STAREPS IS SET UP FOR TESTING.                                              
*   THIS MUST BE CHANGED FOR THE LIVE RUN, TO BRING IN THE STATIONS             
*      FROM THE ACTUAL COMPANIES!!                                              
*                                                                               
*TAREPS  DC    C'V1'               PETRY                                        
         DC    X'0000'                                                          
STAREPS  DC    C'PV'               PETRY PRODUCTION                             
         DC    X'0000'                                                          
*                                                                               
         EJECT                                                                  
*   SALTABLE:  INSERT SALESPERSON INFORMATION INTO TABLE, THEN SORT             
*        BY ORIGINAL PETRY CODE.                                                
*        TABLE IS CONSTRUCTED:                                                  
*            BYTES 1-4    =  ORIGINAL PETRY CODE                                
*            BYTES 5-6    =  ORIGINAL PETRY DIV/TEAM CODE                       
*            BYTES 7-8    =  ORIGINAL PETRY OFFICE CODE                         
*            BYTES 9-11   =  EQUIVALENCY CODE                                   
*            BYTES 12-13  =  SALESPERSON TEAM                                   
*            BYTES 14-15  =  SALESPERSON OFFICE                                 
*                                                                               
SALTABLE EQU   *                                                                
*        MVC   P+1(17),=C'ENTERING SALTABLE'                                    
*        GOTO1 REPORT                                                           
         L     R6,ARECORD5         SET ADDRESSABILITY TO S/P RECORD             
         ST    R6,AIOAREA                                                       
         USING RSALREC,R6                                                       
*                                                                               
         L     R3,ASALAREA         SALESPERSON AREA                             
         L     R4,A2SPAREA         2ND S/P AREA                                 
         MVI   15(R3),1            STUFF FIRST ENTRY WITH MISCELL               
         MVI   02(R4),1            STUFF FIRST ENTRY WITH MISCELL               
         LA    R3,16(R3)           BUMP TO 2ND ENTRY OF S/P TABLE               
         LA    R4,03(R4)           BUMP TO 2ND ENTRY OF 2ND S/P TAB             
         LA    R2,STAREPS          SET A(REPS INVOLVED IN CONVERSION)           
SALT0020 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,6               SET RECORD TYPE                              
         MVC   KEY+22(2),0(R2)     INSERT COMPANY INTO KEY                      
         GOTO1 HIGH1               READ FIRST RECORD                            
         B     SALT0060                                                         
SALT0040 EQU   *                                                                
         GOTO1 SEQ1                READ NEXT RECORD                             
SALT0060 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     SAME REC TYPE/REP?                           
         BNE   SALT0120            NO  - FINISHED                               
         GOTO1 GREC1               RETRIEVE RECORD                              
         MVC   0(3,R4),RSALKSAL    INSERT SALESPERSON CODE INTO                 
*                                     2ND S/P TABLE                             
         LA    R4,3(R4)            BUMP TO NEXT POSITION                        
*                                     OF SECOND TABLE                           
         L     RF,SP2CTR                                                        
         LA    RF,1(RF)            INCREMENT COUNTER                            
         ST    RF,SP2CTR           SAVE COUNTER                                 
*                                                                               
         OC    RSALEQU,RSALEQU     ANY ORIGINAL PETRY VALUE?                    
         BZ    SALT0040            NO  - SKIP THIS RECORD                       
         MVC   0(8,R3),RSALEQU     INSERT SP/DIV-TEAM/OFFICE                    
*                                  INSERT ORIGINAL SALESPERSON CODE             
         MVC   8(3,R3),RSALKSAL    INSERT SALESPERSON CODE                      
         MVC   11(2,R3),RSALTEAM   INSERT NEW SALESPERSON DIV/TEAM              
         MVC   13(2,R3),RSALOFF    INSERT NEW SALESPERSON OFFICE                
         CLI   QUESTOR+8,C'Y'      DISPLAY SALESPERSON CODES?                   
         BNE   SALT0115            NO                                           
         CLC   SALCTR,=F'100'      DISPLAY ONLY 1ST 100                         
         BH    SALT0115                                                         
         MVC   P+1(10),=C'S/P: ORIG:'                                           
         MVC   P+14(8),0(R3)                                                    
         MVC   P+22(08),=C'DDS EQU:'                                            
         MVC   P+33(7),8(R3)                                                    
         GOTO1 REPORT                                                           
SALT0115 EQU   *                                                                
         ST    R3,ASALEND          SAVE LAST SALESPERSON ADDR                   
         LA    R3,16(R3)           BUMP SALSPERSON TABLE ADDR                   
         L     RF,SALCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,SALCTR                                                        
         B     SALT0040            GO BACK FOR NEXT SALESPERSON                 
SALT0120 EQU   *                                                                
         MVC   0(3,R4),=X'FFFFFF'  FLAG LAST ENRY IN 2ND TABLE                  
         L     RF,SP2CTR           SET NUMBER OF RECORDS                        
         LA    RF,2(RF)            INCREMENT FOR CONTROL RECORDS                
*                                     (FIRST/LAST RECORD PADS)                  
         ST    RF,SP2CTR           RESTORE IT                                   
         MVC   0(4,R3),=X'FFFFFFFF' FLAG LAST ENRY                              
         L     R2,ASALAREA         SET A(SALESPERSON TABLE)                     
         L     R3,SALCTR           SET NUMBER OF RECORDS                        
         LA    R3,2(R3)            INCREMENT FOR CONTROL RECORDS                
*                                     (FIRST/LAST RECORD PADS)                  
         ST    R3,SALCTR           RESTORE IT                                   
*                                  SORT TABLE OF EQUIV S/P CODES                
         GOTO1 =V(QSORT),DMCB,(R2),(R3),16,08,0                                 
*                                  PUT TABLE IN PETRY SEQUENCE                  
SALT0160 EQU   *                                                                
         CLI   QUESTOR+8,C'Y'      DISPLAY SALESPERSON CODES?                   
         BE    SALT0180            YES                                          
         CLI   QUESTOR+8,C'A'      DISPLAY SALESPERSON CODES?                   
         BNE   SALT0240            NO                                           
SALT0180 EQU   *                                                                
         L     R2,ASALAREA         YES - LOAD A(TABLE)                          
SALT0200 EQU   *                                                                
         MVC   P+1(03),=C'S/P'                                                  
         MVC   P+5(16),0(R2)       MOVE TABLE ENTRY                             
         GOTO1 REPORT                                                           
         LA    R2,16(R2)                                                        
         OC    0(16,R2),0(R2)      ANY ENTRY?                                   
         BNZ   SALT0200            YES                                          
         GOTO1 REPORT              NO  - PUT OUT A BLANK                        
*                                  NOW DISPLAY 2ND S/P TABLE                    
         L     R2,A2SPAREA         YES - LOAD A(TABLE)                          
SALT0220 EQU   *                                                                
         MVC   P+1(03),=C'2ND'                                                  
         MVC   P+5(03),0(R2)       MOVE TABLE ENTRY                             
         GOTO1 REPORT                                                           
         LA    R2,03(R2)                                                        
         OC    0(03,R2),0(R2)      ANY ENTRY?                                   
         BNZ   SALT0220            YES                                          
         GOTO1 REPORT              NO  - PUT OUT A BLANK                        
SALT0240 EQU   *                                                                
*        MVC   P+1(17),=C'LEAVING  SALTABLE'                                    
*        GOTO1 REPORT                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         B     AGYTABLE                                                         
         EJECT                                                                  
*                                                                               
*   AGYTABLE:  READ ALL AGENCY RECORDS.  DISPLACE INTO TABLE AND                
*        SET BYTE TO 'Y' IF AGENCY CODE EXISTS.  ONLY PROCESS                   
*        ALL-NUMERIC AGENCY CODES.                                              
*                                                                               
AGYTABLE EQU   *                                                                
         MVC   P+1(17),=C'ENTERING AGYTABLE'                                    
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+20(16),=C'START TIME     :'                                    
         GOTO1 HEXOUT,DMCB,RUNEND,P+40,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         LA    RF,RECORD           SET A(IO AREA FOR PROCEDURE)                 
         ST    RF,AIOAREA                                                       
AGYT0020 EQU   *                                                                
*                                  DON'T USE ANY AGENCY RECORDS                 
         L     R3,AAGYAREA         PRIME AGENCY AREA WITH                       
*                                     DUMMY RECORDS: LOW AND HIGH               
         MVI   11(R3),1            SET LOW RECORD                               
         LA    R3,12(R3)           BUMP TO NEXT SLOT                            
         MVC   0(6,R3),=X'FFFFFFFFFFFF'                                         
*                                  SET HIGH RECORD                              
         LA    RF,2                                                             
         ST    RF,AGYCTRIN         SAVE COUNT OF 2                              
         B     AGYT0200            DON'T USE ANY AGENCY RECORDS                 
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'0A'           SET RECORD TYPE                              
         GOTO1 HIGH1               READ FIRST RECORD                            
         B     AGYT0060                                                         
AGYT0040 EQU   *                                                                
         GOTO1 SEQ1                READ NEXT RECORD                             
AGYT0060 EQU   *                                                                
         CLI   KEY,X'0A'           AGENCY RECORD?                               
         BNE   AGYT0200            NO  - FINISHED                               
         CLC   KEY+25(2),PETRYRP2 AGENCY IN QUESTION?                           
         BNE   AGYT0040            NO  - SKIP THE RECORD                        
         L     R3,AAGYEND          LOAD A(NEXT SLOT IN TABLE)                   
         MVC   0(6,R3),KEY+19      INSERT AGENCY/AGYOFF                         
*                                     INTO 1ST (PETRY) POSITIONS                
         MVC   6(6,R3),KEY+19      INSERT AGENCY/AGYOFF                         
*                                     INTO 2ND (DDS)   POSITIONS                
         L     RF,AGYCTRIN                                                      
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTRIN         INCREMENT AGENCY TABLE COUNTER               
         MVC   KEYSAV2,KEY         SAVE KEY FOR RESTART                         
         MVI   KEY,X'1A'                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     SECONDARY RECORD FOUND?                      
         BNE   AGYT0100            NO  - USE AGENCY CODE FOR BOTH               
AGYT0080 EQU   *                                                                
         GOTO1 GREC1               RETRIEVE SECONDARY RECORD                    
         CLC   RAGY2EQU,SPACES     EQUIV ENTRY?                                 
         BE    AGYT0100            NO                                           
         OC    RAGY2EQU,RAGY2EQU   DITTO                                        
         BZ    AGYT0100            NO                                           
         MVC   0(4,R3),RAGY2EQU    INSERT AGENCY EQUIV                          
         MVC   4(2,R3),SPACES      INSERT SPACES INTO EQUIV OFFICE              
         CLC   RAGY2EQO,SPACES     EQUIV OFFICE ENTRY?                          
         BE    AGYT0100                                                         
         OC    RAGY2EQO,RAGY2EQO   DITTO                                        
         BZ    AGYT0100                                                         
         MVC   4(2,R3),RAGY2EQO    INSERT AGENCY EQUIV OFFICE                   
AGYT0100 EQU   *                                                                
         LA    R3,12(R3)           BUMP TO NEXT SLOT                            
         ST    R3,AAGYEND          SAVE A(NEXT SLOT)                            
         MVC   KEY,KEYSAV2         RESTART KEY                                  
         GOTO1 HIGH                RESTART FOR READ                             
         B     AGYT0040                                                         
AGYT0200 EQU   *                                                                
         MVC   P+1(19),=C'AGENCY TABLE COUNT:'                                  
         EDIT  AGYCTRIN,(6,P+21)                                                
         GOTO1 REPORT                                                           
*                                                                               
         L     R2,AAGYAREA         SET A(AGENCY TABLE)                          
         L     R3,AGYCTRIN         SET NUMBER OF RECORDS                        
         GOTO1 =V(QSORT),DMCB,(R2),(R3),12,6,0                                  
*                                  SORT TABLE INTO EQUIV SEQUENCE               
         CLI   QUESTOR+6,C'Y'      DISPLAY AGENCY TABLE?                        
         BE    AGYT0250            YES                                          
         CLI   QUESTOR+6,C'A'      DISPLAY AGENCY TABLE+ NEW O/P?               
         BNE   AGYT0300            NO                                           
AGYT0250 EQU   *                                                                
         L     R3,AAGYAREA         YES - SET A(AGENCY AREA)                     
         LA    R0,100              DO FIRST 100 ENTRIES                         
AGYT0260 EQU   *                                                                
         MVC   P+1(12),0(R3)                                                    
         GOTO1 REPORT                                                           
         LA    R3,12(R3)           BUMP TO NEXT ENTRY                           
         BCT   R0,AGYT0260         GO BACK FOR NEXT                             
*                                                                               
AGYT0300 EQU   *                                                                
         L     RF,AAG2AREA         PRIME 'MISSING AGENCY' TABLE                 
         XC    0(6,RF),0(RF)       SET FIRST ENTRY TO BIN ZERO                  
         MVC   6(6,RF),=X'FFFFFFFFFFFF' SET 2ND ENTRY TO HI-VALUE               
         LA    RF,2                                                             
         ST    RF,AG2CTR           SET COUNTER TO 2 ENTRIES                     
*                                                                               
         MVC   P+1(17),=C'LEAVING  AGYTABLE'                                    
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+20(16),=C'END   TIME     :'                                    
         GOTO1 HEXOUT,DMCB,RUNEND,P+40,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
*        COMMUNICATION WITH DATA MANAGER (DIRECTORY)                            
         SPACE                                                                  
READ1    MVC   COMMAND(8),DMREAD                                                
         B     DIRCTRY1                                                         
         SPACE 2                                                                
SEQ1     MVC   COMMAND(8),DMRSEQ                                                
         B     DIRCTRY1                                                         
         SPACE 2                                                                
HIGH1    MVC   COMMAND(8),DMRDHI                                                
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY1                                                         
         SPACE 2                                                                
ADD1     MVC   COMMAND(8),DMADD                                                 
         B     DIRCTRY1                                                         
         SPACE 2                                                                
WRITE1   MVC   COMMAND(8),DMWRT                                                 
         B     DIRCTRY1                                                         
         SPACE 2                                                                
DIRCTRY1 NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         B     RGENIOD1                                                         
         EJECT                                                                  
*        COMMUNICATION WITH DATA MANAGER (FILE)                                 
         SPACE 3                                                                
GREC1    MVC   COMMAND(8),GETREC                                                
         B     FILE1                                                            
         SPACE 2                                                                
PREC1    MVC   COMMAND(8),PUTREC                                                
         B     FILE1                                                            
         SPACE 2                                                                
AREC1    MVC   COMMAND(8),ADDREC                                                
         B     FILE1                                                            
         SPACE 2                                                                
FILE1    NTR1                                                                   
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         IC    R4,DMINBTS                                                       
         ICM   R5,15,AIOAREA                                                    
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',                X        
               (R2),(R5),DMWORK                                                 
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
RGENIOD1 OC    DMCB+8(1),DMCB+8                                                 
*                                                                               
         XIT1                      RETURN                                       
*                                                                               
*                                                                               
* * * * * * * * * END OF INCLUDE DATASET RGENIO * * * * * * * * * *             
         LTORG                                                                  
         EJECT                                                                  
         DROP  RC                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033REREPP102 05/01/02'                                      
         END                                                                    
