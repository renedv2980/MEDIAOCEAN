*          DATA SET REREP102A  AT LEVEL 078 AS OF 05/01/02                      
*PHASE REP102A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
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
*     QUESTOR+0   =                                                *            
*     QUESTOR+1   =                                                *            
*     QUESTOR+2   =                                                *            
*     QUESTOR+3   =                                                *            
*     QUESTOR+4   =                                                *            
*                 =                                                *            
*     QUESTOR+5   =                                                *            
*     QUESTOR+6   =                                                *            
*     QUESTOR+7   =                                                *            
*     QUESTOR+8   =                                                *            
*     QUESTOR+9   =                                                *            
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
         BNE   MAIN0900                                                         
*                                                                               
MAIN0020 DS    0H                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTRT          SAVE START TIME                              
*                                                                               
         GOTO1 =A(INITIAL),DMCB,(RC),RR=Y                                       
         GOTO1 =A(TABLINIT),DMCB,(RC),RR=Y                                      
*                                                                               
MAIN0040 EQU   *                                                                
         GOTO1 =A(GETTPREC),DMCB,(RC),RR=Y                                      
*                                  UNBLOCK AND DELIVER TAPE REC                 
         BZ    MAIN0100            CC = ZERO = END OF FILE                      
         L     RF,TOTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR           INCREMENT TOTAL RECORDS READ                 
         LA    RF,RECTABLE         IS RECORD TO BE PROCESSED?                   
MAIN0041 EQU   *                                                                
         CLC   RECORD2(2),=X'0020' LAST RECORD TO BE CONVERTED                  
*                                     COMPLETED?                                
         BH    MAIN0100            YES - FINISHED                               
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    MAIN0040            YES - SKIP THIS RECORD TYPE                  
         CLC   0(2,RF),RECORD2     CHECK RECORD TYPE AGAINST TABLE              
         BE    MAIN0042            PROCESS                                      
         LA    RF,2(RF)            BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0041            GO BACK FOR NEXT                             
*                                                                               
RECTABLE EQU   *                                                                
*NOT CON DC    X'0001'             REP       RECORD                             
*NOT CON DC    X'0002'             STATION   RECORD                             
*NOT CON DC    X'0003'             OFFICE    RECORD                             
*NOT CON DC    X'0004'             S/P       RECORD                             
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
MAIN0042 EQU   *                                                                
         CLC   =X'0001',RECORD2    REP       RECORD FOUND?                      
         BNE   MAIN0043            NO                                           
         GOTO1 REPRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0043 EQU   *                                                                
         CLC   =X'0002',RECORD2    STATION   RECORD FOUND?                      
         BNE   MAIN0044            NO                                           
         GOTO1 STARPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0044 EQU   *                                                                
         CLC   =X'0003',RECORD2    OFFICE    RECORD FOUND?                      
         BNE   MAIN0045            NO                                           
         GOTO1 OFFRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0045 EQU   *                                                                
         CLC   =X'0004',RECORD2    S/P       RECORD FOUND?                      
         BNE   MAIN0046            NO                                           
         GOTO1 SALRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0046 EQU   *                                                                
         CLC   =X'0005',RECORD2    GROUP     RECORD FOUND?                      
         BNE   MAIN0047            NO                                           
         GOTO1 GRORPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0047 EQU   *                                                                
         CLC   =X'0006',RECORD2    REGION    RECORD FOUND?                      
         BNE   MAIN0048            NO                                           
         GOTO1 REGRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0048 EQU   *                                                                
         CLC   =X'0007',RECORD2    DIVISION  RECORD FOUND?                      
         BNE   MAIN0049            NO                                           
         GOTO1 DIVRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0049 EQU   *                                                                
         CLC   =X'000A',RECORD2    AGENCY    RECORD FOUND?                      
         BNE   MAIN0050            NO                                           
         GOTO1 AGYRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0050 EQU   *                                                                
         CLC   =X'000B',RECORD2    ADVERT    RECORD FOUND?                      
         BNE   MAIN0051            NO                                           
         GOTO1 ADVRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0051 EQU   *                                                                
         CLC   =X'0010',RECORD2    CONTRACT  RECORD FOUND?                      
         BNE   MAIN0052            NO                                           
         GOTO1 CONRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0052 EQU   *                                                                
         CLC   =X'0020',RECORD2    BUY       RECORD FOUND?                      
         BNE   MAIN0053            NO                                           
         GOTO1 BUYRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0053 EQU   *                                                                
         CLC   =X'002B',RECORD2    CATEGORY  RECORD FOUND?                      
         BNE   MAIN0054            NO                                           
         GOTO1 CATRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0054 EQU   *                                                                
         CLC   =X'002C',RECORD2    CLASS     RECORD FOUND?                      
         BNE   MAIN0055            NO                                           
         GOTO1 CLSRPROC,DMCB,(RC)                                               
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0055 EQU   *                                                                
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN0100 EQU   *                                                                
         CLI   BUYREC4,C'Y'        BUY RECORD IN RECORD4?                       
         BNE   MAIN0200            NO                                           
         GOTO1 PUTRECS,DMCB,(RC),RECORD4                                        
*                                  NO  - PUT RECORD4 TO OUTPUT                  
         L     RF,BUYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
         GOTO1 DISPPUT,DMCB,(RC)                                                
MAIN0200 EQU   *                                                                
         GOTO1 =A(DISPTOTS),DMCB,(RC),RR=RELO                                   
*                                  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         CLOSE (INTAPE,REWIND)                                                  
*                                                                               
MAIN0900 XIT1                                                                   
*                                  PETRY TV DIVISIONS                           
*                                  BYTE 1   =  PETRY DIVISION CODE              
*                                  BYTE 2-3 =  CONVERT REP                      
*                                  BYTE 4-5 =  GROUP/SUBGROUP DEFAULT           
*    NOTE:                         CODES USED HERE HAVE NOT BEEN                
*                                  FINALIZED - PLEASE REVIEW                    
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
*  AGYRPROC:  PROCESS THE AGENCY    RECORD.                      *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
AGYRPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XCEFL RECORD,1008         CLEAR THE RECORD BUILD AREA                  
         MVC   RAGYLEN(2),=X'9C'                                                
         MVI   RAGYKTYP,X'0A'      INSERT RECORD TYPE                           
         MVC   RAGYKAGY(6),JAGYKAGY                                             
*                                  INSERT AGENCY/AGENCY OFFICE                  
         MVC   RAGYKREP,JAGYKREP   INSERT REP CODE                              
*                                                                               
         MVC   RAGYELEM(2),=X'017A'                                             
*                                  INSERT ELT CODE/LENGTH                       
         MVC   RAGYNAM1,JAGYNAM1   INSERT NAME 1                                
         MVC   RAGYNAM2,JAGYNAM2   INSERT NAME 2                                
         MVC   RAGYADD1,JAGYADD1   INSERT ADDRESS 1                             
         MVC   RAGYADD2,JAGYADD2   INSERT ADDRESS 2                             
         GOTO1 PUTRECS,DMCB,(RC),RECORD                                         
         L     RF,AGYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR                                                        
         GOTO1 DISPPUT,DMCB,(RC)                                                
         XC    RAGYELEM(200),RAGYELEM                                           
*                                  CLEAR LOWER PART OF RECORD                   
         MVC   RAGYLEN(2),=X'56'                                                
         MVI   RAGYKTYP,X'1A'      INSERT NEW RECORD TYPE                       
         MVC   RAGYELEM(2),=X'1034'                                             
*                                  INSERT ELT CODE/LENGTH                       
         GOTO1 PUTRECS,DMCB,(RC),RECORD                                         
         L     RF,AGYCTR2                                                       
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR2                                                       
         GOTO1 DISPPUT,DMCB,(RC)                                                
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
         MVC   RADVLEN(2),=X'64'                                                
         MVI   RADVKTYP,X'08'      INSERT RECORD TYPE                           
         MVC   RADVKADV(4),JADVKADV                                             
*                                  INSERT ADVERT CODE                           
         MVC   RADVKREP,JADVKREP   INSERT REP CODE                              
*                                                                               
         MVC   RAGYELEM(2),=X'0142'                                             
*                                  INSERT ELT CODE/LENGTH                       
         MVC   RADVNAME,JADVNAME   INSERT NAME                                  
*                                                                               
*   NEED CONVERT LIST FOR ADVERTISER CATEGORY CODES                             
*                                                                               
         GOTO1 PUTRECS,DMCB,(RC),RECORD                                         
         L     RF,ADVCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ADVCTR                                                        
         GOTO1 DISPPUT,DMCB,(RC)                                                
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
         XCEFL RECORD,1008         CLEAR THE RECORD BUILD AREA                  
         MVC   RCONLEN(2),=X'005E' SET INITIAL LENGTH                           
         MVI   RCONKTYP,X'0C'      INSERT RECORD TYPE                           
         MVC   RCONKREP,JCONKREP   INSERT REP CODE                              
         MVC   RCONKGRP,JCONKGRP   INSERT GROUP/SUBGROUP                        
         MVC   RCONKSTA(4),JCONKSTA                                             
*                                  INSERT STATION                               
         MVI   RCONKSTA+4,C' '     INSERT SPACE AS MEDIA                        
         MVC   RCONKOFF,JCONKOFF   INSERT OFFICE CODE                           
         MVC   RCONKAGY(6),JCONKAGY                                             
*                                  INSERT AGENCY/AGENCY OFFICE                  
         MVC   RCONKADV,JCONKADV   INSERT ADVERTISER                            
         MVC   RCONKCON,JCONKCON   INSERT CONTRACT NUMBER                       
*                                                                               
         MVC   RCONELEM,=X'013C'   INSERT DESCRIP ELT CODE/LEN                  
         MVC   RCONBUYR,SPACES                                                  
         MVI   JDSELT,X'20'        GET BUYER NAME ELEMENT                       
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   CONR0040            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,CONR0020         MOVE BUYER NAME BY LENGTH                    
         B     CONR0040                                                         
CONR0020 EQU   *                                                                
         MVC   RCONBUYR(0),1(RF)   MOVE BUYER BY LENGTH                         
CONR0040 EQU   *                                                                
         MVC   RCONPRD,SPACES      CLEAR THE PRODUCT CODE                       
*                                                                               
*   DOES JDS PERMIT PRODUCT CODES?                                              
*                                                                               
         MVC   ELTBILD1(2),=C'0516' BUILD PRODUCT NAME ELEMENT                  
         MVC   ELTBILD1+2(20),SPACES                                            
         MVI   JDSELT,X'30'        GET PRODUCT NAME ELEMENT                     
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   CONR0080            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,CONR0060         MOVE PRODUCT NAME BY LENGTH                  
         GOTO1 HELOCON1            INSERT ELEMENT, ERASE ELEMENT                
         B     CONR0080                                                         
CONR0060 EQU   *                                                                
         MVC   ELTBILD1+2(0),1(RF)                                              
CONR0080 EQU   *                                                                
         MVC   RCONTEM,JCONKDIV    INSERT DIVISION/TEAM                         
*                                                                               
*   NEED TO TABLE SEARCH FOR SALESPERSON CODE :  DIFF SIZES                     
*                                                                               
******************************************************************              
         MVC   RCONSAL,JCONKSAL    INSERT SALESPERSON CODE                      
******************************************************************              
*                                                                               
*                                                                               
         GOTO1 JDSDATE,DMCB,JCONSDTE,RCONDATE,(2,0)                             
         GOTO1 JDSDATE,DMCB,JCONEDTE,RCONDATE+3,(2,0)                           
         MVC   RCONMOD,JCONMOD     INSERT MOD #                                 
         GOTO1 JDSDATE,DMCB,JCONMODD,RCONMODD,(2,0)                             
*                                                                               
*   SET RCONMODR HERE                                                           
*                                                                               
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
         BE    CONR0140            YES                                          
         TM    JCONOPT,X'40'       ARB?                                         
         BNE   CONR0140            NO                                           
         MVI   RCONRTGS,C'A'       SET TO 'ARB'                                 
CONR0140 EQU   *                                                                
**       MVC   RCONINVD,???        LAST INVOICE CHANGE DATE                     
         MVC   RCONWKS,JCONWKS     INSERT NUMBER OF WEEKS                       
*                                                                               
*   DESCRIPTOR ELEMENT SHOULD BE DIRECTLY ACCESSIBLE.                           
*                                                                               
*                                                                               
*   NEED LIST OF CONTRACT TYPES FOR CONVERSION.                                 
*                                                                               
         LA    RF,TYPTABLE                                                      
CONR0160 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    CONR0200            YES - NOT FOUND                              
         CLC   JCONTYPE,0(RF)      IN TABLE?                                    
         BE    CONR0180            YES - USE VALUE                              
         LA    RF,3(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     CONR0160            GO BACK FOR NEXT                             
CONR0180 EQU   *                                                                
         MVC   RCONTYPE,2(RF)      INSERT CONTRACT TYPE FROM TABLE              
         B     CONR0200                                                         
*                                                                               
*   TYPTABLE:  THREE-BYTE ENTRIES:                                              
*        1 - 2 = PETRY CODE                                                     
*        3     = DDS EQUIVALENT                                                 
*                                                                               
TYPTABLE DC    C'XXY'                                                           
         DC    C'ZZ1'                                                           
         DC    X'0000'                                                          
CONR0200 EQU   *                                                                
         MVI   JDSELT,X'05'        GET PRODUCT NAME ELEMENT                     
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   CONR0120            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
*                                                                               
*   NEED LIST OF CONTRACT TYPES FOR CONVERSION.                                 
*                                                                               
         LA    RF,CATTABLE                                                      
CONR0220 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    CONR0260            YES - NOT FOUND                              
         CLC   2(4,RE),0(RF)       IN TABLE?                                    
         BE    CONR0240            YES - USE VALUE                              
         LA    RF,6(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     CONR0220            GO BACK FOR NEXT                             
CONR0240 EQU   *                                                                
         MVC   RCONTYPE,2(RF)      INSERT CONTRACT TYPE FROM TABLE              
         B     CONR0260                                                         
*                                                                               
*   CATTABLE:  SIX-BYTE ENTRIES:                                                
*        1 - 4 = PETRY CODE                                                     
*        5 - 6 = DDS EQUIVALENT                                                 
*                                                                               
CATTABLE DC    C'XXXXYY'                                                        
         DC    C'ZZZZ11'                                                        
         DC    X'0000'                                                          
CONR0260 EQU   *                                                                
         LA    R0,2                SET LOOP CONTROL: 2 MAX                      
         MVI   JDSELT,X'01'        GET CONTRACT COMMENT ELT                     
         GOTO1 GETJDSEL,DMCB,(RC)                                               
CONR0280 EQU   *                                                                
         BNZ   CONR0340            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
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
         GOTO1 HELOCON1            INSERT ELEMENT, ERASE ELEMENT                
         GOTO1 GETJDSNX,DMCB,(RC)  GET NEXT ELEMENT                             
         BCT   R0,CONR0280         GO BACK AND CHECK IF FOUND                   
         B     CONR0340                                                         
CONR0320 EQU   *                                                                
         MVC   ELTBILD1+2(0),2(RF) INSERT COMMENT BY LENGTH                     
CONR0340 EQU   *                                                                
         MVI   JDSELT,X'B0'        GET ESTIMATE $$ ELEMENT                      
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     CONR0380                                                         
CONR0360 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
CONR0380 EQU   *                                                                
         BNZ   CONR0400            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         L     RE,JDSLEN           SET L(JDS DATA)                              
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
         L     RF,JDSADDR          SET A(JDS ELT)                               
         L     RE,JDSLEN           SET L(JDS DATA)                              
         GOTO1 DOLBUCK,DMCB,(RC),(4,0)                                          
         B     CONR0420            GO BACK FOR NEXT ELEMENT                     
CONR0460 EQU   *                                                                
         GOTO1 COMPELEM,DMCB,(RC)                                               
*                                  GENERATE THE COMPETITIVE ELEMENT             
         GOTO1 COMPCOMT,DMCB,(RC)                                               
*                                  GENERATE THE COMPET COMMENT ELT              
         MVI   ELTBILD1,9          BUILD EXTRA SPL ELEMENT                      
         MVI   ELTBILD1+1,11       SET ELEMENT LENGTH                           
         MVI   ELTBILD1+10,X'60'                                                
*                                  SET $0 FCST $0 BUDGET ENTERED                
         GOTO1 HELOCON1 INSERT ELEMENT                                          
*                                                                               
         GOTO1 BUILDSAR,DMCB,(RC)                                               
*                                  BUILD/INSERT THE SAR ELEMENT                 
         GOTO1 EXCONELT,DMCB,(RC)                                               
*                                  BUILD/INSERT 1F/20 ELEMENTS                  
         GOTO1 PUTRECS,DMCB,(RC),RECORD                                         
         L     RF,CONCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         GOTO1 DISPPUT,DMCB,(RC)                                                
***>>>HERE                                                                      
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
         GOTO1 JDSDATE,DMCB,JCONESAC,ELTBILD1+4,(1,0)                           
         CLI   JDSLEN,6            JDS ELT HAVE PENNIES?                        
         BNE   DOLB0060            YES                                          
         ZICM  RF,JCONESAM,2       NO  - NEED TO DECIMAL ALIGN                  
         SR    RE,RE                                                            
         M     RE,=F'100'          INSERT PENNIES                               
         ST    RF,DUB                                                           
         MVC   ELTBILD1+6(4),DUB   INSERT INTO ELEMENT                          
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
*  COMPELEM:  GENERATE THE COMPETITIVE ELEMENT FROM THE JDS                     
*        X'F0' ELEMENT                                                          
*        THERE ARE NO STATIONS IN THIS ELEMENT.  NEED TO BUILD                  
*        A STATION TABLE WHICH CONTAINS                                         
*        1.  BASE STATION                                                       
*        2.  COMPETITIVE STATIONS IN EXACT ORDER                                
*                                                                               
COMPELEM NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   JDSELT,X'F0'        GET COMPETITIVE ELEMENT                      
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   COMP0400            NOT FOUND                                    
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
         LA    R1,8                CALCULATE LENGTH OF ELEMENT                  
         SR    RE,RE                                                            
         LA    RF,5                                                             
         MR    RE,R4               CALCULATE LENGTH OF STA %S                   
         AR    R1,RF               ADD TO BASE LENGTH                           
         STC   R1,ELTBILD1+1       INSERT LENGTH INTO ELEMENT                   
         LA    RF,JCONCDPC         SET A(1ST PERCENT)                           
         LA    RE,ELTBILD1+9       SET A(1ST RECEIVING FIELD)                   
COMP0040 EQU   *                                                                
*                                                                               
*   NO PROVISION IS BEING MADE TO INSERT STATION CALL LETTERS YET.              
*     WHATEVER IS DONE NEEDS TO GO HERE!!                                       
*                                                                               
         ZIC   R7,0(RF)            RETRIEVE PERCENT                             
         SR    R6,R6                                                            
         M     R6,=F'100'          DECIMAL ALIGN TO 2 PLACES                    
         STCM  R7,15,5(RE)         INSERT INTO ELEMENT                          
         LA    RE,9(RE)            BUMP TO NEXT DDS STATION                     
         LA    RF,1(RF)            BUMP TO NEXT JDS PERCENT                     
         BCT   R4,COMP0040         GO BACK FOR NEXT                             
         GOTO1 HELOCON1            INSERT ELEMENT W/ERASE                       
COMP0400 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*   COMPCOMT:  RETRIEVE JDS 03 ELEMENT, CONSTRUCT UP TO FOUR                    
*        COMPETITIVE COMMENT ELEMENTS                                           
*                                                                               
COMPCOMT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R0,4                SET LOOP CONTROL: 4 MAX                      
         MVI   JDSELT,X'03'        GET COMP COMMENT ELT                         
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     COCO0040                                                         
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
         GOTO1 HELOCON1            INSERT ELEMENT, ERASE ELEMENT                
         GOTO1 GETJDSNX,DMCB,(RC)  GET NEXT ELEMENT                             
         BCT   R0,COCO0020         GO BACK AND CHECK IF FOUND                   
         B     COCO0100                                                         
COCO0080 EQU   *                                                                
         MVC   ELTBILD1+2(0),2(RF) INSERT COMMENT BY LENGTH                     
COCO0100 EQU   *                                                                
         XIT1                                                                   
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
         LA    R2,ELTBILD1                                                      
         USING RSARXEL,R2                                                       
*                                                                               
         MVC   RSARXEL,=X'1280'    SET CODE/LENGTH OF ELEMENT                   
         MVC   RSAREDT,RCONCREA    INSERT ORDER CREATE DATE AS                  
*                                     ENTRY DATE:  NO JDS EQUIV                 
         MVI   JDSELT,X'80'        GET DEMO ELEMENT                             
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0040            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,2(R3)            SET TO 1ST DEMO                              
*                                                                               
*   FOLLOWING ROUTINES DON'T REALLY KNOW HOW LONG THE DATA IS IN                
*        THE JDS ELEMENTS.  IT IS LIKELY THAT THE LOOP CONTROL                  
*        IS WRONG, AS IT MUST BE SET BY DIVIDING THE LENGTH OF DATA             
*        BY THE LENGTH OF THE INDIVIDUAL DATA ITEM.                             
*                                                                               
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
*                                                                               
BSAR0020 EQU   *                                                                
*                                                                               
*   NEED TRANSLATOR FOR DEMO CODES                                              
*                                                                               
         LA    R3,1(R3)            BUMP TO NEXT JDS DEMO                        
         BCT   R0,BSAR0020         GO BACK FOR NEXT                             
         B     BSAR0060            FINISHED WITH DEMOS                          
BSAR0040 EQU   *                                                                
*                                  NO DEMO CODE ELEMENT: INSERT HOMES           
        MVI   RSARXDEM+2,1                                                      
*                                 INSERT DEMO CODE OF HOMES!!                   
        MVI   RSARXDEM+1,C' '                                                   
*                                 INSERT QUALIFIER                              
BSAR0060 EQU   *                                                                
         MVC   RSARXSRC,RCONRTGS   INSERT RATING SERVICE                        
*                                                                               
         MVI   JDSELT,X'70'        GET BOOK ELEMENT                             
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0140            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,2(R3)            SET TO 1ST BOOK                              
         CLI   JDSLEN,6            SIX BOOKS = MAX                              
         BNH   BSAR0080                                                         
         MVI   JDSLEN,6            FORCE MAX OF SIX                             
BSAR0080 EQU   *                                                                
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
         LA    R7,RSARXBKS         SET A(SAR ELT BOOKS)                         
BSAR0100 EQU   *                                                                
         MVC   WORK+12(1),0(R3)    UNLOAD DATE                                  
         GOTO1 JDSDATE,DMCB,WORK+12,WORK,(1,0)                                  
*                                  CONVERT JDS 1-BYTE TO DDS 2-BYTE             
         MVC   1(2,R7),WORK        INSERT BOOK DATE INTO ELT                    
         CLI   RCONRTGS,C'N'       SERVICE = NIELSEN?                           
         BNE   BSAR0120            NO                                           
         OI    0(R7),X'40'         YES - TURN ON SERVICE BIT FOR NSI            
BSAR0120 EQU   *                                                                
         LA    R7,3(R7)            A(NEXT DDS BOOK FIELD)                       
         LA    R3,1(R3)            A(NEXT JDS BOOK FIELD)                       
         BCT   R0,BSAR0100         GO BACK FOR NEXT                             
BSAR0140 EQU   *                                                                
         MVI   JDSELT,X'50'        GET DAYPART ELEMENT                          
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0200            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,2(R3)            SET TO 1ST DYPT                              
         CLI   JDSLEN,6            SIX DYPTS = MAX                              
         BNH   BSAR0160                                                         
         MVI   JDSLEN,6            FORCE MAX OF SIX                             
BSAR0160 EQU   *                                                                
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
         LA    R7,RSARXDPT         SET A(SAR ELT DYPTS)                         
BSAR0180 EQU   *                                                                
         MVC   1(2,R7),0(R3)       INSERT DYPT INTO ELT                         
         LA    R7,3(R7)            A(NEXT DDS DYPT FIELD)                       
         LA    R3,1(R3)            A(NEXT JDS DYPT FIELD)                       
         BCT   R0,BSAR0180         GO BACK FOR NEXT                             
BSAR0200 EQU   *                                                                
         MVI   JDSELT,X'40'        GET BUDGET  ELEMENT                          
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0260            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,2(R3)            SET TO 1ST BUDGET                            
         CLI   JDSLEN,6            SIX BDGTS = MAX                              
         BNH   BSAR0220                                                         
         MVI   JDSLEN,6            FORCE MAX OF SIX                             
BSAR0220 EQU   *                                                                
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
         LA    R7,RSARXBUD         SET A(SAR ELT BDGTS)                         
BSAR0240 EQU   *                                                                
         MVC   0(3,R7),0(R3)       INSERT BDGT INTO ELT                         
         LA    R7,3(R7)            A(NEXT DDS BDGT FIELD)                       
         LA    R3,3(R3)            A(NEXT JDS BDGT FIELD)                       
         BCT   R0,BSAR0240         GO BACK FOR NEXT                             
BSAR0260 EQU   *                                                                
*                                                                               
*   DON'T KNOW HOW LONG LENGTH VALUE IS:  CHECK AGAINST A DUMP                  
*                                                                               
         MVI   JDSELT,X'60'        GET LENGTH  ELEMENT                          
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0320            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         LA    R3,2(R3)            SET TO 1ST LENGTH                            
         CLI   JDSLEN,6            SIX LGTHS = MAX                              
         BNH   BSAR0280                                                         
         MVI   JDSLEN,6            FORCE MAX OF SIX                             
BSAR0280 EQU   *                                                                
         L     R0,JDSLEN           SET L(JDS DATA) AS LOOP CONTROL              
         LA    R7,RSARXRFR         SET A(SAR ELT SECONDS)                       
BSAR0300 EQU   *                                                                
         MVC   0(2,R7),0(R3)       INSERT LGTH INTO ELT                         
         LA    R7,2(R7)            A(NEXT DDS LGTH FIELD)                       
         LA    R3,2(R3)            A(NEXT JDS LGTH FIELD)                       
         BCT   R0,BSAR0300         GO BACK FOR NEXT                             
BSAR0320 EQU   *                                                                
         MVI   RSARXFLG,X'E0'      SET BDGT $ = MKT $                           
*                                      $0 BDGT ENTERED                          
*                                      $0 SHARE ENTERED                         
*                                                                               
         MVI   JDSELT,X'08'        GET SHARE GOAL ELEMENT                       
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BSAR0340            NOT FOUND                                    
         L     R3,JDSADDR          SET A(JDS ELT)                               
         MVC   RSARXSHG,2(R3)      INSERT SHARE GOAL FROM '08' ELT              
BSAR0340 EQU   *                                                                
         GOTO1 HELOCON1            INSERT ELEMENT W/ERASE                       
*                                                                               
*                                  INSERT ACTIVITY COMMENT ELEMENTS             
         MVI   ELTBILD1,X'11'                                                   
         MVI   JDSELT,X'02'        GET ACTIVITY COMMENT ELEMENT                 
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BSAR0380                                                         
BSAR0360 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BSAR0380 EQU   *                                                                
         BNZ   BSAR0420            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         L     RE,JDSLEN           SET L(JDS DATA)                              
         BCTR  RE,0                DECREMENT BY 1 FOR EX                        
         EX    RE,BSAR0400         MOVE ACTIVITY COMMT BY LENGTH                
         LA    RF,3(RF)            INCREMENT FOR LENGTH                         
         STC   RF,ELTBILD1+1       INSERT LENGTH                                
         GOTO1 HELOCONA            INSERT ELEMENT, W/O ERASE                    
         B     BSAR0360            LOOK FOR NEXT ELEMENT                        
BSAR0400 EQU   *                                                                
         MVC   ELTBILD1+2(0),1(RF)                                              
BSAR0420 EQU   *                                                                
         XC    ELTBILD1,ELTBILD1   CLEAR ELEMENT                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  EXCONELT:  BUILD THE X'1F'/X'20'S ELEMENT FROM JDS INFO       *              
*                                                                *              
*     MOD/VERSION NUMBER CONVERSION WILL REQUIRE SOME SAMPLES    *              
*     TO DETERMINE BIT SETTINGS, AND DATES FOR DDS SYSTEM.       *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
EXCONELT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,ELTBILD1                                                      
         USING RCONXEL,R2          BUILD THE X'1F' ELT                          
         MVC   RCONXEL(2),=X'1F18'                                              
         TM    JCONCNTL,X'20'      UNCONFIRMED?                                 
         BNO   EXCO0040            NO  -  CONFIRMED                             
         OI    RCONCONF,X'80'      YES - SET 'UNCONFIRMED'                      
*                                                                               
*   NEED MORE INFORMATION RE:  CONFIRM/UNCONFIRM VS MOD #.                      
*                                                                               
         CLI   JCONMOD,0           INITIAL CONFIRMATION?                        
         BE    EXCO0020            YES                                          
         OI    RCONCONF,X'20'      NO  - SET 'PREV CONFIRMED' ALSO              
EXCO0020 EQU   *                                                                
         B     EXCO0060                                                         
EXCO0040 EQU   *                                                                
         OI    RCONCONF,X'40'      SET 'CONFIRMED'                              
EXCO0060 EQU   *                                                                
         GOTO1 HELOCON1                                                         
*                                                                               
         DROP  R2                                                               
         USING RCONSEND,R2         BUILD THE X'20' ELT                          
         MVC   RCONSNCO(2),=X'2029'                                             
         TM    JCONEFLG,X'40'      REP TR'D?                                    
         BO    EXCO0062            YES                                          
         TM    JCONEFLG,X'20'      REP TR'D?                                    
         BNO   EXCO0064            NO                                           
EXCO0062 EQU   *                                                                
         OI    RCONSENF,X'80'      SET 'LAST SENT BY REP'                       
         MVC   RCONSRV,JCONMVER    INSERT VERSION AS 'REP VER'                  
         ZIC   RF,JCONMVER                                                      
         BCTR  RF,0                SET STA VERSION 1 LESS                       
         STC   RF,RCONSSV          INSERT VERSION FOR STA                       
         B     EXCO0068                                                         
EXCO0064 EQU   *                                                                
         TM    JCONEFLG,X'10'      STA TR'D?                                    
         BO    EXCO0066            YES                                          
         TM    JCONEFLG,X'08'      STA TR'D?                                    
         BNO   EXCO0068            NO                                           
EXCO0066 EQU   *                                                                
         OI    RCONSENF,X'40'      SET 'LAST SENT BY STA'                       
         MVC   RCONSSV,JCONMVER    INSERT VERSION AS 'STA VER'                  
         ZIC   RF,JCONMVER                                                      
         BCTR  RF,0                SET REP VERSION 1 LESS                       
         STC   RF,RCONSRV          INSERT VERSION FOR REP                       
EXCO0068 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RCONCREA),(2,RCONSRDT)                            
*                                  INSERT SEND DATE INTO ELEMENT                
         MVC   RCONSRTI,=C'120000' INSERT DUMMY TIME                            
         MVC   RCONSSDT,RCONSRDT                                                
         MVC   RCONSSTI,RCONSRTI   INSERT DUMMY TIME                            
*                                                                               
         GOTO1 HELOCON1            INSERT X'20' ELEMENT                         
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
         XCEFL RECORD,1008         CLEAR THE RECORD BUILD AREA                  
         MVC   RBUYLEN(2),=X'004D' SET INITIAL LENGTH                           
         MVI   RBUYKTYP,X'0B'      INSERT RECORD TYPE                           
         MVC   RBUYKREP,JBUYKREP   INSERT REP CODE                              
         MVC   RBUYKPLN,=X'FFFFFF' INSERT PLAN                                  
         MVI   JDSELT,X'60'        GET PLAN ELEMENT                             
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0020            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         MVC   RBUYKPLN,1(RF)      INSERT PLAN FROM PLAN ELEMENT                
BUYR0020 EQU   *                                                                
         MVC   RBUYKMLN,JBUYKMLI   INSERT MASTER LINE NUMBER                    
         MVC   RBUYKLIN,JBUYKLIN   INSERT DETAIL LINE NUMBER                    
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),JBUYKCON                                              
*                                  RETRIEVE JDS CONTRACT NUMBER                 
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
*                                  REVERSE THE COMPLIMENT                       
         PACK  RBUYKCON+1(1),WORK+18(1)                                         
         PACK  RBUYKCON+2(1),WORK+17(1)                                         
         PACK  RBUYKCON+3(1),WORK+16(1)                                         
         PACK  RBUYKCON+4(1),WORK+15(1)                                         
*                                                                               
*   WITH CON # COMPLIMENTED/REVERSED, CHECK BACKUP AREA TO SEE IF               
*        THERE IS A BUYREC WHICH HAS TO BE PUT OUT.  THIS ORDER WAS             
*        RETAINED TO PERMIT INSERTION OF CROSS-REFERENCE MG ELEMENTS,           
*        IF NECESSARY.                                                          
*                                                                               
         CLI   BUYREC4,C'Y'        BUY RECORD IN REC4?                          
         BNE   BUYR0030            NO                                           
         CLC   RECORD4+RBUYKCON-RBUYKEY(4),RBUYKCON                             
*                                  SAME CONTRACT NUMBER?                        
         BE    BUYR0030            YES                                          
         MVI   BUYREC4,C'N'        SET 'NO RECORD IN REC4'                      
         GOTO1 PUTRECS,DMCB,(RC),RECORD4                                        
*                                  NO  - PUT RECORD4 TO OUTPUT                  
         L     RF,BUYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
         GOTO1 DISPPUT,DMCB,(RC)                                                
         XCEFL RECORD4,1024        CLEAR THE RECORD AREA                        
BUYR0030 EQU   *                                                                
         MVC   RBUYELEM(4),=X'012B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         MVC   RBUYNW,JBUYNPW      INSERT NUMBER PER WEEK                       
         MVC   RBUYCOS,JBUYRATE    INSERT BUYLINE COST                          
*                                                                               
         MVI   JDSELT,X'40'        GET CLASS ELEMENT                            
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0040            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         MVC   RBUYCLS,1(RF)       INSERT CLASS FROM ELEMENT                    
BUYR0040 EQU   *                                                                
*                                                                               
         MVI   JDSELT,X'50'        GET SECTN ELEMENT                            
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0060            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         MVC   RBUYSEC,1(RF)       INSERT SECTN FROM ELEMENT                    
BUYR0060 EQU   *                                                                
         MVC   RBUYCREA,RCONCREA   INSERT CONTRACT CREATION DATE                
*                                     AS BUY CREATION DATE: NO JDS              
         GOTO1 JDSDATE,DMCB,JBUYACTD,RBUYCHGD,(2,0)                             
         MVC   RBUYKMOD,JBUYMODN   INSERT MOD #                                 
         MVC   RBUYCHGI,JBUYCHGC   INSERT CHANGE CODE(S)                        
*                                                                               
*  NEED TO CALCULATE RBUYTSPT, RBUYTCOS, RBUYTWKS FROM DETAILS                  
*                                                                               
         MVC   RBUYSTED,JBUYSTED   INSERT START/END DAYS                        
         MVC   RBUYDUR,JBUYTYPE    INSERT COMM'L LENGTH + QUALIFIER             
         MVC   RBUYVER,JBUYVERS    INSERT VERSION NUMBER                        
*                                                                               
         MVI   JDSELT,X'70'        GET DAYPART ELEMENT                          
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0080            NOT FOUND                                    
         L     RF,JDSADDR          SET A(JDS ELT)                               
         MVC   RBUYDPT,1(RF)       INSERT DYPT FROM ELEMENT                     
BUYR0080 EQU   *                                                                
*                                                                               
*   GENERATE ONE OR MORE DAY/TIME ELEMENTS FROM BUY RECORD                      
*                                                                               
         MVI   JDSELT,X'20'        GET DAY/TIME ELEMENT                         
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0120                                                         
BUYR0100 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0120 EQU   *                                                                
         BNZ   BUYR0140            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         USING JBUYDYEL,RE                                                      
         MVC   ELTBILD1(2),=X'0209'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         MVC   RBUYDYIN,JBUYDYSE   INSERT START/END DAY                         
         ZIC   RF,JBUYDYDY         RETRIEVE DAYS OF WEEK                        
         SRL   RF,1                DROP LOW-ORDER, SHIFT 1 RIGHT                
         STC   RF,RBUYDAYS         INSERT DAYS OF WEEK                          
         MVC   RBUYDYT1(4),JBUYDYST                                             
*                                  INSERT START/END TIMES                       
         GOTO1 HELOBUY1            INSERT ELEMENT INTO BUY RECORD               
         B     BUYR0100            GO BACK FOR NEXT                             
*                                                                               
         DROP  RE                                                               
*                                                                               
BUYR0140 EQU   *                                                                
*                                                                               
*   GENERATE ONE OR MORE EFF DATE ELEMENTS FROM BUY RECORD                      
*                                                                               
         MVI   JDSELT,X'30'        GET DAY/TIME ELEMENT                         
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0180                                                         
BUYR0160 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0180 EQU   *                                                                
         BNZ   BUYR0280            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         USING JBUYDTEL,RE                                                      
         MVC   ELTBILD1(2),=X'030B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 JDSDATE,DMCB,JBUYDTST,ELTBILD1+2,(2,0)                           
*                                  INSERT START DATE                            
         MVC   ELTBILD1+5(3),ELTBILD1+2                                         
*                                  SET END DATE TO START                        
         CLI   JBUYDTEL,X'32'      SHORT JDS ELEMENT?                           
         BE    BUYR0200            YES - LEAVE END = START                      
         GOTO1 JDSDATE,DMCB,JBUYDTED,ELTBILD1+5,(2,0)                           
*                                  INSERT END DATE FROM JDS                     
BUYR0200 EQU   *                                                                
         MVC   RBUYDYIN,JBUYDYSE   INSERT START/END DAY                         
         OI    ELTBILD1+6,X'80'    SET 'RUNS EVERY WEEK'                        
         TM    JBUYDTNW,X'80'      ALTERNATE WEEK RUN?                          
         BNO   BUYR0220            NO                                           
         OI    ELTBILD1+6,X'40'    YES - SET 'RUNS EVERY OTHER WEEK'            
         NI    ELTBILD1+6,X'FF'-X'80'                                           
*                                  DROP 'RUNS EVERY WEEK'                       
BUYR0220 EQU   *                                                                
         MVC   ELTBILD1+7(1),JBUYDTNW                                           
*                                  INSERT JDS NUMBER PER WEEK                   
         NI    ELTBILD1+7,X'FF'-X'C0'                                           
*                                  TURN OFF HIGH-ORDER 2 BITS                   
         GOTO1 DATCON,DMCB,(3,ELTBILD1+3),(0,WORK)                              
*                                  CONVERT EFF START DATE TO EBCDIC             
         GOTO1 DATCON,DMCB,(3,ELTBILD1+6),(0,WORK+6)                            
*                                  CONVERT EFF END   DATE TO EBCDIC             
         GOTO1 =V(PERVERT),DMCB,WORK,WORK+6                                     
*                                  DETERMINE # EFFECTIVE WEEKS                  
         ZICM  RF,DMCB+12,2        GET NUMBER OF WKS QUOTIENT                   
         OC    DMCB+10(2),DMCB+10  ANY REMAINDER?                               
         BZ    BUYR0240            NO                                           
         LA    RF,1(RF)            YES - ADD 1 TO NUMBER OF WEEKS               
BUYR0240 EQU   *                                                                
         TM    ELTBILD1+6,X'80'    RUNS EVERY WEEK?                             
         BO    BUYR0260            YES                                          
         SRL   RF,1                NO  - ALT WEEKS: DIVIDE BY 2                 
BUYR0260 EQU   *                                                                
         STC   RF,ELTBILD1+8       INSERT NUMBER OF WEEKS                       
*                                                                               
         DROP  RE                                                               
*                                                                               
         ZIC   RF,ELTBILD1+8       NUMBER OF WEEKS                              
         L     RE,TWEEKCTR         ACCUMULATE TOTAL WEEKS                       
         AR    RF,RE                                                            
         ST    RF,TWEEKCTR         SAVE TOTAL WEEKS                             
         SR    RE,RE                                                            
         ZIC   R1,ELTBILD1+7       NUMBER SPOTS PER WEEK                        
         MR    RE,R1               #WKS X SPTS/WEEK = TTL SPOTS                 
         L     RE,TSPTCTR          ACCUMULATE TOTAL SPOTS                       
         AR    RF,RE                                                            
         ST    RF,TSPTCTR          SAVE TOTAL SPOTS                             
         SR    RE,RE                                                            
         ZICM  R1,RBUYCOS,4        GET BUYLINE COST                             
         MR    RE,R1               TOT SPOTS X COST = TOTAL COST                
         L     RE,TCOSCTR                                                       
         AR    RF,RE                                                            
         ST    RF,TCOSCTR                                                       
         GOTO1 HELOBUY1            INSERT ELEMENT INTO BUY RECORD               
         B     BUYR0160            GO BACK FOR NEXT                             
*                                                                               
BUYR0280 EQU   *                                                                
         MVC   RBUYTSPT,TSPTCTR+2  INSERT 2 BYTES FROM SPOT COUNTER             
         MVC   RBUYTCOS,TCOSCTR    INSERT 4 BYTES FROM COST COUNTER             
         MVC   RBUYTWKS,TWEEKCTR+3 INSERT 1 BYTE  FROM WEEK COUNTER             
*                                                                               
         LA    R0,2                LOOK FOR TWO COMMENTS                        
         MVI   JDSELT,X'90'        GET PROGRAM NAME ELEMENT                     
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         BNZ   BUYR0300            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         L     RF,JDSLEN           SET L(JDS DATA)                              
*                                                                               
         MVI   ELTBILD1,4          INSERT ELEMENT TYPE                          
         BCTR  RF,0                DECREMENT FOR MOVE BY LENGTH                 
         MVC   ELTBILD1+2(2),=C'P='                                             
         EX    RF,BUYR0288         MOVE PROGRAM NAME BY LENGTH                  
         GOTO1 HELOBUY1            INSERT ELEMENT W/ERASE                       
         LA    R0,1                ONLY LOOK FOR 1 COMMENT                      
         B     BUYR0300                                                         
*                                                                               
BUYR0288 MVC   ELTBILD1+4(0),2(RE) MOVE PROG NAME BY LENGTH                     
*                                                                               
BUYR0300 EQU   *                                                                
         MVI   JDSELT,X'01'        GET BUY COMMENT  ELEMENT                     
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0320                                                         
BUYR0310 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0320 EQU   *                                                                
         BNZ   BUYR0340            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         L     RF,JDSLEN           SET L(JDS DATA)                              
*                                                                               
         MVI   ELTBILD1,4          INSERT ELEMENT TYPE                          
         BCTR  RF,0                DECREMENT FOR MOVE BY LENGTH                 
         EX    RF,BUYR0328         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            SET LENGTH                                   
         STC   RF,ELTBILD1+1       INSERT LENGTH                                
         GOTO1 HELOBUY1            INSERT ELEMENT W/ERASE                       
         BCT   R0,BUYR0310         LOOK FOR ANOTHER COMMENT                     
         B     BUYR0340                                                         
*                                                                               
BUYR0328 MVC   ELTBILD1+2(0),2(RE) MOVE BUY COMENT BY LENGTH                    
*                                                                               
BUYR0340 EQU   *                                                                
*                                                                               
*   GENERATE ONE OR MORE BUY M/G REFERENCE ELEMENTS                             
*                                                                               
         MVI   JDSELT,X'D0'        GET M/G REFERENCE ELEMENT                    
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0380                                                         
BUYR0360 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0380 EQU   *                                                                
         BNZ   BUYR0460            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         USING JBUYMGEL,RE                                                      
         MVC   ELTBILD1(2),=X'050A'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         MVC   ELTBILD1+1(1),JBUYLIN#                                           
*                                  INSERT LINE # OF MISSED SPOT                 
         GOTO1 JDSDATE,DMCB,JBUYMSDT,ELTBILD1+3,(2,0)                           
*                                  INSERT MISSED DATE FROM JDS                  
         MVC   ELTBILD1+9(1),JBUY#MDS                                           
*                                  INSERT NUMBER OF MISSED SPOTS                
         GOTO1 HELOBUYA            INSERT X'05' W/O ERASE                       
*                                                                               
         MVC   ELTBILD1(2),=X'0607'                                             
*                                  INSERT XREF ELEMENT CODE/LENGTH              
         MVC   ELTBILD1+2(3),ELTBILD1+3                                         
*                                  SLIDE DATE OVER 1 BYTE                       
         MVC   ELTBILD1+5(1),RBUYKLIN                                           
*                                  INSERT REFERENCE LINE NUMBER                 
         MVC   ELTBILD1+6(1),ELTBILD1+10                                        
*                                  SLIDE OVER NUMBER OF SPOTS                   
         GOTO1 HELOREC4            INSERT ELEMENT INTO RBUYREC                  
*                                     IN RECORD4 AREA                           
*                                        AND ERASE ELEMENT                      
         B     BUYR0360            GO BACK FOR NEXT ELEMENT                     
         DROP  RE                                                               
BUYR0460 EQU   *                                                                
         LA    R0,2                SET MAX OF 2                                 
         MVI   JDSELT,X'03'        GET BUY ORDER COMMENT  ELEMENT               
         GOTO1 GETJDSEL,DMCB,(RC)                                               
         B     BUYR0500                                                         
BUYR0480 EQU   *                                                                
         GOTO1 GETJDSNX,DMCB,(RC)                                               
BUYR0500 EQU   *                                                                
         BNZ   BUYR0540            NOT FOUND                                    
         L     RE,JDSADDR          SET A(JDS ELT)                               
         L     RF,JDSLEN           SET L(JDS DATA)                              
*                                                                               
         MVI   ELTBILD1,X'84'      INSERT ELEMENT TYPE                          
         BCTR  RF,0                DECREMENT FOR MOVE BY LENGTH                 
         EX    RF,BUYR0520         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            SET ELEMENT LENGTH                           
         STC   RF,ELTBILD1+1       INSERT LENGTH INTO ELEMENT                   
         GOTO1 HELOBUY1            INSERT ELEMENT W/ERASE                       
         BCT   R0,BUYR0480         LOOK FOR ANOTHER COMMENT                     
         B     BUYR0540                                                         
*                                                                               
BUYR0520 MVC   ELTBILD1+2(0),2(RE) MOVE BUY ORD COM BY LENGTH                   
*                                                                               
BUYR0540 EQU   *                                                                
         CLC   RBUYKMLN,RBUYKLIN   MASTER = DETAIL LINE NUMBER?                 
         BE    BUYR0560            YES - MOVE TO ALTERNATE AREA                 
*                                  NO  - MAKEGOOD RECORD CAN BE                 
*                                     WRITTEN TO OUTPUT                         
         GOTO1 PUTRECS,DMCB,(RC),RECORD                                         
*                                  NO  - PUT RECORD  TO OUTPUT                  
         L     RF,BUYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
         GOTO1 DISPPUT,DMCB,(RC)                                                
         B     BUYR0600                                                         
BUYR0560 EQU   *                                                                
         PRINT GEN                                                              
         MOVE  (RECORD4,1024),RBUYREC                                           
*                                  SAVE BUY REC FOR POSSIBLE MG XREFS           
         PRINT NOGEN                                                            
BUYR0600 EQU   *                                                                
***>>>HERE                                                                      
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
         LA    R1,RECORD2          SET A(RECORD IN PROCESS)                     
         LR    R2,R1               CALCULATE END OF RECORD                      
         ZICM  RF,JCONLEN,2        GET RECORD LENGTH                            
         AR    R2,RF               FIND END OF RECORD                           
         L     R1,JDSADDR          SET A(LAST ELEMENT)                          
         TM    JDSELT,X'F0'        DETERMINE COMPRESSED/UNCOMPRESSED            
         BNZ   GJDS0400            COMPRESSED: SKIP THIS ELEMENT                
         B     GJDS0100            UNCOMPRESSED: SKIP THIS ELEMENT              
GETJDSEL NTR1                                                                   
         LA    R1,RECORD2+66       SET A(DESCRIPTOR ELEMENT)                    
         LR    R2,R1               CALCULATE END OF RECORD                      
         ZICM  RF,JCONLEN,2        GET RECORD LENGTH                            
         AR    R2,RF               FIND END OF RECORD                           
         TM    JDSELT,X'F0'        DETERMINE COMPRESSED/UNCOMPRESSED            
         BNZ   GJDS0200            COMPRESSED:                                  
GJDS0020 EQU   *                                                                
         CR    R1,R2               END OF RECORD REACHED?                       
         BNL   GJDS0800            YES - NO ELT: RETURN CC NOT ZERO             
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
         LTR   RB,RB               SET CC NOT ZERO                              
         B     GJDS1200                                                         
GJDS1000 EQU   *                                                                
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
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD1,0             
*                                  DON'T CLEAR THE ELEMENT                      
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
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RECORD4,ELTBILD1,0             
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
         CLC   CONCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   CONCTR,HIGHCTR                                                   
         BH    DIPU0090                                                         
         GOTO1 REPORT                                                           
         B     DIPU0020            DISPLAY THE RECORD                           
DIPU0010 EQU   *                                                                
         MVC   P+1(8),=C'BUY REC '                                              
         CLI   REC,X'0B'           CONTRACT?                                    
         BNE   DIPU0015            YES                                          
         CLC   BUYCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   BUYCTR,HIGHCTR                                                   
         BH    DIPU0090                                                         
         GOTO1 REPORT                                                           
         B     DIPU0020            DISPLAY THE RECORD                           
DIPU0015 EQU   *                                                                
         MVC   P+1(8),=C'ADV REC '                                              
         CLI   REC,X'08'           CONTRACT?                                    
         BNE   DIPU0090            YES                                          
         CLC   ADVCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   ADVCTR,HIGHCTR                                                   
         BH    DIPU0090                                                         
         GOTO1 REPORT                                                           
         B     DIPU0020            DISPLAY THE RECORD                           
DIPU0020 EQU   *                                                                
         MVC   P+10(06),=C'OUTPUT'                                              
         GOTO1 REPORT                                                           
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
*                                                                               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
         DS    1600C               DUMMY FOR ADDRESSABILITY                     
*        REMOVE WHEN PROGRAM CODE PASSES FIRST BASE REGISTER                    
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
ASTAAREA DS    A                   STATION/GROUP-SUBGROUP AREA                  
ASTAEND  DS    A                   A(LAST ENTRY IN TABLE)                       
ASALAREA DS    A                   SALESPERSON CONVERSION AREA                  
ASALEND  DS    A                   A(LAST ENTRY IN TABLE)                       
AMISAREA DS    A                   A(STATION MISSING AREA)                      
ANEXTMIS DS    A                                                                
AAGYAREA DS    A                   AGENCY CONVERSION AREA                       
AAGYEND  DS    A                   A(LAST ENTRY IN TABLE)                       
ANEWSALS DS    A                                                                
ANXTSAL  DS    A                                                                
ANEWPPRS DS    A                                                                
ANXTPPR  DS    A                                                                
ARECORD4 DS    A                                                                
NEWPPRCT DS    F                                                                
LBLDAREA DS    F                                                                
TOTCTR   DS    F                                                                
CONCTR   DS    F                                                                
BUYCTR   DS    F                                                                
ADVCTR   DS    F                                                                
AGYCTR   DS    F                                                                
AGYCTR2  DS    F                                                                
TSPTCTR  DS    F                                                                
TCOSCTR  DS    F                                                                
TWEEKCTR DS    F                                                                
LOWCTR   DC    F'99999'            LOW DISPLAY COUNT                            
HIGHCTR  DC    F'99999'            HIGH COUNTER                                 
PUTCTR   DS    F                                                                
STACTR   DS    F                                                                
SALCTR   DS    F                                                                
SALMISS  DS    F                                                                
AIOAREA  DS    F                                                                
BUCKWORK DS    4F                  BUCKET UPDATE ARGUMENTS                      
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
ELTBILD1 DS    CL128                                                            
ELTBILD2 DS    CL128                                                            
ELTBILD3 DS    CL128                                                            
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
WORK2    DS    CL256                                                            
*                                                                               
DATEFLAG DS    CL1                                                              
ACEGRAPH DS    XL1                 ACE/GRAPHNET INDICATOR                       
CONVERT# DS    CL1                                                              
JDSELT   DS    CL1                 JDS ELEMENT SEARCH ARGUMENT                  
JDSELT2  DS    CL1                 JDS ELEMENT SEARCH ARGUMENT                  
BLKFLAG  DS    CL1                 BLOCK EMPTY FLAG                             
JDSADDR  DS    F                                                                
JDSLEN   DS    F                                                                
BLKADDR  DS    F                   ADDRESS OF CURRENT RECORD IN BLOCK           
BLKEND   DS    F                   ADDRESS OF CURRENT RECORD EOR                
BASEYEAR DC    F'92'               BASE YEAR IS UNDETERMINED!!                  
BUYREC4  DS    CL1                 RECORD IN BUYREC4 AREA FLAG                  
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=1024,             X        
               BLKSIZE=20480,MACRF=GM,EODAD=GETT0100                            
*                                                                               
         SPACE 3                                                                
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL1024                                                           
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
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERT      RECORD                           
         EJECT                                                                  
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
*  OTHER JDS RECORD DSECTS GET ORG'D HERE                                       
         EJECT                                                                  
RECORD3  DS    CL1024                                                           
RECORD4  DS    CL1024                                                           
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         EJECT                                                                  
*********************************************************************           
         CSECT                                                                  
*                                                                               
******************************************************************              
*  GETTPREC:  UNBLOCK PETRY RECORD INPUT.  EXPAND KEY.  MOVE     *              
*             RECORD TO RECORD2, WHERE IT WILL BE PROCESSED.     *              
*             UPON EOF, SET CC = ZERO, WHICH WILL END JOB        *              
*             ELSE SET CC NOT ZERO.                              *              
******************************************************************              
*                                                                               
GETTPREC NMOD1 0,*GTTP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*  TAPE READ IS TO BE DONE WHEN A BLOCK IS REQUIRED.  WHEN A RECORD             
*        REMAINS IN THE BLOCK, IT IS TO BE UNBLOCKED, AND DELIVERED             
*        TO THE RECORD2 AREA                                                    
*                                                                               
         CLI   BLKFLAG,C'Y'        EMPTY BLOCK?                                 
         BNE   GETT0020            NO  - BUMP TO NEXT RECORD                    
GETT0010 EQU   *                                                                
         GET   INTAPE,RECORD3      READ TAPE RECORD INTO RDA                    
*                                     END OF FILE -> GETT0100 (DCB)             
         LA    RF,RECORD3          CALCULATE EOR, ETC                           
         ZICM  RE,RECORD3,2        GET BLOCK LENGTH FROM RECORD                 
         AR    RF,RE               CALCULATE END OF BLOCK                       
         ST    RF,BLKEND           SAVE A(END OF RECORD)                        
         LA    RF,12(RF)           BUMP TO L(FIRST RECORD)                      
         ST    RF,BLKADDR          SAVE A(REC IN PROCESS)                       
         B     GETT0040            DON'T BUMP TO NEXT RECORD                    
GETT0020 EQU   *                                                                
         MVI   BLKFLAG,C'N'        SET 'BLOCK NOT EMPTY'                        
         L     RF,BLKADDR          LOAD A(RECORD IN PROCESS)                    
         PRINT GEN                                                              
         ZICM  RE,0(RF),2          GET RECORD LENGTH                            
         PRINT NOGEN                                                            
         AR    RF,RE               BUMP TO NEXT RECORD                          
         L     RE,BLKEND           GET EOR                                      
         CR    RF,RE               END OF RECORD REACHED?                       
         BNL   GETT0010            YES - READ ANOTHER BLOCK                     
         ST    RF,BLKADDR          NO  - STORE A(NEW REC IN PROCESS)            
GETT0040 EQU   *                                                                
         LA    RF,6(RF)            SET A(FIRST CHARACTER TO MOVE)               
         LA    RE,RECORD2+30       SET A(RECEIVING RECORD)                      
*                                     SKIP CONTROL 15 JDS WORDS                 
         LR    R1,RF               SET DISP INTO RECEIVING REC                  
         LA    R1,4(R1)            SET A(COMPRESSED BYTE COUNT)                 
         PRINT GEN                                                              
         ZIC   R2,0(R1)            GET COMPRESSED BYTE COUNT                    
         PRINT NOGEN                                                            
         AR    RE,R2               DISPLACE INTO RECEIVING REC                  
         L     R1,BLKADDR          SET A(RECORD IN PROCESS)                     
         ZICM  R2,0(R1),2          RETRIEVE RECORD LENGTH                       
         EX    R2,GETT0060                                                      
         LTR   RB,RB               SET CC NOT ZERO                              
         B     GETT0200            EXIT ROUTINE                                 
GETT0060 EQU   *                                                                
         MVC   0(0,RE),0(RF)       MOVE DATA BY LENGTH                          
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
*                                                                               
         LA    R4,RECORD2                                                       
         GOTO1 REPORT                                                           
*                                                                               
*   LENGTH IS NOT CORRECT.  JUST SET FOR SPACE HOLDING                          
*                                                                               
         ZICM  RF,RCONLEN          GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                  DISPLAY FIRST PORTION OF RECORD              
DIPI0120 EQU   *                                                                
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
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'TOTAL CONTRACTS READ   :'                             
         EDIT  TOTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS     PROCESSED:'                             
         EDIT  CONCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUYS GENERATED         :'                             
         EDIT  BUYCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'RECORDS         WRITTEN:'                             
         EDIT  PUTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         L     R1,AMISAREA                                                      
DITO0120 EQU   *                                                                
         OC    0(5,R1),0(R1)       STATION PRESENT?                             
         BZ    DITO0200            NO  - FINISHED                               
         MVC   P+1(16),=C'MISSING STATION:'                                     
         MVC   P+20(5),0(R1)       INSERT STATION                               
         MVC   RSTAKSTA,0(R1)      INSERT STATION INTO KEY                      
*                                                                               
*  INSERTION OF CORRECT REP CODE FOR MISSING STATION MUST BE                    
*        RESOLVED.                                                              
*                                                                               
****     LA    RF,PETRYDV2         TRANSLATE COMPANY TO REP                     
DITO0140 EQU   *                                                                
*        CLI   0(RF),0             END OF TABLE REACHED?                        
*        BNE   *+6                 NO                                           
*        DC    H'0'                YES ??????                                   
*        CLC   5(1,R1),0(RF)       COMPANY FOUND IN TABLE?                      
*        BE    DITO0150            YES - USE IT                                 
*        LA    RF,LDIVSET2(RF)     NO  - BUMP TABLE                             
*        B     DITO0140            GO BACK FOR NEXT                             
DITO0150 EQU   *                                                                
         MVC   RSTAKREP,1(RF)      MOVE OUT NEW REP                             
         MVC   P+27(2),1(RF)       INSERT FOR PRINT                             
         GOTO1 REPORT              PRINT STATION/COMPANY MISSING                
         LA    RF,REC                                                           
         LA    R2,1000                                                          
         LA    RE,RSTAREC                                                       
         ST    R1,DUB              SAVE A(TABLE)                                
         PRINT GEN                                                              
         MOVE  ((RF),(R2)),(RE)    MOVE RECORD TO OUTPUT                        
         PRINT NOGEN                                                            
         L     R1,DUB              RESET A(TABLE)                               
         MVC   REC-4(2),RSTALEN    INSERT LENGTH INTO OUTPUT                    
         GOTO1 PUTRECS2            GENERATE O/P REC FOR STATIONS                
         BAS   RE,DTOTSTAS         DISPLAY THE STATION RECORD                   
DITO0160 EQU   *                                                                
         LA    R1,6(R1)            BUMP ADDRESS IN ARRAY                        
         B     DITO0120            NO - GO BACK FOR NEXT                        
DITO0200 EQU   *                                                                
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTRT,P+20,4,=C'TOG'                               
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
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
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SALCONV:  CONVERT SALESPERSON CODES FROM TABLE EQUIVALENCY.                 
*                                                                               
SALCONV  NMOD1 0,**SPCV**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
SALC0840 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DETERMINE NEW SALESPERSON, BASED ON INPUT, AND ADD TO BINARY                
*        SEARCH TABLE FOR NEXT USE.                                             
*                                                                               
SETNEWSP NTR1                                                                   
         XIT1                                                                   
*   INITIALIZATIONS ....                                                        
INITIAL  NMOD1 0,**INIT**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   BLKFLAG,C'Y'        SET 'BLOCK EMPTY' FLAG                       
         OPEN  (INTAPE,(INPUT))                                                 
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1000000,1000000                                 
*                                  GET 1MEG STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'30000'        TAPE BUFFER AREA:                            
         ST    RF,AAGYAREA         SET A(AGENCY AREA)                           
         A     RF,=F'11000'           NEEDS 10K BYTES AS FLAGS                  
         ST    RF,ASTAAREA         SET A(STATION AREA)                          
*                                                                               
*   LEAVE SUFFICIENT SPACE IN STATION                                           
*        TABLE FOR 15,000 ENTRIES:  150,000 BYTES.                              
*                                                                               
         A     RF,=F'150000'       STATION TABLE                                
         ST    RF,ASALAREA         SET A(SALESPERSON AREA)                      
*                                                                               
*   LEAVE SUFFICIENT SPACE IN SALESPERSON                                       
*        TABLE FOR 2,000 ENTRIES:  24,000 BYTES.                                
         A     RF,=F'30000'        MISSING STATION TABLE                        
         ST    RF,AMISAREA         SET A(MISSING STATION AREA)                  
         ST    RF,ANEXTMIS         SET A(NEXT MISSING SLOT)                     
*                                                                               
*                                                                               
*   LEAVE SUFFICIENT SPACE IN STATION                                           
*        TABLE FOR 2000 BYTES.                                                  
         A     RF,=F'2000'         NEWLY ADDED SALESPERSON TABLE                
         ST    RF,ANEWSALS         SET A(NEWLY ADDED S/P AREA)                  
         ST    RF,ANXTSAL          SET A(NEXT ADDED SLOT)                       
*                                                                               
*   LEAVE SUFFICIENT SPACE IN NEW SALESPERSON                                   
*        TABLE FOR 10000 ENTRIES: 30,000 BYTES                                  
         A     RF,=F'30000'        NEWLY ADDED POINT PERSONS TABLE              
         ST    RF,ANEWPPRS         SET A(NEWLY ADDED P/P AREA)                  
         ST    RF,ANXTPPR          SET A(NEXT ADDED SLOT)                       
*                                                                               
*   LEAVE SUFFICIENT SPACE IN NEW POINT PERSON                                  
*        TABLE FOR 10000 ENTRIES: 30,000 BYTES                                  
*                                                                               
*        MVC   PETRYREP,QRECORD+36 SAVE OVERRIDE REP                            
*        MVC   PETRYREP2,QRECORD+38 SAVE AGENCY REP                             
         MVC   CONVERT#,QRECORD+19                                              
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
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    STAT0800            YES - TABLE BUILT                            
*                                                                               
*        MVC   P+1(16),=C'START OF COMPANY'                                     
*        MVC   P+18(2),0(R2)                                                    
*        GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY+20(2),0(R2)     INSERT REP CODE INTO KEY                     
         GOTO1 HIGH1               READ FIRST RECORD                            
         B     STAT0060                                                         
STAT0040 EQU   *                                                                
         GOTO1 SEQ1                READ NEXT RECORD                             
STAT0060 EQU   *                                                                
         CLI   QUESTOR+7,C'Y'      DISPLAY STATION CODES?                       
         BNE   STAT0070            NO                                           
         CLC   STACTR,=F'100'      CHECK 1ST 100 ENTRIES                        
         BH    STAT0070                                                         
         MVC   P+1(8),=C'STA READ'                                              
         MVC   P+10(34),KEY                                                     
         GOTO1 REPORT                                                           
STAT0070 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME REC TYPE/REP?                           
         BE    STAT0080                                                         
         LA    R2,2(R2)            NO  - BUMP TABLE, GO BACK FOR NEXT           
         B     STAT0020                                                         
STAT0080 EQU   *                                                                
         GOTO1 GREC1               RETRIEVE RECORD                              
         MVC   0(2,R3),RSTAKREP    INSERT STATION REP CODE                      
         MVC   2(5,R3),RSTAKSTA    INSERT STATION LETTERS                       
         MVC   7(2,R3),RSTAGRUP    INSERT GROUP/SUBGROUP                        
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   STAT0120            NO X'05' - NOT ACE/GRAPHNET                  
         OC    10(2,R6),10(R6)     IS THERE A RECEIVING ID?                     
         BZ    STAT0120            NO  - NOT ACE/GRAPHNET                       
         CLC   10(2,R6),=X'0406'   GRAPHNET?                                    
         BNE   *+12                                                             
         OI    9(R3),X'40'         YES - MARK STATION 'GRAPHNET'                
         B     STAT0120                                                         
         OI    9(R3),X'80'         NO  - MARK STATION 'ACE'                     
STAT0120 EQU   *                                                                
         CLI   QUESTOR+7,C'Y'      DISPLAY STATION CODES?                       
         BNE   STAT0160            NO                                           
         MVC   P+1(14),=C'STATION ENTRY:'                                       
         MVC   P+20(10),0(R3)                                                   
         GOTO1 REPORT                                                           
STAT0160 EQU   *                                                                
*                                                                               
         ST    R3,ASTAEND          SAVE LAST STATION ADDR                       
         LA    R3,10(R3)           BUMP STATION TABLE ADDR                      
         L     RF,STACTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,STACTR                                                        
         B     STAT0040            GO BACK FOR NEXT STATION                     
STAT0800 EQU   *                                                                
*        MVC   P+1(17),=C'LEAVING  STATABLE'                                    
*        GOTO1 REPORT                                                           
         B     SALTABLE                                                         
*                                                                               
*   STAREPS IS SET UP FOR TESTING.                                              
*   THIS MUST BE CHANGED FOR THE LIVE RUN, TO BRING IN THE STATIONS             
*      FROM THE ACTUAL COMPANIES!!                                              
*                                                                               
STAREPS  DC    C'V1'               SELTEL                                       
         DC    C'V2'               SELTEL INTERNATIONAL                         
         DC    X'0000'                                                          
*TAREPS  DC    C'SZ'               SELTEL                                       
*        DC    C'SI'               SELTEL INTERNATIONAL                         
*                                                                               
*    WHAT IS CODE FOR SELTEL INTERNATIONAL?                                     
*                                                                               
*        DC    X'0000'                                                          
XTAREPS  DC    C'KZ'               PETRY                                        
         DC    C'KT'               PETRY ALTERNATE COMPANIES?                   
         DC    X'0000'                                                          
TTAREPS  DC    C'B4'               **TEST******                                 
         DC    X'0000'                                                          
*                                                                               
         EJECT                                                                  
*   SALTABLE:  INSERT SALESPERSON INFORMATION INTO TABLE, THEN SORT             
*        BY ORIGINAL PETRY CODE.                                                
*        TABLE IS CONSTRUCTED:                                                  
*            BYTES 1-5    =  ORIGINAL PETRY CODE                                
*            BYTES 6-8    =  ORIGINAL PETRY OFFICE CODE                         
*            BYTES 9-10   =  COMPANY CODE                                       
*            BYTES 11-13  =  EQUIVALENCY CODE                                   
*            BYTES 14-15  =  SALESPERSON TEAM                                   
*                                                                               
SALTABLE EQU   *                                                                
*        MVC   P+1(17),=C'ENTERING SALTABLE'                                    
*        GOTO1 REPORT                                                           
         L     R3,ASALAREA         SALESPERSON AREA                             
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
         BE    SALT0080            NO  - FINISHED                               
         LA    R2,2(R2)            BUMP TO NEXT REP                             
         CLI   0(R2),0             END OF TABLE?                                
         BE    SALT0120            YES - ALL EXTRACTED                          
         B     SALT0020            NO  - GO BACK FOR NEXT                       
SALT0080 EQU   *                                                                
         GOTO1 GREC1               RETRIEVE RECORD                              
         OC    RSALMRG,RSALMRG     ANY ORIGINAL PETRY VALUE?                    
         BZ    SALT0040            NO  - SKIP THIS RECORD                       
         EDIT  RSALMRG,(5,(R3)),FILL=0,ZERO=NOBLANK                             
*                                  INSERT ORIGINAL SALESPERSON CODE             
         MVC   8(2,R3),0(R2)       INSERT COMPANY CODE                          
         MVC   10(3,R3),RSALKSAL   INSERT NEW SALESPERSON CODE                  
         MVC   13(2,R3),RSALTEAM   INSERT NEW SALESPERSON CODE                  
*        LA    RF,PETRYOFFC        FIND/CONVERT OFFICE TO NUMBER                
SALT0100 EQU   *                                                                
*        CLI   0(RF),0             END OF TABLE REACHED?                        
*        BNE   *+6                 NO                                           
*        DC    H'0'                YES - UNRECOGNIZED OFFICE ON S/P?            
*        CLC   RSALOFF,3(RF)       S/P OFFICE FOUND?                            
*        BE    SALT0110            YES                                          
*        LA    RF,5(RF)            NO  - BUMP TO NEXT ENTRY                     
*        B     SALT0100            GO BACK FOR NEXT                             
SALT0110 EQU   *                                                                
         MVC   5(3,R3),0(RF)       INSERT OFFICE INTO TABLE                     
         CLI   QUESTOR+6,C'Y'      DISPLAY SALESPERSON CODES?                   
         B     SALT0115            NO                                           
****>>>  BNE   SALT0115            NO                                           
         CLC   SALCTR,=F'100'      DISPLAY ONLY 1ST 100                         
         BH    SALT0115                                                         
         MVC   P+1(10),=C'S/P: ORIG:'                                           
         MVC   P+14(5),0(R3)                                                    
         MVC   P+22(08),=C'DDS OFF:'                                            
         MVC   P+33(2),RSALOFF                                                  
         MVC   P+38(04),=C'NEW:'                                                
         MVC   P+45(3),10(R3)                                                   
         MVC   P+51(09),=C'ORIG OFF:'                                           
         MVC   P+63(3),5(R3)                                                    
         GOTO1 REPORT                                                           
SALT0115 EQU   *                                                                
         ST    R3,ASALEND          SAVE LAST SALESPERSON ADDR                   
         LA    R3,15(R3)           BUMP SALSPERSON TABLE ADDR                   
         L     RF,SALCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,SALCTR                                                        
         B     SALT0040            GO BACK FOR NEXT SALESPERSON                 
SALT0120 EQU   *                                                                
         L     R2,ASALAREA         SET A(SALESPERSON TABLE)                     
         L     R3,SALCTR           SET NUMBER OF RECORDS                        
         GOTO1 =V(QSORT),DMCB,(R2),(R3),15,10,0                                 
*                                  SORT TABLE INTO PETRY SEQUENCE               
SALT0160 EQU   *                                                                
         CLI   QUESTOR+6,C'Y'      DISPLAY SALESPERSON CODES?                   
         BNE   SALT0240            NO                                           
         L     R2,ASALAREA         YES - LOAD A(TABLE)                          
*        LA    R3,100              DISPLAY FIRST 100 SALESPERSONS               
SALT0200 EQU   *                                                                
         MVC   P+1(03),=C'S/P'                                                  
         MVC   P+5(15),0(R2)       MOVE TABLE ENTRY                             
         GOTO1 REPORT                                                           
         LA    R2,15(R2)                                                        
***>>    BCT   R3,SALT0200         GO BACK FOR NEXT                             
         OC    0(15,R2),0(R2)      ANY ENTRY?                                   
         BNZ   SALT0200            YES                                          
         GOTO1 REPORT              NO  - PUT OUT A BLANK                        
SALT0240 EQU   *                                                                
*        MVC   P+1(17),=C'LEAVING  SALTABLE'                                    
*        GOTO1 REPORT                                                           
         B     AGYTABLE                                                         
         EJECT                                                                  
*                                                                               
*   AGYTABLE:  READ ALL AGENCY RECORDS.  DISPLACE INTO TABLE AND                
*        SET BYTE TO 'Y' IF AGENCY CODE EXISTS.  ONLY PROCESS                   
*        ALL-NUMERIC AGENCY CODES.                                              
*   BECAUSE SELTEL WILL USE SAME CODES IN BOTH FILES, ONLY ONE SET              
*        OF DATA IS BEING LOADED, SPECIFIED BY PETRYREP2                        
*                                                                               
AGYTABLE EQU   *                                                                
*        MVC   P+1(17),=C'ENTERING AGYTABLE'                                    
*        GOTO1 REPORT                                                           
*        LA    RF,RECORD3          SET A(IO AREA FOR PROCEDURE)                 
*        ST    RF,AIOAREA                                                       
AGYT0020 EQU   *                                                                
*        XC    KEY,KEY             CLEAR KEY                                    
*        MVI   KEY,X'0A'           SET RECORD TYPE                              
*        GOTO1 HIGH1               READ FIRST RECORD                            
*        B     AGYT0060                                                         
AGYT0040 EQU   *                                                                
*        GOTO1 SEQ1                READ NEXT RECORD                             
AGYT0060 EQU   *                                                                
*        CLI   KEY,X'0A'           AGENCY RECORD?                               
*        BNE   AGYT0800            NO  - FINISHED                               
*        CLC   KEY+25(2),PETRYREP2 AGENCY IN QUESTION?                          
*        BNE   AGYT0040            NO  - SKIP THE RECORD                        
*        LA    RF,KEY+19           CHECK KEY FOR ALL NUMERIC                    
*        LA    RE,4                SET LOOP CONTROL                             
AGYT0080 EQU   *                                                                
*        CLI   0(RF),C'0'          ZERO COMPARE                                 
*        BL    AGYT0040            LESS THAN ZERO: SKIP IT                      
*        CLI   0(RF),C'9'          9 COMPARE                                    
*        BH    AGYT0040            MORE THAN 9: SKIP IT                         
*        BCT   RE,AGYT0080         DO EACH POSITION                             
*                                  ALL NUMERIC: USE IT                          
*        PACK  DUB,KEY+19(4)       PACK THE CODE                                
*        CVB   RF,DUB              CONVERT TO BINARY                            
*        BCTR  RF,0                MAKE ZERO RELATIVE                           
*        L     RE,AAGYAREA         LOAD A(AGENCY TABLE)                         
*        AR    RE,RF               DISPLACE TO CORRECT POSITION                 
*        MVI   0(RE),C'Y'          INSERT 'AGENCY PRESENT' FLAG                 
*        B     AGYT0040            GO BACK FOR NEXT RECORD                      
AGYT0800 EQU   *                                                                
*        MVC   P+1(17),=C'LEAVING  AGYTABLE'                                    
*        GOTO1 REPORT                                                           
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
**PAN#1  DC    CL21'078REREP102A 05/01/02'                                      
         END                                                                    
