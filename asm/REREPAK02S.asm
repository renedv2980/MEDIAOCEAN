*          DATA SET REREPAK02S AT LEVEL 195 AS OF 05/01/02                      
*PHASE REAK02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
         TITLE 'REREPAK02A (REAK02A) --- KATZ EDI ACKNOWLEDGMENT '              
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPAK02A -- KATZ EDI ACKNOWLEDGMENT: BUILD A TAPE OF    *            
*                      KATZ ACKNOWLEDGMENT RECORDS FROM RGENCON    *            
*                      AND RGENBUY RECORDS                         *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JAN31/96 ABBEY --- ORIGINAL ENTRY                                *            
*                                                                  *            
* FEB26/96 (BU ) --- QOPTION3 = Y:  OVERRIDE X'E1' KEY USE BY      *            
*                    X'0E' KEY USE (TEMP VS PERMANENT)             *            
*                                                                  *            
* MAY15/96 (BU ) --- PROCESSING CHANGES FOR KATZ TV                *            
*                                                                  *            
* JUN24/96 (BU ) --- ADJUST LEO ORIGINAL ORDER IDENTIFICATION      *            
*                                                                  *            
* JUL11/96 (SKU) --- FIX ESTIMATE NUMBER BUG                       *            
*                                                                  *            
* AUG19/96 (BU ) --- TEST VERSION TO FIX TOTALS RECORD (??)        *            
*                                                                  *            
* FEB07/97 (BU ) --- TEST S/P PHONE FOR SPACES ALSO                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    ***  END TOMBSTONE  ***                       *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REAK02   CSECT                                                                  
         NMOD1 0,**REAK**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTRT          SAVE START TIME                              
*                                                                               
MAIN0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,WORK),(3,DAREDATE)                                
*                                  SET AS-AT DATE TO TODAY                      
         CLC   QASAT,SPACES        ANY AS-AT DATE ENTERED?                      
         BE    MAIN0040            NO  - USE TODAY'S DATE                       
         GOTO1 DATCON,DMCB,(0,QASAT),(3,DAREDATE)                               
*                                  YES - USE IT FOR DATE                        
MAIN0040 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'E1'           INSERT KEY TYPE: EDI DRIVER                  
         CLI   QOPTION3,C'Y'       OVERRIDE E1 REC TYPE WITH 0E                 
         BNE   MAIN0050            NO                                           
         MVI   KEY,X'0E'           YES                                          
MAIN0050 EQU   *                                                                
         L     RF,AREPTABL         SET A(REP CODE IN TABLE)                     
         MVC   KEY+14(2),0(RF)     INSERT REP CODE FROM TABLE                   
         MVC   KEY+16(1),QOPTION1  INSERT ACK/CON FLAG                          
         GOTO1 HIGH                GET FIRST RECORD                             
         B     MAIN0080                                                         
MAIN0060 EQU   *                                                                
         GOTO1 SEQ                                                              
MAIN0080 EQU   *                                                                
         CLC   KEY(17),KEYSAVE     SAME TYPE/REP/ACT?                           
         BNE   MAIN0100            NO  - FINISHED                               
         CLC   DAREDATE,KEY+21     RECORD FOR DATE OF RUN?                      
         BNE   MAIN0060            NO  - SKIP IT                                
         CLC   KEY+17(7),DAREKEY+17                                             
*                                  CONTRACT/DATE ALREADY DONE?                  
         BE    MAIN0060            YES - GO BACK FOR NEXT                       
         MVC   DAREKEY,KEY         SAVE KEY FOR RESEQUENCING                    
*                                                                               
         LA    R2,DAREKEY+17                                                    
         GOTO1 CONTPROC,DMCB,(RC),(R2)                                          
*                                  PROCESS CONTRACT AND BUYS                    
         MVC   KEY(27),DAREKEY     RESET LAST DARE KEY                          
         GOTO1 HIGH                RESET TO LAST KEY                            
         B     MAIN0060            GO BACK FOR NEXT DARE RECORD                 
*                                                                               
*                                                                               
MAIN0100 EQU   *                                                                
         L     RF,AREPTABL         BUMP TO NEXT TABLE ENTRY                     
         LA    RF,2(RF)                                                         
         CLI   0(RF),0             END OF TABLE?                                
         BE    MAIN0120            YES - FINISHED                               
         ST    RF,AREPTABL         NO  - SAVE TABLE ADDR                        
         B     MAIN0020            GO BACK AND PROCESS                          
*                                                                               
MAIN0120 DS    0H                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         MVC   P+1(33),=C'NIGHTLY ACKNOWLEDGEMENTS COMPLETE'                    
         CLI   QOPTION1,C'A'       ACK RUN?                                     
         BE    MAIN0140            YES                                          
*                         1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.                
         MVC   P+1(34),=C'NIGHTLY CONTRACT TAPE RUN COMPLETE'                   
MAIN0140 EQU   *                                                                
         BAS   RE,MYREPT                                                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CONTPROC:  FOR EACH CONTRACT, PRODUCE A SINGLE KATZ FORMAT    *              
*     ORDER.                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            A(CONTRACT IN TABLE)                         
*                                                                               
         L     RF,CONCTR           INCREMENT RECORD COUNTER                     
         LA    RF,1(RF)               FOR COMPANY IN PROGRESS                   
         ST    RF,CONCTR                                                        
*                                                                               
         XC    KEY,KEY             CLEAR KEY FOR BUYLINE READ                   
         MVI   KEY,X'8C'           INSERT RECORD TYPE                           
         L     RF,AREPTABL         SET A(REP CODE IN TABLE)                     
         MVC   KEY+21(2),0(RF)     INSERT REP CODE FROM TABLE                   
*        GOTO1 =V(HEXIN),DMCB,0(R2),KEY+23,8                                    
         MVC   KEY+23(4),0(R2)                                                  
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),KEY+23(4)                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+23(4),WORK+15   INSERT THE COMP'D KEY                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    CPRO0020            YES                                          
         MVC   P+1(11),=C'NOT FOUND: '                                          
         MVC   P+15(10),0(R2)                                                   
         GOTO1 REPORT                                                           
         B     CPRO0520            EXIT                                         
CPRO0020 EQU   *                                                                
         MVC   AIOAREA,AIOAREA1    LOAD A(RECORD)                               
         GOTO1 GREC                                                             
*                                                                               
*   TEST                                                                        
***      CLC   =X'04156926',RCONKCON                                            
***      BNE   CPRO0520            SKIP ALL BUT SPECIAL CON                     
*                                                                               
*        SKIP DMBB ORDERS!!                                                     
*                                                                               
         CLC   =C'1342',RCONKAGY   DMBB ORDER?                                  
         BE    CPRO0520            YES - DON'T PROCESS IT                       
         CLC   =C'2905',RCONKAGY   DMBB ORDER?                                  
         BE    CPRO0520            YES - DON'T PROCESS IT                       
         CLC   =C'DMBB',RCONKAGY   DMBB ORDER?                                  
         BE    CPRO0520            YES - DON'T PROCESS IT                       
*                                                                               
*                                                                               
         LA    R4,EDIREC11         CLEAR TARGET RECORD                          
         L     R5,=F'529'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA1                
*                                     FOR 529 BYTES                             
         L     R4,AEDIR12          CLEAR TARGET RECORD                          
         L     R5,=F'529'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA2                
*                                     FOR 529 BYTES                             
*                                     FOR 529 BYTES                             
*                                                                               
         L     R4,AEDIR13          CLEAR TARGET RECORD                          
         L     R5,=F'529'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA2                
*                                     FOR 529 BYTES                             
         L     R4,AEDIR14          CLEAR TARGET RECORD FOR HDR WRITE            
         L     R5,=F'529'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA2                
*                                                                               
         LA    R3,EDIREC11         BUILD BOTH HEADERS IN PARALLEL               
         USING DAKHEDR1,R3                                                      
         L     R4,AEDIR12                                                       
         USING DAKHEDR2,R4                                                      
         L     R5,AEDIR13                                                       
         USING DAKHEDR3,R5                                                      
*                                                                               
*    RETRIEVE AGENCY 2 RECORD FOR ROUTING CODE                                  
*                                                                               
         XC    RECORD2(32),RECORD2                                              
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         MVC   KEY,RECORD2                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         MVC   AIOAREA,AIOAREA2    SET IO AREA TO RECORD3                       
         GOTO1 GREC                                                             
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'41'           SET UP ACTIVE DARE KEY                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        CHECK IF A ONE-SHOT CONVERT                  
         BAS   RE,GETEL                                                         
         BNZ   CPRO0040            NO X'1D'                                     
         TM    RCONDRFG-RCONDREL(R6),X'02'                                      
*                                  ONE-SHOT ORDER?                              
         BO    CPRO0060            YES - FORCE '51' RECORD USE                  
CPRO0040 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BZ    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         TM    RCONCONF-RCONXEL(R6),X'60'                                       
*                                  CONFIRMED/PREV CONFIRMED?                    
         BZ    CPRO0080            NO  - USE X'41' RECORD                       
CPRO0060 EQU   *                                                                
         MVI   KEY,X'51'           USE HISTORY DARE RECORD                      
CPRO0080 EQU   *                                                                
         L     RF,AREPTABL                                                      
         MVC   KEY+7(2),0(RF)      INSERT REP CODE                              
         MVC   KEY+9(4),RCONKSTA   INSERT STATION LETTERS                       
         MVC   KEY+13(2),=C'T '    MEDIA T                                      
*                                                                               
         MVC   KEY+15(5),RAGY2DAR                                               
*                                                                               
         LA    R6,RCONREC          FIND CONTRACT '1D' ELEMENT                   
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL            RETRIEVE ELEMENT                             
         BZ    *+6                 FOUND                                        
         DC    H'0'                MUST BE THERE                                
         MVC   KEY+20(4),RCONDRLK-RCONDREL(R6)                                  
*                                  INSERT AGENCY ORDER NUMBER                   
         MVI   KEY+24,X'10'        AGENCY HEADER ONLY                           
*                                                                               
         LA    R2,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R7,3                COMBINATIONS WE NEED TO CHECK                
*                                                                               
CPRO0100 DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BE    CPRO0120            YES                                          
*                                                                               
*   TEST                                                                        
*        MVC   P+1(10),=C'KEY SEEKS:'                                           
*        MVC   P+12(27),KEYSAVE                                                 
*        GOTO1 REPORT                                                           
*   TEST                                                                        
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         LA    R2,5(R2)                                                         
         MVC   KEY+15(5),0(R2)                                                  
         BCT   R7,CPRO0100                                                      
         MVC   P+1(24),=C'ORDER NOT FOUND ON FILE:'                             
         GOTO1 HEXOUT,DMCB,RCONKCON,P+30,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     CPRO0520            SKIP THIS ORDER                              
*                                                                               
CPRO0120 DS    0H                                                               
         MVC   AIOAREA,AIOAREA3    SET IO AREA TO RECORD3                       
         GOTO1 GREC                RETRIEVE RECORD                              
*                                                                               
         MVC   DAKHTYP,=C'11'      INSERT RECORD TYPE                           
         MVC   DAK2TYP,=C'12'      INSERT RECORD TYPE                           
         MVC   DAK3TYP,=C'13'      INSERT RECORD TYPE                           
*                                                                               
*   DARE AGENCY HEADER RECORD IS IN RECORD3 AT THIS TIME.                       
*        SEED DATA FROM IT WHERE APPROPRIATE.                                   
*                                                                               
         L     R6,AIOAREA3                                                      
         USING RDARREC,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RDARELEM,R6                                                      
*                                                                               
         MVC   HDRAGADR(15),RDARAGNM                                            
*                                  PUT AGENCY ADDRESS INTO HDR                  
         MVC   DAKHREF,RDARAGAD+5                                               
         MVC   DAKHMKT,RDARAGAD+30 MARKET #                                     
         MVC   DAK2REF,DAKHREF     INSERT INTO SECOND RECORD                    
         MVC   DAK3REF,DAKHREF     INSERT INTO SECOND RECORD                    
         DROP  R6                                                               
*                                                                               
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK,4,=C'TOG'                              
         MVC   DAK2CON,WORK+1      INSERT CONTRACT NUMBER                       
         MVC   DAK2CON2,FOXZEROS   INSERT ZEROS INTO 2ND CON #                  
         MVC   DAK2CNAD,RDARAGNM                                                
         MVC   DAK2INV,RDARAGNM+15                                              
         MVC   DAK2CSQL,RDARTDEM+7                                              
         CLI   DAK2CSQL,X'00'      BINARY ZERO IN FIELD?                        
         BNE   CPRO0140            NO                                           
         MVI   DAK2CSQL,C' '       YES - INSERT A SPACE                         
CPRO0140 EQU   *                                                                
         MVC   DAK2CLCD,RDARTDEM+6                                              
         CLI   DAK2CLCD,X'00'      BINARY ZERO IN FIELD?                        
         BNE   CPRO0160            NO                                           
         MVI   DAK2CLCD,C'1'       YES - INSERT CLASS CODE OF '1'               
CPRO0160 EQU   *                                                                
*                                                                               
         L     R6,AIOAREA3                                                      
         USING RDARREC,R6                                                       
         MVI   ELCODE,X'ED'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RDAREDEM,R6                                                      
*                                                                               
         MVC   DAK2BFNM,RDAREDDT+1                                              
         MVC   DAK2FLNO,RDAREDDT+51                                             
         MVC   DAKHMKNM,RDAREDDT+21                                             
         DROP  R6                                                               
*                                                                               
**********************************************************************          
*        PROCESS S/P SECTION                                                    
**********************************************************************          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6               INSERT S/P REC TYPE                          
         MVC   KEY+22(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+24(3),RCONSAL   INSERT S/P CODE                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         GOTO1 GREC                                                             
         MVC   DAK2ACCT(20),RSALNAME                                            
*                                  INSERT SALESPERSON NAME                      
         CLC   RSALTEL,SPACES      TELEPHONE NUMBER?                            
         BE    CPRO0163            NO                                           
         OC    RSALTEL,RSALTEL     TELEPHONE NUMBER?                            
         BNZ   CPRO0165            YES                                          
CPRO0163 EQU   *                                                                
         MVC   DAK2AEFN(10),FOXNINES                                            
         B     CPRO0170                                                         
CPRO0165 EQU   *                                                                
         MVC   DAK2AEFN+0(03),RSALTEL                                           
         MVC   DAK2AEFN+3(03),RSALTEL+4                                         
         MVC   DAK2AEFN+6(04),RSALTEL+8                                         
*                                  INSERT SALESPERSON PHONE                     
*                                     STRIPPING DASHES                          
CPRO0170 EQU   *                                                                
         OC    RSALFAX,RSALFAX                                                  
         BZ    CPRO0180            NO FAX NUMBER                                
         MVC   DAK2AEFX+0(03),RSALFAX                                           
         MVC   DAK2AEFX+3(03),RSALFAX+4                                         
         MVC   DAK2AEFX+6(04),RSALFAX+8                                         
*                                  INSERT SALESPERSON PHONE                     
*                                     STRIPPING DASHES                          
CPRO0180 EQU   *                                                                
**********************************************************************          
         EJECT                                                                  
*                                                                               
*   DDS DOESN'T HAVE A MARKET NUMBER                                            
*                                                                               
         MVC   DAKHSTAT,RCONKSTA   INSERT STATION CALL LETTERS                  
*                                                                               
*                                                                               
**********************************************************************          
*        PROCESS ADV SECTION                                                    
**********************************************************************          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,8               INSERT ADV REC TYPE                          
         MVC   KEY+21(4),RCONKADV  INSERT ADV CODE                              
         MVC   KEY+25(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         GOTO1 GREC                RETRIEVE RECORD                              
         MVC   DAKHADNM(20),RADVNAME                                            
*                                  INSERT ADVERTISER NAME                       
**********************************************************************          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,5            FIND PRODUCT CODE ELEMENT                    
         BAS   RE,GETEL                                                         
         BNZ   CPRO0200            NOT FOUND                                    
         MVC   DAKHPRNM(20),2(R6)  INSERT PRODUCT NAME                          
CPRO0200 EQU   *                                                                
*                                                                               
**********************************************************************          
*        PROCESS AGY SECTION                                                    
**********************************************************************          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,10              INSERT AGY REC TYPE                          
         MVC   KEY+19(4),RCONKAGY  INSERT AGY CODE                              
         MVC   KEY+23(2),RCONKAOF  INSERT AGY OFFICE CODE                       
         MVC   KEY+25(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         GOTO1 GREC                RETRIEVE RECORD                              
         MVC   DAK2AGCD(24),RDARPRN2     AGY CD, ADVCD, PRD CD                  
         CLC   DAK2AGCD(24),SPACES ANY VALUES?                                  
         BNE   CPRO0220            YES                                          
         MVC   DAK2AGCD(24),RDARCLNM                                            
*                                  NO  - USE ALTERNATE AREA                     
*                                     THIS MUST BE A START-UP ORDER             
CPRO0220 EQU   *                                                                
         MVC   DAK2TIND,RDARPRN2+32                                             
         CLI   DAK2TIND,X'00'      BINARY ZERO IN FIELD?                        
         BNE   CPRO0240            NO  - LEAVE AS IS                            
         MVI   DAK2TIND,C'P'       SET TEST/PROD IND TO 'PROD'                  
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BNE   CPRO0240            NO  - LEAVE AS IS                            
         MVI   DAK2TIND,C'T'       YES - SET TO 'TEST'                          
CPRO0240 EQU   *                                                                
         MVC   DAK2DVCD,RDARPRN2+33                                             
         CLI   QACCTOPT,C'K'       KATZ RUN?                                    
         BE    CPRO0260            YES - DON'T DO NEXT TEST                     
         CLI   DAK2DVCD,X'00'      BINARY ZERO IN FIELD?                        
         BNE   CPRO0260            NO                                           
         MVI   DAK2DVCD,C'S'       YES - INSERT SELTEL                          
*                                     NOT A PROBLEM FOR KATZ TV:                
*                                        MISTAKE WILL BE FIXED BY THEN          
CPRO0260 EQU   *                                                                
*                                                                               
         MVC   DAKHNAME(20),RAGYNAM1                                            
*                                  INSERT AGENCY NAME                           
         OC    RAGYADD1,SPACES     INSERT ZONE BITS, IF MISSING                 
         MVC   DAKHADR1(20),RAGYADD1                                            
*                                  INSERT 1ST LINE OF ADDR                      
         MVC   DAKHADR1(20),RAGYADD1                                            
*                                  INSERT 2ND LINE OF ADDR                      
         MVC   DAKHADR3(2),RAGYSTAT                                             
         MVC   DAKHADR3+3(5),RAGYZIP                                            
*                                  INSERT STATE AND ZIP ON LINE 3               
         MVC   AGYPRF10,RAGYPR10   SAVE LAST AGENCY PROFILE                     
********************************************************************            
*                                                                               
         MVC   DAK2BUOF,RDARAGAD+35                                             
*                                  INSERT AGENCY OFFICE CODE                    
         CLC   RCONKAOF,SPACES     ANY AGENCY OFFICE CODE?                      
         BE    CPRO0280            NO                                           
         XC    KEY,KEY                                                          
         MVI   KEY,4               INSERT OFF REC TYPE                          
         MVC   KEY+23(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+25(2),RCONKAOF  INSERT OFF CODE                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         GOTO1 GREC                RETRIEVE RECORD                              
*                                  INSERT AGENCY OFFICE NAME                    
*********************************************************************           
CPRO0280 EQU   *                                                                
         MVC   DAKHESNO,RDARPRN2+24                                             
         MVC   DAK2EST,RDARESDS    EST DESC                                     
*                                                                               
*   MAY NEED TO SAVE ESTIMATE NUMBER/DESCRIPTION TO TURN AROUND                 
*                                                                               
         MVC   DAKHOFF(2),RCONKOFF INSERT OFFICE CODE                           
*                                                                               
* CHECK IF ESTIMATE NUMBER PRESENT. ESTIMATE NUMBERS WERE NOT EXPORTED          
* TO THE DARE RECORD WITH THE KATZ ONE-SHOT. AS A RESULT, WE NEED TO            
* CHECK IF ESTIMATE NUMBER IS PRESENT IN THE X'A2' ELEMENT IN THE               
* CONTRACT RECORD                                                               
*                                                                               
         OC    DAKHESNO,SPACES     SPACE PAD                                    
         CLC   DAKHESNO,SPACES     CHECK IF NO ESTIMATE NUMBER PRESENT          
         BNE   CPRO0290                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'        FIND EASI ELEMENT WITH EST NUMBER            
         BAS   RE,GETEL                                                         
         BNE   CPRO0290            NOT FOUND                                    
         USING RCONIEL,R6                                                       
         MVC   DAKHESNO,RCONXEST   DEFAULT TO EXPANDED EST NUM                  
         OC    DAKHESNO,SPACES                                                  
         CLC   DAKHESNO,SPACES                                                  
         BNE   CPRO0290                                                         
         MVC   DAKHESNO,RCONIEST   USE OLD EST NUM IF DEFAULT NOT FOUND         
         DROP  R6                                                               
*                                                                               
CPRO0290 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,4               INSERT OFF REC TYPE                          
         MVC   KEY+23(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+25(2),RCONKOFF  INSERT OFF CODE                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         GOTO1 GREC                RETRIEVE RECORD                              
         MVC   DAKHOFNM(20),ROFFNAME                                            
*                                                                               
**********************************************************************          
         MVC   DAKHBUYR(20),RCONBUYR   BUYER NAME                               
*                                                                               
         MVC   DAKHBUFN,RDARBTEL   BUYER PHONE                                  
*                                                                               
         MVC   DAKHSRVC,RCONRTGS   INSERT RATING SERVICE                        
*                                                                               
         MVC   DAKHDEMO,RDARTDEM   DEMO                                         
*                                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,RDARDATE),(0,DAKHDATE)     DATE                   
         PRINT GEN                                                              
         EDIT  RDARTIME,(4,DAKHTIME),FILL=0                                     
****     GOTO1 HEXOUT,DMCB,RDARTIME,DAKHTIME,2,=C'TOG'   TIME                   
         PRINT NOGEN                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,FLTSTDAT)                            
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,FLTENDAT)                          
*                                  SAVE FLIGHT ST/END DATES YYMMDD              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,DAKHST1)                             
*                                  INSERT START DATE OF ORDER                   
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,DAKHEND1)                          
*                                  INSERT END   DATE OF ORDER                   
*                                                                               
*                                                                               
***>>>   EDIT  RCONWKS,(2,K#WEEKS),FILL=0                                       
*                                  INSERT NUMBER OF WEEKS                       
         MVC   DAKHPOB,=C'100'     DEFAULT PERCENT TO 100%                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        LOOK FOR SAR ELEMENT ELEMENTS                
         BAS   RE,GETEL                                                         
         BNZ   CPRO0300            NO SAR ELEMENT                               
         ZIC   R1,RSARXSHG-RSARXEL(R6)                                          
*                                  RETRIEVE SHARE GOAL                          
         LTR   R1,R1                                                            
         BNZ   *+14                                                             
         MVC   DAKHPOB,RDARAGAD+32                                              
         B     CPRO0300                                                         
         EDIT  (R1),(3,DAKHPOB),FILL=0                                          
********************************************************************            
*                                                                               
CPRO0300 DS    0H                                                               
****>>>>                                                                        
*        XC    KEY,KEY                                                          
*        MVI   KEY,1               INSERT REP REC TYPE                          
*        MVC   KEY+25(2),RCONKREP  INSERT REP CODE                              
*        GOTO1 HIGH                                                             
*        CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
*        BE    *+6                                                              
*        DC    H'0'                NO                                           
*        MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
*        GOTO1 GREC                RETRIEVE RECORD                              
****>>>                                                                         
         CLC   =C'SZ',RCONKREP     SELTEL RUN?                                  
         BNE   CPRO0302            NO  - MUST BE KATZ TV                        
         MVC   DAK2RPCD(5),=C'SELTL'                                            
         MVC   DAK2RPNM(6),=C'SELTEL'                                           
*                                  INSERT REP NAME                              
         B     CPRO0318                                                         
CPRO0302 EQU   *                                                                
         MVC   DAK2RPCD,SPACES     INSERT KATZ TV                               
         MVC   DAK2RPNM,SPACES                                                  
         MVC   DAK2RPCD(5),=C'KATZ '                                            
         MVC   DAK2RPNM(19),=C'KATZ COMMUNICATIONS'                             
*                                  INSERT REP NAME                              
         B     CPRO0318                                                         
CPRO0318 EQU   *                                                                
         MVI   DAK2MED,C'T'        INSERT 'T' AS MEDIA                          
*                                                                               
*   SPECIAL PROVISIONS FOR REVISION NUMBER MUST BE MADE.  THIS IS IN            
*        DISCUSSION WITH KATZ/AGENCIES.  FOR SHORT TERM, CONTRACT MOD           
*        NUMBER IS INSERTED IN HERE.                                            
*                                                                               
         CLI   QOPTION1,C'A'       ACKNOWLEDGMENT?                              
         BE    CPRO0380            YES - DON'T SET REVISION NUMBER              
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'ED'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONEDEL,R6                                                      
*                                                                               
         CLI   AGYPRF10,C'L'       ORDER FOR LEO BURNETT?                       
         BNE   CPRO0320            NO  - INSERT CONTRACT MOD #                  
         CLI   RCONEDMD,0          YES - ORIGINAL CONFIRMED?                    
         BE    CPRO0340            YES - FOR LEO, LEAVE ORIG                    
*                                     CONFIRMED BLANK                           
*                                                                               
CPRO0320 EQU   *                                                                
         EDIT  RCONEDMD,(2,DAK2REV),FILL=0                                      
CPRO0340 EQU   *                                                                
         MVI   DAK2STAT,C'3'       SET TO ORIGINAL CONFIRMED                    
         CLI   RCONEDMD,1          ORIGINAL CONFIRMATION?                       
         BE    CPRO0360            YES - LEAVE AS IS                            
         MVI   DAK2STAT,C'4'       NO  - SET TO REVISED                         
CPRO0360 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RCONEDDT),(0,DAK2LTDT)                            
CPRO0380 EQU   *                                                                
         DROP  R6                                                               
*                                                                               
*   DDS RETAINS TWO ORDER COMMENTS.  THIS IS ALL THAT IS SEARCHED               
*        FOR.  MORE ARE DISCARDED.                                              
*                                                                               
         LA    R7,DAK3COM1         SET A(FIRST COMMENT)                         
CPRO0400 EQU   *                                                                
         MVI   COMSEXST,C'N'       SET COMMENTS DON'T EXIST                     
         LA    R6,RCONREC                                                       
         MVI   ELCODE,2            LOOK FOR CONTRACT COMMENT ELEMENTS           
         BAS   RE,GETEL                                                         
         BNZ   CPRO0440            NO COMMENTS                                  
         MVI   COMSEXST,C'Y'       SET COMMENTS EXIST                           
         ZIC   RF,1(R6)            GET COMMENT LENGTH                           
         SH    RF,=H'3'            SUBTRACT FOR CONTROL + EX                    
         EX    RF,CPRO0420         MOVE COMMENT BY LENGTH                       
         LA    R7,75(R7)           BUMP TO NEXT COMMENT O/P                     
         BAS   RE,NEXTEL           GET NEXT 02 ELEMENT                          
         BNZ   CPRO0440            NOT FOUND                                    
         ZIC   RF,1(R6)            GET COMMENT LENGTH                           
         SH    RF,=H'3'                                                         
         EX    RF,CPRO0420                                                      
         B     CPRO0440                                                         
CPRO0420 EQU   *                                                                
         MVC   0(0,R7),2(R6)       MOVE COMMENT BY LENGTH                       
CPRO0440 EQU   *                                                                
*                                                                               
         MVI   HDRTEST,C'P'        SET TEST/PROD IND TO 'PROD'                  
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BNE   CPRO0460            NO  - LEAVE AS IS                            
         MVI   HDRTEST,C'T'        YES - SET TO 'TEST'                          
CPRO0460 EQU   *                                                                
         CLI   QOPTION1,C'A'       ACKNOWLEDGEMENT?                             
         BNE   CPRO0480            NO  - LEAVE AS IS                            
         MVC   HDRTYP,=C'855'      YES - SET TO ACKNOWLEDGEMENT                 
         MVC   HDRPOPR,=C'PR'      SET TO ACKNOWLEDGEMENT                       
CPRO0480 EQU   *                                                                
         L     RF,AEDIR14          SET A(EDIREC14)                              
         MVC   0(HDRLEN,RF),HDRREC                                              
*                                  SET UP HEADER FOR TRANSMISSION               
         L     R7,AEDIR14                                                       
         GOTO1 PUTRECS,DMCB,(RC),(R7)                                           
         LA    R7,EDIREC11                                                      
         GOTO1 PUTRECS,DMCB,(RC),(R7)                                           
         L     R7,AEDIR12                                                       
         GOTO1 PUTRECS,DMCB,(RC),(R7)                                           
         CLI   COMSEXST,C'N'       COMMENTS?                                    
         BE    CPRO0500            NO                                           
         L     R7,AEDIR13                                                       
         GOTO1 PUTRECS,DMCB,(RC),(R7)                                           
CPRO0500 EQU   *                                                                
*                                                                               
         DROP  R3,R4,R5                                                         
*                                                                               
         MVC   AIOAREA,AIOAREA1                                                 
*                                  RESET A(PRIME IO AREA)                       
         GOTO1 DOBUYS,DMCB,(RC)                                                 
*                                  PROCESS BUY RECORDS FOR ORDER                
         L     R4,AEDIR14          CLEAR TARGET RECORD AGAIN                    
         L     R5,=F'529'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA2                
*                                                                               
         L     RF,AEDIR14          SET A(EDIREC14)                              
         MVC   0(TRLRLEN,RF),TRLREC                                             
*                                  SET UP TRAILER FOR TRANSMISSION              
         L     R7,AEDIR14                                                       
         GOTO1 PUTRECS,DMCB,(RC),(R7)                                           
CPRO0520 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  DOBUYS:  CYCLE THROUGH THE BUY RECORDS FOR THE ORDER.  IF THIS               
*        IS AN ACKNOWLEDGEMENT PASS, DON'T OUTPUT THE BUYS, JUST                
*        GRAB THE FIGURES FOR THE TRAILER                                       
*                                                                               
DOBUYS   NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R4,EDIREC11+27      REUSE FIRST AREA: LEAVE KEY                  
         L     R5,=F'502'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA2                
*                                                                               
*                                                                               
         L     R4,AEDIR12          SET A(EDIREC12)                              
         LA    R4,27(R4)           REUSE SECOND AREA: LEAVE KEY                 
         L     R5,=F'502'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA2                
*                                                                               
         LA    R3,EDIREC11         BUILD BUYS IN EDIREC11                       
         USING DAKDTAIL,R3                                                      
         L     R4,AEDIR12          BUILD TRAILER IN EDIREC12                    
         USING DAKTRLR,R4                                                       
*                                                                               
         MVC   DAKDTYP,=C'14'      INSERT BUY DETAIL REC TYPE                   
         MVC   DAKTTYP,=C'99'      INSERT TRAILER    REC TYPE                   
*                                                                               
*   ESTABLISH KEY OF FIRST BUY RECORD                                           
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0B'           INSERT RECORD TYPE                           
         L     RF,AREPTABL                                                      
         MVC   KEY+16(2),0(RF)     INSERT REP CODE FROM TABLE                   
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         PACK  KEY+18(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  KEY+19(1),WORK+17(1)                                             
         PACK  KEY+20(1),WORK+16(1)                                             
         PACK  KEY+21(1),WORK+15(1)                                             
         GOTO1 HIGH                                                             
         B     DOBY0040                                                         
DOBY0020 EQU   *                                                                
         GOTO1 SEQ                 READ SEQUENTIAL FOR NEXT BUY                 
DOBY0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THRU CON#?                          
         BNE   DOBY0500            NO  - FINISHED WITH BUYS                     
         GOTO1 GREC                RETRIEVE BUY RECORD                          
         XC    SPOTGRID(SPGRIDLN),SPOTGRID                                      
         EDIT  RBUYAGBL,(4,DAKDLINE),FILL=0                                     
*                                  INSERT ORIGINAL BUYLINE #                    
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2            FIND BUY D/T ELEMENT                         
         BAS   RE,GETEL                                                         
         BZ    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING RBUYDYEL,R6                                                      
         MVC   WORK(1),RBUYDAYS    RETRIEVE DAYS TO BUILD ARRAY                 
         LA    RE,DAKDDAYS         SET A(ARRAY)                                 
         LA    R0,7                SET LOOP CONTROL                             
*                                  ALWAYS TEST 2ND BIT, AS FIRST                
*                                     BIT IS USED AS 'SPARE'                    
DOBY0060 EQU   *                                                                
         TM    WORK,X'40'          IS DAY BIT ON?                               
         BNO   DOBY0080            NO                                           
         MVI   0(RE),C'X'          YES - INSERT 'X' IN ARRAY                    
DOBY0080 EQU   *                                                                
         ZIC   RF,WORK             SHIFT DAYS                                   
         SLL   RF,1                MOVE 1 LEFT                                  
         STC   RF,WORK                                                          
         LA    RE,1(RE)            BUMP TO NEXT DAY IN GRID                     
         BCT   R0,DOBY0060                                                      
*                                  FINISHED:  ALL BITS DONE                     
         MVI   DAKDQUAL,C'1'       INSERT 'MONDAY' INTO QUALIFIER               
*                                     DEFAULT SETTING                           
         ZIC   RF,RBUYDYIN         DETERMINE START DAY                          
         SRL   RF,4                DROP LOW 4 BITS                              
         ZIC   RE,RBUYDYIN         DETERMINE END DAY                            
         SLL   RE,28               DROP HI  4 BITS                              
         SRL   RE,28               REALIGN HI 4 BITS                            
         CR    RF,RE               COMPARE START VS END DAYS                    
         BNH   DOBY0095            START <= END: USE DEFAULT                    
*                                  START > END: OOWR USE START                  
         STC   RF,DAKDQUAL         INSERT INTO QUALIFIER                        
         OI    DAKDQUAL,X'F0'      TURN ON ZONE BITS                            
DOBY0095 EQU   *                                                                
         CLC   =C'TBA',RBUYDYT2    START TIME = TBA?                            
         BNE   DOBY0100            NO                                           
         MVI   DAKDTMXC,C'T'       INSERT 'T' IN EXCEPTION                      
         MVC   DAKDSTRT(8),FOXZEROS                                             
         B     DOBY0160                                                         
DOBY0100 EQU   *                                                                
         EDIT  RBUYDYT1,(4,DAKDSTRT),FILL=0                                     
         CLC   =C'24',DAKDSTRT     MIDNIGHT TO 1AM?                             
         BNE   DOBY0120            NO                                           
         MVC   DAKDSTRT(2),=C'00'  YES - SET TO 0000-0059.                      
DOBY0120 EQU   *                                                                
         CLC   =C'CC',RBUYDYT2     END TIME = CC?                               
         BNE   DOBY0140            NO                                           
         MVI   DAKDTMXC,C'C'       INSERT 'C' IN EXCEPTION                      
         MVC   DAKDEND(8),FOXZEROS                                              
         B     DOBY0160                                                         
DOBY0140 EQU   *                                                                
         EDIT  RBUYDYT2,(4,DAKDEND),FILL=0                                      
*                                  INSERT START TIME                            
         DROP  R6                                                               
*                                                                               
DOBY0160 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,4            RETRIEVE BUY COMMENT ELEMENT                 
         BAS   RE,GETEL                                                         
         BNZ   DOBY0200            NOT FOUND: SEND 'TIME PERIOD'                
         CLC   =C'P=',2(R6)        PROGRAM NAME ?                               
         BNE   DOBY0200            NO  - SEND 'TIME PERIOD'                     
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SH    RF,=H'5'            SUBTRACT CONTROL, KEYWORK, EX                
         LA    RE,DAKDNAME                                                      
         MVC   DAKDNAME,SPACES     CLEAR FIELD TO SPACE                         
         EX    RF,DOBY0180         MOVE NAME BY LENGTH                          
         B     DOBY0220                                                         
DOBY0180 EQU   *                                                                
         MVC   0(0,RE),4(R6)       INSERT NAME BY LENGTH                        
         B     DOBY0220                                                         
DOBY0200 EQU   *                                                                
         MVC   DAKDNAME(11),=C'TIME PERIOD'                                     
DOBY0220 EQU   *                                                                
         MVC   WORK+20(2),RBUYDUR                                               
         NI    WORK+20,X'FF'-X'8F' TURN OFF HIGH ORDER                          
         EDIT  (2,WORK+20),(3,DAKDLEN),FILL=0                                   
         MVI   DAKDLNQL,C'S'       INSERT 'SECONDS'                             
         TM    RBUYDUR,X'80'       HIGH-ORDER BIT ON?                           
         BNO   DOBY0240            NO  -                                        
         MVI   DAKDLNQL,C'M'       YES - SET 'MINUTES'                          
DOBY0240 EQU   *                                                                
         MVC   DAKDDYPT,SPACES     CLEAR DAYPART FIELD                          
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'ED'        RETRIEVE BUY EDI DYPT ELEMENT                
         BAS   RE,GETEL                                                         
         BNZ   DOBY0250            NOT FOUND: LEAVE BLANK                       
         MVC   DAKDDYPT,2(R6)      DAYPART CODE                                 
*                                                                               
DOBY0250 EQU   *                                                                
         GOTO1 SETGRID,DMCB,(RC)                                                
*                                                                               
         EDIT  RBUYTSPT,(5,DAKDSPTS),FILL=0                                     
*                                  INSERT SPOTS FOR THE LINE                    
         EDIT  RBUYCOS,(9,DAKDUNCS),FILL=0                                      
*                                  INSERT UNIT COST                             
         EDIT  RBUYTCOS,(9,DAKDLNCS),FILL=0                                     
*                                  INSERT TOTAL LINE COST                       
*                                                                               
         MVI   DCOMFULL,C'N'       SET 'DETAIL COMMENT EMPTY'                   
         CLI   AGYPRF10,C'F'       FOOTE, CONE ORDER?                           
         BNE   DOBY0280            NO  - DON'T SHOW STATION COMMENT             
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'84'        RETRIEVE BUY ORDER COMMENT                   
         BAS   RE,GETEL                                                         
         BNZ   DOBY0280            NO BUY ORDER COMMENT                         
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SH    RF,=H'3'            SET UP FOR MOVE BY LENGTH                    
         EX    RF,DOBY0260                                                      
         MVI   DCOMFULL,C'S'       SET 'DETAIL COMMENT: STATION'                
         B     DOBY0280                                                         
DOBY0260 EQU   *                                                                
         MVC   DAKDCOMM(0),2(R6)   INSERT COMMENT BY LENGTH                     
DOBY0280 EQU   *                                                                
         EDIT  RBUYKLIN,(3,DAKDRPLN),FILL=0                                     
*                                  INSERT REP LINE NUMBER                       
         LA    R7,EDIREC11                                                      
         CLI   QOPTION1,C'A'       ACKNOWLEDGEMENT RUN?                         
         BE    DOBY0380            YES - DON'T OUTPUT DETAILS                   
*                                  NO  - CHECK FOR CREDIT OR MG,                
*                                     WHICH MUST SET FLAGS                      
         CLI   RBUYCHGI,C'C'       BUYLINE CANCELLED?                           
         BE    DOBY0290            YES                                          
         CLI   RBUYCHGI+1,C'C'     MAYBE - BUYLINE CANCELLED?                   
         BNE   DOBY0295            NO                                           
DOBY0290 EQU   *                                                                
         MVI   DAKDCSXC,C'4'       SET 'CANCELLED'                              
DOBY0295 EQU   *                                                                
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,4            BUY COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNZ   DOBY0360            NO COMMENT ELEMENT                           
*                                                                               
*   TEST                                                                        
*        MVC   P+1(14),=C'COMMENT FOUND:'                                       
*        MVC   P+20(6),2(R6)                                                    
*        MVC   P+30(1),DCOMFULL                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   DAKDCSXC,C'4'       BUY CANCELLED?                               
         BE    DOBY0320            YES - IGNORE CREDIT/MG COMMENTS              
         CLC   =C'CR=',2(R6)       1ST TWO CHARS 'CREDIT' FLAG?                 
         BNE   DOBY0300            NO                                           
         MVI   DAKDCSXC,C'5'       SET 'CREDIT'                                 
         B     DOBY0360                                                         
DOBY0300 EQU   *                                                                
         CLC   RBUYKMLN,RBUYKLIN   MASTER SAME AS DETAIL LINE #?                
         BE    DOBY0320            NO                                           
         MVI   DAKDCSXC,C'6'       SET 'MAKEGOOD'                               
         B     DOBY0360                                                         
DOBY0320 EQU   *                                                                
         CLI   DCOMFULL,C'N'       DETAIL COMMENT FIELD FULL?                   
         BNE   DOBY0360            YES - MUST GO INTO CONTINUATION              
         CLC   =C'P=',2(R6)        COMMENT=PROGRAM NAME?                        
         BNE   DOBY0340            NO                                           
*                                                                               
*   TEST                                                                        
*        MVC   P+1(14),=C'P=NAME  FOUND:'                                       
*        MVC   P+20(6),2(R6)                                                    
*        MVC   P+30(1),DCOMFULL                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNZ   DOBY0360            NO MORE ELEMENTS -                           
*                                  MORE ELEMENTS: INSERT THIS ONE               
DOBY0340 EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(14),=C'2ND COM FOUND:'                                       
*        MVC   P+20(6),2(R6)                                                    
*        MVC   P+30(1),DCOMFULL                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         ZIC   RF,1(R6)            NO  - GET ELEMENT LENGTH                     
         SH    RF,=H'3'            SET UP FOR MOVE BY LENGTH                    
         EX    RF,DOBY0350                                                      
         MVI   DCOMFULL,C'Y'       SET 'DETAIL COMMENT'                         
         B     DOBY0360                                                         
DOBY0350 EQU   *                                                                
         MVC   DAKDCOMM(0),2(R6)   INSERT COMMENT BY LENGTH                     
DOBY0360 EQU   *                                                                
         GOTO1 PUTRECS,DMCB,(RC),(R7)                                           
*                                  OUTPUT BUY RECORD TO FILE                    
DOBY0380 EQU   *                                                                
         LR    RF,R4               SAVE 'USING' ADDR                            
         LA    R4,EDIREC11+27      CLEAR FIRST AREA: LEAVE KEY                  
         L     R5,=F'502'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA2                
*                                                                               
         LR    R4,RF               RESET 'USING' ADDR                           
*                                                                               
         GOTO1 BUYCOMMT,DMCB,(RC),                                              
*                                                                               
DOBY0400 EQU   *                                                                
         ZICM  RF,RBUYTSPT,2       ACCUMULATE TOTAL SPOTS                       
         TM    RBUYTSPT,X'80'      TOTAL SPOTS NEGATIVE?                        
         BNO   DOBY0420            NO                                           
         SLL   RF,16               YES - SHIFT TO PROPAGATE SIGN                
         SRA   RF,16               SHIFT IT BACK                                
*                                                                               
DOBY0420 EQU   *                                                                
         L     RE,SPOTCTR                                                       
         AR    RE,RF                                                            
         ST    RE,SPOTCTR          SAVE TOTAL SPOTS                             
         ZICM  RF,RBUYTCOS,4       ACCUMULATE TOTAL DOLLARS                     
         L     RE,DOLRCTR                                                       
         AR    RE,RF                                                            
         ST    RE,DOLRCTR          SAVE TOTAL DOLLARS                           
         L     RE,LINCTR                                                        
         LA    RE,1(RE)                                                         
         ST    RE,LINCTR           SAVE TOTAL # LINES                           
         B     DOBY0020            GO BACK FOR NEXT RECORD                      
*                                                                               
DOBY0500 EQU   *                                                                
         EDIT  LINCTR,(4,DAKTLIN),FILL=0                                        
*                                  EDIT TOTAL NUMBER OF LINES                   
         EDIT  SPOTCTR,(7,DAKTSPT),FILL=0                                       
*                                  EDIT TOTAL NUMBER OF SPOT                    
         EDIT  DOLRCTR,(11,DAKTCST),FILL=0                                      
*                                  EDIT TOTAL NUMBER OF DOLLARS                 
         L     R7,AEDIR12                                                       
         GOTO1 PUTRECS,DMCB,(RC),(R7)                                           
*                                  PUT OUT THE TOTAL RECORD                     
         XC    LINCTR,LINCTR       CLEAR ALL THREE COUNTERS                     
         XC    DOLRCTR,DOLRCTR                                                  
         XC    SPOTCTR,SPOTCTR                                                  
*                                                                               
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*   BUYCOMMT:  LOOK FOR BUY COMMENTS.  IF PRESENT, INSERT INTO                  
*        DETAIL COMMENT RECORD                                                  
*                                                                               
BUYCOMMT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   COMTFLAG,C'N'       SET 'NO COMMENTS'                            
         LA    R6,RBUYREC                                                       
         LA    R4,EDIREC11                                                      
         USING DAKHEDR4,R4                                                      
*                                                                               
         LA    R7,DAK4COM1         SET A(FIRST COMMENT)                         
         MVI   ELCODE,4            BUY COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         B     BCOM0040                                                         
BCOM0020 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT COMMENT                             
BCOM0040 EQU   *                                                                
         BNZ   BCOM0120            COMMENTS FINISHED                            
*                                                                               
*   CHECK IF FIRST COMMENT IS A PROGRAM NAME.  IF IT IS, SKIP IT.               
*                                                                               
         CLC   =C'P=',2(R6)        COMMENT=PROGRAM NAME?                        
         BNE   BCOM0045            NO                                           
         BAS   RE,NEXTEL           YES - GET NEXT ELEMENT                       
         BNZ   BCOM0120            NO MORE ELEMENTS -                           
*                                  MORE ELEMENTS: INSERT THIS ONE               
BCOM0045 EQU   *                                                                
*                                                                               
*   CHECK IF FIRST COMMENT INSERTED INTO MAIN DETAIL RECORD.  IF SO,            
*        (DCOMFLAG = Y), DON'T INSERT THE COMMENT AGAIN:  SKIP IT.              
*                                                                               
         CLI   DCOMFULL,C'Y'       FIRST COMMENT ALREADY INSERTED?              
         BNE   BCOM0050            NO                                           
         MVI   DCOMFULL,C'N'       YES - SET FLAG OFF                           
         B     BCOM0020            SKIP THIS COMMENT                            
BCOM0050 EQU   *                                                                
         MVI   COMTFLAG,C'Y'       SET 'COMMENTS FOUND'                         
         ZIC   RF,1(R6)            GET LENGTH OF COMMENT                        
         SH    RF,=H'3'            SUBTRACT 3 FOR CONTRL, EX                    
         CH    RF,=H'50'           LENGTH > 50?                                 
         BNH   BCOM0060            NO                                           
         LA    RF,50               YES - SET MAXIMUM                            
BCOM0060 EQU   *                                                                
         EX    RF,BCOM0080         MOVE COMMENT BY LENGTH                       
         B     BCOM0100                                                         
BCOM0080 MVC   0(0,R7),2(R6)       MOVE COMMENT BY LENGTH                       
BCOM0100 EQU   *                                                                
         LA    R7,50(R7)           BUMP TO NEXT COMMENT                         
         B     BCOM0020            GO BACK FOR NEXT COMMENT                     
BCOM0120 EQU   *                                                                
         CLI   COMTFLAG,C'N'       ANY COMMENTS?                                
         BE    BCOM0140            NO  - NO OUTPUT                              
         MVC   DAK4AGLN,=C'9999'   YES - INSERT INDICATOR                       
         LA    R7,EDIREC11                                                      
         CLI   QOPTION1,C'A'       ACKNOWLEDGEMENT RUN?                         
         BE    BCOM0140            YES - DON'T OUTPUT DETAILS                   
*                                                                               
*   DETAIL COMMENT RECORDS ARE 'MISDEFINED' - READ 'UNDEFINED',                 
*       BECAUSE NO ONE TOLD US WHAT TO DO.                                      
*   THEY ARE BEING SKIPPED IN THE OUTPUT FOR NOW.                               
*                                                                               
****>>   GOTO1 PUTRECS,DMCB,(RC),(R7)                                           
*                                  PUT OUT THE COMMENT RECORD                   
*                                                                               
         DROP  R4                                                               
*                                                                               
BCOM0140 EQU   *                                                                
         LA    R4,EDIREC11+27      CLEAR FIRST AREA: LEAVE KEY                  
         L     R5,=F'502'          LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA2                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*  SETGRID:  CYCLE THROUGH X'03' ELEMENTS, AND COUNT SPOTS BY                   
*        DATE, DISPLACING INTO SPOTGRID FOR LATER EDITING INTO                  
*        EDI RECORD GRID.                                                       
*                                                                               
SETGRID  NTR1                                                                   
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,3            LOOK FOR EFFECT DATE ELTS                    
         BAS   RE,GETEL                                                         
         B     SGRI0040                                                         
SGRI0020 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
SGRI0040 EQU   *                                                                
         BNZ   SGRI0200            DONE - TRANSLATE THE GRID                    
*                                                                               
*   TEST                                                                        
*        MVC   P+1(07),=C'03 ELT='                                              
*        MVC   P+10(11),0(R6)                                                   
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         USING RBUYDTEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,DATEWORK)                            
*                                  -> EBCDIC: EFF START DATE                    
         GOTO1 =V(PERVERT),DMCB,FLTSTDAT,DATEWORK                               
*                                  DETERMINE DISPLACEMENT INTO GRID             
         ZICM  RF,DMCB+12,2        GET NUMBER OF WKS QUOTIENT                   
         OC    DMCB+10(2),DMCB+10  ANY REMAINDER?                               
         BZ    SGRI0060            NO                                           
         LA    RF,1(RF)            YES - ADD 1 TO NUMBER OF WEEKS               
SGRI0060 EQU   *                                                                
         CH    RF,=H'14'           MORE THAN 14?                                
         BH    SGRI0020            YES - CAN'T GO INTO GRID                     
*                                     SO SKIP THIS ELEMENT                      
         BCTR  RF,0                MAKE BUCKET ZERO RELATIVE                    
         SLL   RF,2                MULTIPLY BY FOUR FOR DISPLACEMENT            
         LA    R2,SPOTGRID         SET A(SPOTGRID)                              
         LR    R1,R2               CALCULATE END OF GRID                        
         A     R1,=F'28'           PERMIT 14 2-BYTE ENTRIES                     
         AR    R2,RF               SET WEEK OF GRID                             
         ZIC   RE,RBUYDTNW         # SPOTS PER WEEK                             
         ZIC   R0,RBUYDTWK         # WEEKS OF EFF DATE ELT                      
         LTR   R0,R0               ANY VALUE?                                   
         BNZ   SGRI0080            YES                                          
         LA    R0,1                NO  - SET TO AT LEAST 1                      
*                                                                               
*                                  THIS IS BECAUSE DMBB SENT IN A               
*                                     SERIES OF JUNK WITH ZERO                  
*                                     SPOTS/ZERO WEEKS                          
SGRI0080 EQU   *                                                                
         L     R4,0(R2)            GRID ACCUMULATOR                             
         AR    R4,RE               ADD NUMBER OF SPOTS                          
         ST    R4,0(R2)            PUT ACCUMULATOR BACK                         
         LA    R2,4(R2)            BUMP TO NEXT ACCUMULATOR                     
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK?                              
         BNO   SGRI0100            NO                                           
         LA    R2,4(R2)            YES - SKIP A WEEK                            
SGRI0100 EQU   *                                                                
***>>    CR    R2,R1               END OF GRID REACHED?                         
***>>    BNL   SGRI0020            YES - DON'T PUT INTO GRID                    
*                                     GO BACK FOR NEXT X'03', WHICH             
*                                     PROBABLY WON'T FIT IN GRID                
         BCT   R0,SGRI0080         GO BACK FOR NEXT WEEK                        
*                                                                               
*   TEST                                                                        
*        MVC   P+1(07),=C'GRID  ='                                              
*        MVC   P+10(56),SPOTGRID                                                
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         B     SGRI0020            GO BACK FOR NEXT X'03'                       
*                                                                               
         DROP  R6                                                               
*                                                                               
SGRI0200 EQU   *                                                                
         LA    R3,EDIREC11                                                      
         USING DAKDTAIL,R3                                                      
         LA    R2,SPOTGRID         A(ACCUMULATOR GRID)                          
         LA    R7,DAKDSPGR         A(OUTPUT RECORD GRID)                        
         LA    RF,14               LOOP CONTROL                                 
SGRI0220 EQU   *                                                                
         L     R4,0(R2)                                                         
         EDIT  (R4),(2,0(R7)),FILL=0                                            
         LA    R2,4(R2)            BUMP TO NEXT INPUT                           
         LA    R7,2(R7)            BUMP TO NEXT OUTPUT                          
         BCT   RF,SGRI0220         GO BACK FOR NEXT                             
SGRI0400 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
******************************************************************              
*  FORCE CONTINUOUS RUN             NO PAGING                                   
******************************************************************              
MYREPT   NTR1                                                                   
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   PFOURTH,SPACES                                                   
         B     EXIT                                                             
*                                                                               
INITIAL  NTR1                                                                   
         SPACE 1                                                                
         OPEN  (FILOUTA,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  GET 100K STORAGE SPACE                       
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',100000,100000                                   
*                                  GET 100K STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'40000'        TAPE BUFFER AREA:                            
*                                                                               
         LA    RF,RECORD                                                        
         ST    RF,AIOAREA          SET A(IOAREA) TO FIRST                       
         ST    RF,AIOAREA1         SAVE A(RECORD)                               
         LA    RF,RECORD2                                                       
         ST    RF,AIOAREA2         SAVE A(RECORD2)                              
         LA    RF,RECORD3                                                       
         ST    RF,AIOAREA3         SAVE A(RECORD3)                              
*                                                                               
         LA    RF,EDIREC11         SET A(EDIREC12/13/14)                        
         A     RF,=F'529'          BUMP TO A(EDIREC13)                          
         ST    RF,AEDIR12          SAVE ADDRESS                                 
         A     RF,=F'529'          BUMP TO A(EDIREC13)                          
         ST    RF,AEDIR13          SAVE ADDRESS                                 
         A     RF,=F'529'          BUMP TO A(EDIREC14)                          
         ST    RF,AEDIR14          SAVE ADDRESS                                 
*                                                                               
*        SET REP CODES                                                          
*                                                                               
         LA    RF,SELREPS                                                       
         CLI   QACCTOPT,C' '       DEFAULT:  SELTEL                             
         BE    INIT0040            YES                                          
         CLI   QACCTOPT,C'S'       SELTEL?                                      
         BE    INIT0040            YES                                          
         LA    RF,KATZREPS         NO                                           
         CLI   QACCTOPT,C'K'       KATZ TV?                                     
         BE    INIT0040            YES                                          
         DC    H'0'                NO  - UNRECOGNIZED OPTION                    
INIT0040 EQU   *                                                                
         ST    RF,AREPTABL                                                      
*                                                                               
*   SET REPORT HEADING                                                          
*                                                                               
         CLI   QACCTOPT,C'K'       KATZ TV?                                     
         BNE   INIT0080            NO                                           
         MVC   P+1(32),=C'KATZ   NIGHTLY CONTRACT TAPE RUN'                     
         CLI   QOPTION1,C'C'       CONTRACT RUN?                                
         BE    INIT0120            YES                                          
         MVC   P+1(32),=C'KATZ   NIGHTLY ACKNOWLEDGEMENTS '                     
         B     INIT0120                                                         
INIT0080 EQU   *                                                                
         MVC   P+1(30),=C'SELTEL NIGHTLY CONTRACT TAPE RUN'                     
         CLI   QOPTION1,C'C'       CONTRACT RUN?                                
         BE    INIT0120            YES                                          
         MVC   P+1(30),=C'SELTEL NIGHTLY ACKNOWLEDGEMENTS '                     
         B     INIT0120                                                         
INIT0120 EQU   *                                                                
         BAS   RE,MYREPT           DISPLAY HEADINGS                             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
*----------------------------------------------------------------*              
*                                                                               
         DS    0H                                                               
PUTRECS  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R0,4(R1)            SET A(RECORD TO BE OUTPUT)                   
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
*                                                                               
         LR    R4,R0               A(RECORD LENGTH FIELD)                       
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',529,=C'1D'                 
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
       ++INCLUDE RGENIO                                                         
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
AIOAREA  DS    F                                                                
AIOAREA1 DS    F                                                                
AIOAREA2 DS    F                                                                
AIOAREA3 DS    F                                                                
AEDIR12  DS    F                   A(EDIREC12)                                  
AEDIR13  DS    F                   A(EDIREC13)                                  
AEDIR14  DS    F                   A(EDIREC14)                                  
RUNSTRT  DS    F                                                                
CONCTR   DS    F                                                                
LINCTR   DS    F                                                                
SPOTCTR  DS    F                                                                
SPLINCTR DS    F                                                                
DOLRCTR  DS    F                                                                
COMMAND  DS    CL8                                                              
FOXZEROS DC    C'0000000000000000'                                              
FOXNINES DC    C'9999999999999999'                                              
AREPTABL DS    F                                                                
SELREPS  DC    C'SZ'               SELTEL COMPANY                               
         DC    X'0000'             DELIMITER                                    
KATZREPS DC    C'AM'               KATZ AMERICAN                                
         DC    C'CQ'               KATZ CONTINENTAL                             
         DC    C'NK'               KATZ NATIONAL                                
         DC    X'0000'             DELIMITER                                    
*                                                                               
TOTRECS  DS    F                   FOR TRAILER REC                              
TOTSPOT  DS    F                   FOR TRAILER REC                              
TOTDLLRS DS    F                   FOR TRAILER REC                              
*                                                                               
SPOTGRID DS    28F                 TWENTY-EIGHT WEEKLY COUNTERS                 
SPGRIDLN EQU   *-SPOTGRID                                                       
*                                                                               
DAREKEY  DS    CL27                DARE KEY SAVE AREA                           
DAREDATE DS    CL3                 DARE AS-AT DATE                              
FLTSTDAT DS    CL6                 ORDER FLIGHT START DATE EBCDIC               
FLTENDAT DS    CL6                 ORDER FLIGHT END   DATE EBCDIC               
DATEWORK DS    CL24                DATE WORK AREA                               
COMTFLAG DS    CL1                                                              
COMSEXST DS    CL1                                                              
AGYPRF10 DS    CL1                 10TH AGENCY PROFILE                          
DCOMFULL DS    CL1                 COMMENT USE FLAG                             
*                                                                               
ELCODE   DS    CL1                                                              
*                                                                               
HDRREC   DS    0F                                                               
         DC    C'CTZZKATZ           ZZ'                                         
HDRAGADR DS    CL15                                                             
         DC    C'U002000'                                                       
HDRTEST  DC    C'T'                INSERT 'P' FOR PRODUCTION                    
***PROD  DC    C'P'                INSERT 'T' FOR TEST                          
         DC    C'<X003020      '                                                
HDRPOPR  DC    C'PO'                                                            
HDRTYP   DC    C'850'              850 = CONTRACT, 855 = ACK                    
HDRLEN   EQU   *-HDRREC                                                         
*                                                                               
TRLREC   DS    0F                                                               
         DC    C'**END OF DOCUMENT DATA**'                                      
TRLRLEN  EQU   *-TRLREC                                                         
*                                                                               
*                                                                               
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=FB,MACRF=PM,              X        
               LRECL=529,BLKSIZE=21160,BUFNO=2                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE DMPRTQL                                                              
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD2  DS    CL1024                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENAGY2                                                      
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
         ORG                                                                    
RECORD3  DS    CL1024                                                           
         ORG   RECORD3                                                          
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
         ORG                                                                    
RECORD   DS    CL2000                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
EDIREC11 DS    529C                                                             
EDIREC12 DS    529C                                                             
EDIREC13 DS    529C                                                             
EDIREC14 DS    529C                                                             
EDIREC99 DS    529C                                                             
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DRKZEDIAKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'195REREPAK02S05/01/02'                                      
         END                                                                    
