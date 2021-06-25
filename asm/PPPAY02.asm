*          DATA SET PPPAY02    AT LEVEL 208 AS OF 01/20/21                      
*PHASE T40302A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE 'CHANGE LOG'                                                     
*        TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS'                          
*                                                                               
* KWAN 09/23/19   MULTIPLE INVOICES MARKING ALL BUYS BUG    SPEC-36222          
*                                                                               
* RKEJ 07/30/19   ENHANCE PRINT PAY LIMITS > 255            SPEC-34736          
*                                                                               
* KWAN 09/13/19   INCREASE TOLERANCE TO MAX OF $52.50       SPEC-28586          
*                                                                               
* KWAN 08/28/14   SCRIPT PAY UPLOAD PAYMENT MESSAGE ADJUSTMENT                  
*                 NO NEED TO REPLY AMOUNTS, IT CONFUSES SCRIPT WHICH            
*                 UPLOAD ITEM IS CLEARED AND WHICH ITEM IS NOT.                 
*                                                                               
* KWAN 03/04/14   SCRIPT PAY UPLOAD STATUS                                      
*                                                                               
* BPLA 12/18/12   QST RATE CHANGE-NEW PGSTTAB                                   
*                                                                               
* BOBY 01/18/12   MQ MESSAGE FOR IDESK                                          
*                                                                               
* BOBY 03/22/11   STORAGE PROTECTION BUG                                        
*                                                                               
* BPLA 12/00/10   QST RATE CHANGE - NEW PGSTTAB                                 
*                                                                               
* BOBY 03/00/10   GST OVERRIDE                                                  
*                                                                               
* BOBY 02/00/10   PST BASIS STORED AS BINARY IN CHECK REQUEST                   
*                                                                               
* BOBY 01/00/08   FX PROCESSING                                                 
*                                                                               
* BOBY 05/00/07   NEW CLEARANCE STATUS RECORDS                                  
*                                                                               
* SMYE 04/20/07   BUG FIX - TEST FOR BUY SERIAL# IN ADBUYER CALL                
*                 BEFORE ANY VALIDATION FOR MATCHING STATUS, ETC.               
*                                                                               
* BOBY 10/05      ADD BILLABLE DATE TO CHECK REQUEST                            
*                 ALLOW SINGLY WITH CD, QST, PST AND CR/CK BUYS                 
*                                                                               
* BOBY 03/05      ADD PID AND GROSS TO CHECK REQUEST                            
*                                                                               
* SMYE  12/04     ADD TO "CONTROL DATE CALCULATION FROM INVOICE DATE"           
*                 FOR POSSIBLE DATA FROM PUB                                    
*                                                                               
* SMYE 05/03-     ADD OPTIONAL OUTPUTTING OF ONE CHECK REQUEST FOR EACH         
*      09/03      BUY INCLUDED IN LUMP SUM CLEARANCE                            
*                 LINK TO ADBUYER                                               
*                                                                               
* KWAN 03/25/03   USE COMFACS' GETPROF INSTEAD OF INCLUDING IT                  
*                                                                               
* BOBY 01/03      MAKE 'OVRD' UNIVERSAL OVERRIDE                                
*                                                                               
* SMYE 07/02      FIX LOCK TESTING FOR SUB-CLIENTS                              
*                                                                               
* SMYE 04/19/02   PUBEDIT NOW CORE-RESIDENT                                     
*                                                                               
* SMYE 04/15/02   FIX BUG CAUSING MORE THAN 1 PST TAX ENTRY IN THE BUY          
*                 PST ELEMENT ("PST" OVERRIDE OPTION PROBLEM IN THE             
*                 BLDPST ROUTINE - SEE BLDPSTLP)                                
*                                                                               
* SMYE 03/02      MORE CHANGES FOR NEW I/O AREA (BUYIO)   (BUG FIX)             
*                                                                               
* BPLA 01/02      BUG FIX FOR CR/CK SWITCH IN PROC4 AND PAYEM4                  
*                                                                               
* SMYE 12/01      MANY CHANGES FOR NEW I/O AREA (BUYIO) FOR 4000 BYTE           
*                 BUY RECORD                                                    
*                                                                               
* BPLA 12/01      NO LONGER ISSUE LOCKS + NO UNLOCKING                          
*                                                                               
* BPLA NN/01      SOMETHING WITH "OVRD"                                         
*                                                                               
* SMYE 05/01      CODE FOR DATA LOCKING (CLIENT AND CLIENT/PUB)                 
*                                                                               
* SMYE 03/01      CODE FOR ADDITIONAL CHARGES                                   
*                                                                               
* SMYE 06/99      CODE FOR PARTIAL PAYMENT AGAINST ONE BUY ONLY                 
*                                                                               
* SMYE 12/22/97   GETINS MADE CORE-RESIDENT                                     
*                                                                               
* BPLA 3/6/97     FIX BUG - GO TO CALCPST EVEN IF TOTGSTP IS ZERO               
*                 NEEDED TO HANDLE HST CORRECTLY                                
*                                                                               
* BOBY 10/1/94    OPTION TO STOP PAY IF BILL HAS NOT BEEN PAID                  
*                                                                               
* BOBY 2/28/94    ADD PST CALCULATIONS                                          
*                                                                               
* BPLA 8/6/93     IF PAID LCD (NCDSW = L)  SET ON LCD BIT IN PPDSTAT            
*                 X'40'                                                         
*                                                                               
* BPLA 3/1/93     CR/CK CHANGE - CONTORL DATES MUST MATCH                       
*                                                                               
* BPLA 12/4/92    CR/CK CHANGE - ONLY COUNT RIGHT TYPE ELEMS                    
*                 WHEN USING CRCKSEQ                                            
*                                                                               
* BPLA 12/2/92    "XCD" IN COMMENTS TO EXCLUDE CD FROM THIS INVOICE             
*                                                                               
* BPLA 11/92      MANY CHANGES FOR CR/CK SWITCHING                              
*                                                                               
* BPLA 8/31/92    PROFILE CHECK FOR CLEARANCE STATUS RECORD                     
*                 NO-OPED                                                       
*                                                                               
* BPLA 7/1/92     PUT ZONE/EDITION INTO CLEARANCE STATUS ELEMENT                
*                                                                               
* BPLA 6/29/92    REMOVE ZONE AND EDITION FROM CLEARANCE STAUS RECORD           
*                 KEY                                                           
* BPLA 6/9/92     BYPASS MATCH AND TEARSHEET CHECK IF ALREADY PAID              
*                                                                               
* BPLA 6/8/92     CHANGES TO PAY "FREE" INSERTIONS                              
*                                                                               
* BPLA 5/18/92    CLEARANCE STATUS RECORD CHANGES                               
*                                                                               
* BOBY 3/29/92    TRANSFER FROM MATCH & TEARSHEET CHECKING                      
*                                                                               
* BPLA 3/24/92    IN PROC5D USE GST CODE FROM GETINS INSTEAD OF                 
*                 CODE FROM BUY (PBDGST)                                        
* BPLA 1/29/92    NEW PROGPRO2 CONTROL (+4 IN A0A PROFILE)                      
*                 N - NORMAL PROCESSING                                         
*                 Y - ALL BUYS MUST HAVE BEEN MATCHED                           
*                 O - PAY ONLY MATCHED BUYS                                     
*                                                                               
* ROSA  1/31/91   WHEN CLEARING 0 DOLLARS GST PAY INDICATOR NOT                 
*                 PROPERLY SET TO X'01' TEST NOW IS TO SEE IF GST               
*                 TAX PCT IS PASSED                             BUGO2           
* ROSA  1/28/91   FORCE QCDIND TO C                             BUG01           
* ROS   1/11/91   ADD GST OPTION WHILE PROCESSING FOR (X)TEST    L02            
*                 TO INCLUDE GST IN DETAILS AT BOTTOM OF SCREEN  L02            
* ROSA  12/17/90  ADD LOGIC FOR GST                              L01            
*                                                                               
*        TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS'                          
         TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS'                          
T40302   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,T40302,R9                                                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC           ESTABLISH WORKING STORAGE                    
*                                                                               
         L     RA,4(R1)                                                         
         USING T403FFD,RA          ESTABLISH TWA                                
*                                                                               
         RELOC RELO02              GET RELOCATION FACTOR                        
*                                                                               
         TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS'                          
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
*                                                                               
         CLI   PROGPROF+10,C'C'    IF CCUSA                                     
         BNE   *+12                                                             
         BRAS  RE,CCINV               CHECK IF EACH AMT HAS INV                 
         JNE   ERRORXT1               ERROR FOUND IN ROUTINE                    
*                                                                               
         XC    ADBERNUM,ADBERNUM   INIT ERROR NUMBER                            
*                                                                               
         BRAS  RE,SINGTST          EDIT SCREEN FOR SINGLY OPTION                
*                                                                               
         OC    ADBERNUM,ADBERNUM   CHECK FOR ERRORS                             
         JNZ   ERRORXT1                                                         
*                                                                               
         TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS - PROC'                   
***********************************************************************         
*                                                                     *         
*        CHECK IF WE CAN DO PAYMENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PROC     DS    0H                                                               
*                                                                               
         BRAS  RE,ACCINIT                                                       
*                                                                               
         BRAS  RE,A0AOPT           SET A0A OPTIONS                              
*                                                                               
         BRAS  RE,AMTVAL           VALIDATE ENTERED AMOUNTS                     
         JNE   ERRORXT1            ERROR FOUND IN ROUTINE                       
*                                                                               
         TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS - PROC0'                  
***********************************************************************         
*                                                                     *         
*        READ BUY RECORDS                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PROC0    DS    0H                                                               
*                                                                               
         LA    R3,KEY              ESTABLISH BUY KEY                            
         USING PBUYKEY,R3                                                       
         XC    PBUYKEY,PBUYKEY     INIT BUY KEY                                 
*                                                                               
         MVC   PBUYKAGY,AGYALPHA   SET AGENCY                                   
         MVC   PBUYKMED,PAYMD      MEDIA                                        
         MVI   PBUYKRCD,X'20'      RECORD ID                                    
         MVC   PBUYKCLT,PAYCL      CLIENT                                       
         MVC   PBUYKPRD,SAVPR      PRODUCT                                      
         MVC   PBUYKPUB(6),BPUB    PUB                                          
*                                                                               
         CLI   BPUB+4,X'FF'        IF DOING ALL ZONES/EDTS                      
         BNE   *+10                                                             
         XC    PBUYKPUB+4(2),PBUYKPUB+4  CLEAR ZONE & EDITION                   
*                                                                               
         LA    R4,15               FOR EXEC COMPARE THRU PUB                    
*                                                                               
         CLC   SAVPR,=C'ALL'       SKIP IF NOT ALL PRODUCTS                     
         BNE   PROC1                                                            
*                                                                               
         MVI   PBUYKRCD,X'21'      RESET REOCRD ID                              
         MVC   PBUYKPRD(6),PBUYKPUB PUT PUB HIGHER THAN PRD IN KEY              
         XC    PBUYKEY+13(3),KEY+13    CLEAR PRD AREA                           
*                                                                               
         LA    R4,12               FOR EXEC COMPARE THRU PUB                    
*                                                                               
PROC1    DS    0H                                                               
*                                                                               
         CLI   BPUB+4,X'FF'        TEST ALL ZONES/EDTS                          
         BNE   *+8                                                              
         SHI   R4,2                YES - COMPARE ONLY 8 DIGIT PUB               
*                                                                               
         ZAP   BUYCNT,=P'0'        ZERO BUY COUNTER                             
*                                                                               
         BRAS  RE,HIGH             READ FIRST BUY                               
*                                                                               
         MVC   STARTKEY,KEYSAVE    SAVE STARTING KEY                            
*                                                                               
PROCLOOP DS    0H                                                               
*                                                                               
         LA    R3,KEY              ESTABLISH BUY KEY                            
         USING PBUYKEY,R3                                                       
*                                                                               
         CLI   KEY+25,X'FF'        BYPASS FF DELETES                            
         BE    PROCCONT                                                         
*                                                                               
*--------------------------> CHECK FOR BREAK IN PUB WHILE READING BUYS          
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   PBUYKEY(0),KEYSAVE  MATCH THRU PUB                               
         BNE   PROCDONE                                                         
*                                                                               
         CLI   PBUYKRCD,X'21'      IF DOING ALL PRODUCTS                        
         BNE   PROC2B                                                           
         CLI   KEY+13,C'*'            SKIP OTHER AGY PRD                        
         BE    PROCCONT                                                         
*                                                                               
PROC2B   DS    0H                                                               
*                                                                               
         OC    PBUYKACT,PBUYKACT   SKIP IF ACTIVE PRODUCT KEY                   
         BNZ   PROCCONT                                                         
*                                                                               
         CLC   PBUYKDAT,BSTART     FILTER ON DATES                              
         BL    PROCCONT                                                         
         CLC   PBUYKDAT,BEND                                                    
         BH    PROCCONT                                                         
*                                                                               
         OC    BEST,BEST           SKIP IF NO ESTIMATE ENTERED                  
         BZ    PROC2E                                                           
*                                                                               
         CLC   PBUYKEST,BEST       FILTER ON ESTIMATE                           
         BNE   PROCCONT                                                         
*                                                                               
         CLI   BLIN,0              SKIP IF NO LINE NUMBER ENTERED               
         BE    PROC2E                                                           
*                                                                               
         CLC   PBUYKLIN,BLIN       DROP IF LINE NUMBER DOESN'T MATCH            
         BNE   PROCCONT                                                         
*                                                                               
PROC2E   CLI   PROGPROF+10,C'C'    TEST CCUSA                                   
         BNE   PROC3                                                            
*                                                                               
         CLI   PBUYKEST,0          SEE IF EST NO. BELOW 256                     
         BE    PROC2F              YES - GO CHK RANGE                           
*                                  FOR EST ABOVE 255                            
         CLI   SVXFRSW,C'N'        SEE IF SUPPRESSING XFR                       
         BNE   PROCCONT            NO - THEN MUST BE OUT OF RANGE               
*                                                                               
         B     PROC3               ELSE MUST BE IN RANGE                        
*                                                                               
PROC2F   CLC   PBUYKEST+1(1),SVXFREST   CHK VS LOW EST                          
         BL    PROCCONT                                                         
         CLC   PBUYKEST+1(1),SVXFRESX   SEE IF EST IN RANGE                     
         BH    PROCCONT                                                         
*                                                                               
PROC3    DS    0H                                                               
*                                                                               
*        READ IN BUY RECORD                                                     
*                                                                               
         MVC   AREC,ABUYIO         POINT TO BUY I/O AREA                        
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,ABUYIO           POINT TO BUY I/O AREA                        
         USING PBUYREC,R3                                                       
*                                                                               
         CLI   PBDBFD,C'T'         SKIP TEST BUYS                               
         BE    PROCCONT                                                         
*                                                                               
         TM    PBDCNDA,X'80'       IF CANADIAN PUB                              
         BNO   PROC2B0                                                          
*                                                                               
         CLI   BPUB+4,X'FF'        AND PAYING ACROSS ZONES                      
         BNE   PROC2B0                                                          
*                                                                               
         LA    RF,2(R4)            EX LENGTH TO INCLUDE ZONE IN COMPARE         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE         IF ZONE CHANGES                           
         BE    *+16                                                             
         MVC   KEYSAVE,KEY               UPDATE COMPARE KEY                     
         MVC   PAYGSTAX,=X'FFFF'         FORCE GST INITIALIZING                 
*                                                                               
PROC2B0  DS    0H                                                               
*                                                                               
         CLI   ADBSW,C'Y'          ADBUYER CALL ?                               
         BNE   PROC2BX             NO                                           
*                                  CHECK FOR SERIAL#S                           
         BRAS  RE,ADBTST           SEE IF WE SHOULD SKIP THIS BUY               
         BNE   PROCCONT            YES - SKIP                                   
*                                                                               
PROC2BX  DS    0H                                                               
*                                                                               
*        HANDLE SPECIAL REP BUYS                                                
*                                                                               
         CLI   SVREPTYP,C'S'       SEE IF PAYING SPECIAL REP BUYS               
         BNE   PROC3G              NO                                           
*                                                                               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         MVI   ELCODE,X'80'        LOOK FOR SPECIAL REP ELEM                    
         BRAS  RE,NEXTEL                                                        
         BNE   PROCCONT            NOT FOUND SKIP THIS BUY                      
*                                                                               
         CLC   SVREP,2(R5)         SEE IF REPS MATCH                            
         BNE   PROCCONT            NO SKIP THIS BUY                             
*                                                                               
         B     PROC4                                                            
*                                                                               
PROC3G   DS    0H                  PREP OR NO REP                               
*                                                                               
*        IF NOT PAYING SPECIAL REP                                              
*              DROP IF BUY FOR SPECIAL REP                                      
*                                                                               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         MVI   ELCODE,X'80'        STILL LOOK FOR SPECIAL REP ELEM              
         BRAS  RE,NEXTEL                                                        
         BE    PROCCONT            IGNORE SREP BUYS                             
*                                                                               
PROC4    DS    0H                                                               
*                                                                               
         TM    PBDCNDA,X'80'       IF CANADIAN BUY                              
         BNO   *+12                                                             
         BRAS  RE,BLDPST              UPDATE PST ELEMENT                        
         BNE   EXXMOD                 IN CASE OF ERRORS                         
*                                                                               
*        INIT GST/PST AREAS FOR THIS ONE BUY                                    
*                                                                               
         XC    THISBGST,THISBGST                                                
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LAY   RF,PSTBCKS          PST DATA AREA                                
         USING PSTBCKS,RF                                                       
*                                                                               
         XC    THISBPST,THISBPST                                                
         LA    RF,PSTBCKSL(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
***********************************************************************         
*                                                                               
         CLI   CRCKSW,0           IF DOING A CR/CK SWITCH                       
         BE    PROC4C                                                           
*                                                                               
*                          MODIFY BUY AND "RE-DO" GETINS FOR CR/CK              
         GOTO1 =A(CRCKGET),DMCB,(RC),(R9),RR=RELO02                             
*                                                                               
*        CHECK FOR INVALID CONTROL DATE ERROR                                   
*                                                                               
         CLC   PAYMSG(15),=C'INVALID CONTROL'                                   
         BE    PROCER1                                                          
*                                                                               
         CLI   CRCKFND,C'X'        ANY ELEMENTS FOR CHANGE FOUND ?              
         BE    PROCCONT            NO - SKIP THIS BUY                           
*                                                                               
         B     PROC4D              CONTINUE                                     
*                                                                               
***********************************************************************         
*                                                                               
PROC4C   DS    0H                                                               
*                                                                               
*        CALCULATE ALL COSTS EXCEPT FX                                          
*                                                                               
         GOTO1 VGETINS,DMCB,PBUYREC,PVALUES,PBUYKPRD,(C'F',0),=C'PST',0         
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN BUY                     
         BNO   PROC4C1                                                          
*                                                                               
*        SAVE GST AND PST DATA                                                  
*                                                                               
         L     RF,16(R1)           ADDRESS OF GST DATA                          
         LAY   R1,GVALUES          POINT TO GETINS GST DATA SAVEAREA            
         USING GVALUES,R1          ESTABLISH GST AREA                           
*                                                                               
         MVC   GVALUES(GVALUESL),0(RF) COPY GST DATA                            
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LA    RE,PSTAREA          PST SAVEAREA                                 
         LA    RF,PSTAREA-GVALUES(RF)  PST DATA FROM GETINS                     
*                                                                               
         MVC   0(PSTAREAL,RE),0(RF)  COPY PST DATA                              
         LA    RE,PSTAREAL(RE)    BUMP POINTERS                                 
         LA    RF,PSTAREAL(RF)                                                  
         BCT   R0,*-14                                                          
*                                                                               
PROC4C1  DS    0H                                                               
*                                                                               
         TM    PBUYCNTL,X'80'      TEST DELETED                                 
         BZ    PROC4D              NO                                           
*                                                                               
         OC    PGROSS(12),PGROSS   IF DELETED AND NOT PAID                      
         BZ    PROCCONT            BYPASS                                       
*                                                                               
         XC    GROSS(20),GROSS     SET ORDERED AMOUNTS TO ZERO                  
*                                                                               
         TM    PBDCNDA,X'80'       CANADIAN BUY                                 
         BNO   PROC4D                                             L01           
*                                                                               
*        CLEAR ORDERED GST/PST DOLLARS                                          
*                                                                               
         LAY   R1,GVALUES                                                       
         USING GVALUES,R1                                                       
*                                                                               
         XC    GSTTAX,GSTTAX       CLEAR GROSS GST                 L01          
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LA    RF,PSTAREA          PST SAVEAREA                                 
         USING PSTAREA,RF          ESTABLISH PSTAREA                            
*                                                                               
         XC    PSTTAX,PSTTAX      CLEAR GROSS PST                               
         XC    PST$BS,PST$BS      CLEAR GROSS PST DOLLAR BASIS                  
         LA    RF,PSTAREAL(RF)    BUMP POINTER                                  
         BCT   R0,*-16                                                          
*                                                                               
         DROP  RF,R1                                                            
*                                                                               
PROC4D   DS    0H                                                               
*                                                                               
*        TEST IF BUY PREVIOUSLY PAID                                            
*                                                                               
         MVI   PAIDSW,0       WILL BE SET TO X'01' IF THIS BUY                  
*                             HAS A DATED PAY ELEM                              
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         MVI   ELCODE,X'25'        LOOK FOR PAY ELEM WITH DATE                  
*                                                                               
PROC4D5  BRAS  RE,NEXTEL           FIND A PAY ELEMENT                           
         BNE   PROC4D6               NONE FOUND                                 
*                                                                               
         OC    2(3,R5),2(R5)       IF ELEMENT HAS A PAY DATE                    
         BZ    PROC4D5                                                          
*                                                                               
         MVI   PAIDSW,1               SKIP CHECKS                               
*                                                                               
         B     PROC5                                                            
*                                                                               
PROC4D6  DS    0H                                                               
*                                                                               
*        CHECK TEARSHEET OPTIONS                                                
*                                                                               
         CLI   PROGPRO2+8,C'Y'     IF BUY MUST HAVE TEARSHEET                   
         BNE   PROC4TN                                                          
*                                                                               
         BRAS  RE,PBLOVCK          CHECK FOR OVERRIDES                          
         BE    PROC4TN     OVERRIDE EXISTS -SKIP PROFILE CHECK                  
*                                                                               
         TM    PBDSTAT,X'10'          ERROR IF MISSING TEARSHEET                
         BNO   PROCER2                                                          
*                                                                               
PROC4TN  DS    0H                                                               
*                                                                               
*        CHECK MATCHED/UNMATCHED OPTIONS                                        
*                                                                               
         CLI   PROGPRO2+9,C'O'     PAYING ONLY MATCHED BUYS?                    
         BE    *+8                                                              
         CLI   PROGPRO2+9,C'I'     PAYING ONLY INVOICED BUYS?                   
         BNE   PROC4N                                                           
*                                                                               
         BRAS  RE,PBLOVCK          OVERRIDES?                                   
         BE    PROC4N              YES - SKIP MATCHED CHECK                     
*                                                                               
         TM    PBDSTAT,X'40'       MATCHED?                                     
         BNO   PROCCONT            NOT MATCHED - SKIP BUY                       
         B     PROCINVX            YES MATCHED - ACCEPT                         
*                                                                               
PROC4N   DS    0H                                                               
*                                                                               
         CLI   PROGPRO2+9,C'Y'     BUY INV#S MUST BE MATCHED?                   
         BNE   PROC5                                                            
*                                                                               
         BRAS  RE,PBLOVCK          OVERRIDES?                                   
         BE    PROC5               YES - SKIP MATCHED CHECK                     
*                                                                               
         TM    PBDSTAT,X'40'          ERROR IF NOT MATCHED                      
         BNO   PROCER3                                                          
*                                                                               
PROC5    DS    0H                                                               
*                                                                               
* MATCH INVOICE#S ATTACHED TO BUY WITH INVOICE#S ON SCREEN                      
*                                                                               
         CLI   PROGPRO2+9,C'Y'     BUY INV#S MUST BE MATCHED?                   
         JNE   PROCINVX                                                         
*                                                                               
         BRAS  RE,PBLOVCK          OVERRIDE MATCHED CHECK?                      
         JE    PROCINVX                                                         
*                                                                               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
*                                                                               
PROCIV1B CLI   0(R5),0             END OF BUY RECORD?                           
         JE    PROCINVD                                                         
         XC    WORK(L'PAYINV1),WORK                                             
         CLI   0(R5),PBNVELQ       NEW STYLED INVOICE ELEMENT?                  
         JNE   *+14                                                             
         MVC   WORK(L'PAYINV1),(PBNVINV#-PBNVELM)(R5)                           
         J     PROCIV1J                                                         
         CLI   0(R5),BYCCIDQ       CUSTOM COLUMN ELEMENT?                       
         JNE   PROCIV1E                                                         
         CLC   =AL2(8213),(BYCCSQN-BYCCELM)(R5)    STANDARD INV#?               
         JNE   PROCIV1E                                                         
         MVC   WORK(L'PAYINV1),(BYCCDATA-BYCCELM)(R5)                           
         J     PROCIV1J                                                         
PROCIV1E LLC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         J     PROCIV1B            TRY NEXT ELEMENT IN BUY RECORD               
*                                                                               
PROCIV1J BRAS  RE,CKINVLST         INV# MATCH TO SCREEN OR XINV#LST?            
         JE    PROCINVF                                                         
*                                                                               
         J     PROCIV1E            TRY NEXT INV# IN BUY RECORD                  
*                                                                               
PROCINVD J     PROCCONT            INV#S DON'T MATCH, SKIP BUY RECORD           
*                                                                               
PROCINVF DS    0H                  MATCHED TO AN ENTERED INVOICE                
*                                                                               
PROCINVX DS    0H                                                               
*                                                                               
*        VALIDATE PAY ONLY IF BUY BILLED AND BILL PAID OPTION                   
*                                                                               
         L     RF,PYABLE           SKIP IF PAYABLE IS NOT POSITIVE              
         S     RF,PAID                                                          
         BNP   PROC52                                                           
*                                                                               
         BRAS  RE,PBLOVCK          CHECK FOR OVERRIDES                          
         BE    PROC52      OVERRIDE EXISTS -SKIP PAID CHECK                     
*                                                                               
         SR    RF,RF               INDICATE BILLS MUST BE PAID                  
*                                                                               
         CLI   PROGPRO2+7,C'Y'     CHECK FOR PAY ONLY IF BILL PAID              
         BE    PROC51                                                           
*                                                                               
         CLI   PROGPRO2+7,C'P'     IF OPTION DEPENDS ON PUB                     
         BNE   PROC51A                                                          
*                                                                               
         BRAS  RE,OPTCHK              GO CHECK IT OUT                           
         BNZ   PROC51                 BILL MUST BE PAID                         
*                                                                               
PROC51A  DS    0H                                                               
*                                                                               
         CLI   PROGPRO2+6,C'Y'     CHECK FOR PAY ONLY IF BILLED OPTION          
         BNE   PROC52                                                           
*                                                                               
         LA    RF,1                BILLS NEED NOT BE PAID                       
*                                                                               
PROC51   DS    0H                                                               
*                                                                               
         GOTO1 =A(CHKBLL),DMCB,(RC),(RF),RR=RELO02                              
         BNE   EXIT                FAILED TEST                                  
*                                                                               
PROC52   DS    0H                                                               
*                                                                               
         MVI   BUYSW,C'Y'          INDICATE BUY TO PAY FOUND                    
*                                                                               
*        PAY FREE BUYS THAT ARE UNPAID                                          
*                                                                               
         CLC   PGROSS(12),GROSS    TEST ALL PAID                                
         BNE   PROC5B              NO                                           
*                                                                               
*        FULLY PAID FX BUYS MAY HAVE FX PAYABLE                                 
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN BUY                     
         BNO   PROC52A                                                          
*                                                                               
         TM    FXSW,X'80'          SKIP IF AGENCY NOT USING FX FEATURE          
         BNO   PROC52A                                                          
*                                                                               
         TM    FXSW,X'40'          SKIP IF NOT AN AMERICAN PUB                  
         BNO   PROC52A                                                          
*                                                                               
         MVC   SPAYTOTS(64),GROSS  SAVE GETINS RESULTS                          
         XC    GROSS(64),GROSS     CLEAR GETINS RETURN AREA                     
*                                                                               
*        FIND FX VALUES                                                         
*                                                                               
         GOTOR VGETINS,DMCB,PBUYREC,PVALUES,PBUYKPRD,(C'A',0),0,       X        
               =C'FX'                                                           
*                                                                               
         CLC   PGROSS(12),GROSS    CONTINUE IF FX PAYABLE                       
         MVC   GROSS(64),SPAYTOTS  RESTORE GETINS FOR BUY                       
         BNE   PROC5B              THERE IS A PAYABLE                           
*                                                                               
PROC52A  DS    0H                                                               
*                                                                               
         OC    GROSS(20),GROSS     PROCESS FREE BUYS                            
         BNZ   PROCCONT          NOT FREE - MUST HAVE BEEN FULLY PAID           
*                                SO SKIP                                        
***                                                                             
***      SEE IF "FREE" BUY HAS BEEN PAID                                        
***                                                                             
         CLI   PAIDSW,X'01'                                                     
         BE    PROCCONT            YES THEN SKIP                                
*                                                                               
PROC5B   DS    0H                                                               
*                                                                               
         AP    BUYCNT,=P'1'        ADD TO BUY COUNTER                           
*                                                                               
         CLI   ADBSW,C'Y'          ADBUYER CALL ?                               
         BNE   PROC5B5             NO                                           
*                                                                               
         BRAS  RE,ADBTST           SEE IF WE SHOULD PAY THIS BUY                
         BE    PROC5B2             YES                                          
*                                                                               
         SP    BUYCNT,=P'1'        NO - SUBTRACT FROM BUY COUNTER               
         B     PROCCONT            SKIP                                         
*                                                                               
PROC5B2  DS    0H                                                               
*                                                                               
         CP    BUYCNT,=P'200'      MORE THAN 200 BUYS ?                         
         BH    PROCER4             YES - CANNOT HANDLE                          
*                                                                               
PROC5B5  MVC   BYRECADR(4),KEY+27  SAVE DA FOR USE IF PARTIAL PAYMENT           
*                                                                               
*        FILTER ON SPECIAL DATE FOR OFFICE                                      
*                                                                               
         LA    RF,=X'80'           ASSUME BEFORE SPECIAL DATE                   
         LA    RE,PBDBDATE         BILLABLE DATE                                
*                                                                               
         CLI   SPOFFCTL,C'B'                                                    
         BE    PROC5C                                                           
*                                                                               
         LA    RE,PBDPDATE         PAYABLE DATE                                 
*                                                                               
         CLI   SPOFFCTL,C'P'                                                    
         BE    PROC5C                                                           
*                                                                               
         LA    RE,PBUYKDAT         INSERTION DATE                               
*                                                                               
PROC5C   DS    0H                                                               
*                                                                               
         CLC   0(2,RE),SPOFFDTE                                                 
         BL    *+8                                                              
         LA    RF,=X'40'                AFTER SPECIAL DATE                      
*                                                                               
         OC    SPOFFSW,0(RF)                                                    
*                                                                               
         TM    SPOFFSW,X'C0'            TEST MIX OF BEFORE AND AFTER            
         BO    PROCER5                                                          
*                                                                               
*        CHECK IF PAYING W/O CASH DISCOUNT                                      
*                                                                               
         MVC   FULL,CSHDSC          MUST SAVE OLD CD                            
*                                                                               
         CLI   NCDSW,C'N'           IF NOT PAYING CD                            
         BNE   *+10                                                             
         XC    CSHDSC,CSHDSC           CLEAR CD ORDERED                         
*                                      SINCE MAY STILL BE SET IN REC            
*                                      IT WILL BE SET TO 0 IN FIXCD             
*                                                                               
         CLC   CSHDSC,PCSHDSC       CHK FOR PAYABLE CD                          
         BE    *+8                                                              
         MVI   CDPAY,C'Y'           SET PAYABLE CD ITEM BEING PAID              
*                                                                               
         MVC   CSHDSC,FULL          RESTORE OLD CD                              
*                                                                               
         MVI   PAYSW,C'Y'          UNPAID BUY FOUND                             
*                                                                               
*------------>       SPECIAL HANDLING FOR CANADIAN BUYS                         
*                                                                               
         TM     PBDCNDA,X'80'      SKIP IF NOT CANADIAN BUY                     
         BNO    PROC5X                                                          
*                                                                               
         LAY   R1,GVALUES          ESTABLISH GST FIELDS                         
         USING GVALUES,R1                                                       
*                                                                               
         CLI    PAYGSTAX,X'FF'     FIRST TIME THRU FOR PUB OR ZONE              
         BNE    PROC5E                                             L01          
*                                                                               
         MVC    PAYGSTAX,GSTPCT      GST TAX PCT                   L01          
*                                                                               
         CLI    SVPAYGST,X'FF'     FIRST TIME THRU FOR PUB                      
         BNE    *+10                                                            
         MVC    SVPAYGST,GSTPCT      GST TAX PCT                   L01          
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LA    RF,PSTAREA          PST SAVEAREA                                 
         USING PSTAREA,RF                                                       
         LAY   RE,PSTBCKS          PST DATA AREA                                
         USING PSTBCKS,RE                                                       
*                                                                               
*        UPDATE PROVINCES THAT APPLY TO PUB/EDITION                             
*                                                                               
         CLC   PSTPROV,BLANKS      SKIP IF NO PROVINCIAL CODE                   
         BNH   *+16                                                             
         MVC   PROV,PSTPROV        SAVE PROVINCE CODE                           
         MVC   PAYPSTAX,PSTPCT     SAVE PST TAX PCT                             
         LA    RF,PSTAREAL(RF)     BUMP POINTERS                                
         LA    RE,PSTBCKSL(RE)                                                  
         BCT   R0,*-30                                                          
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
PROC5E   DS    0H                                                               
*                                                                               
         ICM    RF,15,GSTTAX        TOTAL GROSS GST                L01          
         S      RF,GSTTAXPD         PAID GST                       L01          
         ST     RF,THISBGST         THIS BUY'S PAYABLE GST         L01          
*                                                                               
         A      RF,TOTGSTP          PAYABLE GST                    L01          
         ST     RF,TOTGSTP           TOTAL PAYABLE GST             L01          
*                                                                               
         MVC    THISGSTC,GSTCODE    GSTCODE                        L01          
*                                                                  L01          
         CLC    SVPAYGST,GSTPCT    GST PERCENTAGE MUST BE THE SAME              
         BNE    PROCER6                                            L01          
*                                                                  L01          
         LA    R0,10               TEN PROVINCES                                
         LA    RF,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,RF                                                       
         LAY   RE,PSTBCKS          PST DATA AREA                                
         USING PSTBCKS,RE                                                       
*                                                                               
PROC5FL  DS    0H                                                               
*                                                                               
         CLI   PSTCODE,C' '        SKIP IF THERE IS NO CODE                     
         BNH   PROC5FC                                                          
*                                                                               
         CLI   THISPSTC,C' '       IF NONE FOUND YET                            
         BH    *+10                                                             
         MVC   THISPSTC,PSTCODE       USE THIS PSTCODE                          
*                                                                               
         CLC   THISPSTC,PSTCODE    PST CODE MUST BE CONSTANT                    
         BNE   PROCER7                                                          
*                                                                               
         ICM   R2,15,PSTTAX        TOTAL GROSS PST                L01           
         S     R2,PSTTAXPD         PAID PST                       L01           
         ST    R2,THISBPST         THIS BUY'S PAYABLE PST         L01           
         A     R2,TOTPSTP          PAYABLE PST                    L01           
         ST    R2,TOTPSTP           TOTAL PAYABLE PST             L01           
*                                                                  L01          
         ICM   R2,15,PST$BS        TOTAL GROSS PST BASIS                        
         S     R2,PST$BSPD         PAID PST BASIS                               
         ST    R2,THISB$BS         THIS BUY'S PAYABLE PST                       
         A     R2,TOT$BSP          PAYABLE PST BASIS                            
         ST    R2,TOT$BSP          TOTAL PAYABLE PST BASIS                      
*                                                                               
         CLC   PROV,PSTPROV        PROVINCES MUST BE IN SAME ORDER              
         BNE   PROCER7                                                          
*                                                                               
         CLC   PSTPCT,PAYPSTAX     PST PERCENTAGE MUST BE THE SAME L01          
         BNE   PROCER7                                                          
*                                                                  L01          
PROC5FC  DS    0H                                                               
*                                                                               
         LA    RF,PSTAREAL(RF)     NEXT PROVINCE                                
         LA    RE,PSTBCKSL(RE)                                                  
         BCT   R0,PROC5FL                                                       
*                                                                               
PROC5X   DS    0H                                                               
*                                                                               
         DROP  RE,RF,R1                                                         
*                                                                               
*-------------------------------------------------------------!                 
*-  DO NOT CLEAR CONTENTS OF REGISTERS 6,7 & 8          ------!                 
*-  THEY ARE USED AS ACCUMULATORS UNTIL THERE IS A KEY  ------!                 
*-  BREAK.                                              ------!                 
*-------------------------------------------------------------!                 
*                                                                               
PROC6    DS    0H                                                               
*                                                                               
         LM    R6,R8,PAYTOTS       GET PAYABLE GROSS,CD,NET LESS CD             
*                                                                               
         A     R6,GROSS            UPDATE PAYABLE GROSS                         
         S     R6,PGROSS                                                        
*                                                                               
         A     R7,CSHDSC           UPDATE PAYABLE CASH DISCOUNT                 
         S     R7,PCSHDSC                                                       
*                                                                               
         A     R8,PYABLE           PAYABLE NET = GROSS-COMM-CSH DSC             
         S     R8,PAID                                                          
*                                                                               
         STM   R6,R8,PAYTOTS       SAVE PAYABLE GROSS, CD, NET LESS CD          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*------------------> CHECK IF DISPLAYING PAYABLE DOLLARS                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GOTO1 =A(TSTSUB),DMCB,(RC),RR=RELO02                                   
*                                                                               
         CLI   ADBSW,C'Y'          ADBUYER CALL ?                               
         BNE   *+8                 NO                                           
         BRAS  RE,ABDTLWRK         SAVE BUY INFO FOR ADBUYER REPLY              
*                                                                               
*        CALCULATE FX AMOUNTS FOR CANADIAN AGENCIES                             
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN BUY                     
         BNO   PROC6FXX                                                         
*                                                                               
         TM    FXSW,X'80'          SKIP IF AGENCY NOT USING FX FEATURE          
         BNO   PROC6FXX                                                         
*                                                                               
         TM    FXSW,X'40'          SKIP IF NOT AN AMERICAN PUB                  
         BNO   PROC6FXX                                                         
*                                                                               
*        CHECK IF BUY CONTAINS FX ELEMENT                                       
*                                                                               
         NI    FXSW,X'FF'-X'10'    INIT BUY HAS FX SWITCH                       
*                                                                               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         MVI   ELCODE,X'44'        ADDITIONAL CHARGE ELEMENT (PACELEM)          
*                                                                               
PROC6FXL DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   PROC6FXD            END OF AC ELEMENTS                           
*                                                                               
         USING PACELEM,R5          ESTABLISH ADDITIONAL CHG ELM                 
*                                                                               
         CLC   PACCODE,=C'FX'      IF FX ADDITIONAL CHARGE                      
         BNE   *+8                                                              
         OI    FXSW,X'30'             INDICATE BUY HAS FX ELEMENT               
*                                     AND PAYMENT HAS FX COMPONENT              
PROC6FXC DS    0H                                                               
         B     PROC6FXL                                                         
*                                                                               
PROC6FXD DS    0H                                                               
*                                                                               
         TM    FXSW,X'10'          SKIP IF NO FX AMOUNT IN BUY                  
         BNO   PROC6FXX                                                         
*                                                                               
*        GET FX AMOUNTS FOR BUY                                                 
*                                                                               
         GOTO1 VGETINS,DMCB,PBUYREC,PVALUES,PBUYKPRD,(C'A',0),=C'PST', X        
               =C'FX'                                                           
*                                                                               
         TM    PBUYCNTL,X'80'      IF DELETED BUY                               
         BZ    *+10                                                             
         XC    GROSS(20),GROSS        SET ORDERED AMOUNTS TO ZERO               
*                                                                               
*        ACCUMULATE FX PAYABLES                                                 
*                                                                               
         LAY   RF,XPAYTOTS                                                      
         LM    R6,R8,0(RF)         GET PAYABLE GROSS,CD,NET LESS CD             
*                                                                               
         A     R6,GROSS            UPDATE PAYABLE GROSS                         
         S     R6,PGROSS                                                        
*                                                                               
         A     R7,CSHDSC           UPDATE PAYABLE CASH DISCOUNT                 
         S     R7,PCSHDSC                                                       
*                                                                               
         A     R8,PYABLE           PAYABLE NET = GROSS-COMM-CSH DSC             
         S     R8,PAID                                                          
*                                                                               
         STM   R6,R8,0(RF)         SAVE PAYABLE GROSS, CD, NET LESS CD          
*                                                                               
         CLI   ADBSW,C'Y'          ADBUYER CALL ?                               
         BNE   *+8                 NO                                           
         BRAS  RE,ABDTLFX          SAVE FX INFO FOR ADBUYER REPLY               
*                                                                               
PROC6FXX DS    0H                                                               
*                                                                               
*-------------------------------------------------------------!                 
*                                                                               
         CLI   MLTREQSW,C'Y'       OUTPUT ONE REQHDR PER BUY ?                  
         BNE   PROC6X              NO                                           
*                                                                               
         TM    PBDCNDA,X'80'       CANADIAN BUY ?                               
         BO    PROCER8             YES - ERROR                                  
*                                                                               
         CP    PBDCD,=P'0'         CASH DISCOUNT BUY ?                          
         BH    PROCER9             YES - ERROR                                  
*                                                                               
PROC6X   DS    0H                                                               
*                                                                               
         B     PROCCONT            READ NEXT BUY RECORD                         
*                                                                               
*                                                                               
PROCCONT BRAS  RE,SEQ              READ NEXT BUY                                
         B     PROCLOOP                                                         
*                                                                               
PROCDONE DS    0H                                                               
*                                                                               
         ICM   R0,15,TOTPSTP       PAYABLE PST                                  
         LPR   R0,R0                                                            
***      CLM   R0,15,=F'10000000'  PST PAYABLE MUST BE LT $100,000.00           
         CLM   R0,15,=X'7FFFFFFF'  PST PAYABLE MUST BE < $21,474.836.47         
         BNL   PROCER12                                                         
*                                                                               
         ICM   R0,15,TOT$BSP       PAYABLE PST BASIS                            
         LPR   R0,R0                                                            
         CLM   R0,15,=X'7FFFFFFF'  PST PAYABLE BASIS MUST BE LT                 
         BNL   PROCER13               $20,000,000.00                            
*                                                                               
         EJECT                                                                  
*------------------->  SEE IF BUY DOLLARS AND INVOICE AGREE                     
PROCX    DS    0H                                                               
*                                                                               
         CLI   BUYSW,C'Y'          ERROR IF THERE ARE NO BUYS                   
         BNE   PROCER10                                                         
*                                                                               
*                                                                               
         CLI   PAYSW,C'Y'          ERROR IF THERE ARE NO BUY(S) TO PAY          
         BNE   PROCER11                                                         
*                                                                               
* SUM AMOUNTS IN BTOT                                                           
*                                                                               
         LA    R1,BAMTS            SUM OF INVOICES                              
         LA    RE,PBMXIV#Q                                                      
         SR    R0,R0                                                            
*                                                                               
PROCX2   L     R4,0(R1)                                                         
         AR    R0,R4                                                            
         LA    R1,8(R1)                                                         
         BCT   RE,PROCX2                                                        
*                                                                               
         ST    R0,BTOT             SUM OF ALL INVOICES                          
*                                                                               
         MVI   BYTE3,C'Y'          ASSUME OKAY TO PAY                           
*                                                                               
         LM    R6,R8,PAYTOTS       GET BUY FILE TOTALS                          
*                                                                               
         AR    R8,R7         ADD CD TO NET/NET - MAKING GROSS-COM(NET)          
*                                                                               
         CLI   GNOPT,C'G'       --> PAYING BY GROSS                             
         BNE   *+6                                                              
         LR    R8,R6               LOAD GROSS                                   
*                                                                               
         OC    TOTGSTP,TOTGSTP    IF ANY GST TAX PRESENT                        
         BZ    *+8                                                              
         A     R8,TOTGSTP            ADD PAYABLE GST TO FILE TOTAL              
*                                                                               
         LA    R1,10               TEN PROVINCES                                
         LAY   RE,PSTBCKS          PST DATA AREA                                
         USING PSTBCKS,RE                                                       
*                                                                               
         A     R8,TOTPSTP          ADD IN PST                                   
         LA    RE,PSTBCKSL(RE)                                                  
         BCT   R1,*-8                                                           
*                                                                               
         DROP  RE                                                               
*                                                                               
         SR    R0,R8               GET DIFF BETWEEN INVOICE AND FILE            
         LCR   R0,R0                                                            
         LPR   RF,R0                                                            
*                                                                               
         C     RF,=F'0'            OKAY IF FILE AND INVOICES AGREE              
         BE    PROCX10                                                          
*                                                                               
         BRAS  RE,SETTOLRA         SET TOLERANCE AMOUNT                         
*                                                                               
         C     RF,FULL                                                          
         BNH   PROCX3              DIFFERENCE LT .50 - DO ALTER BELOW           
*                                                                               
         CLC   PAYMD+1(4),=C'TEST'                                              
         BE    NOPAY                                                            
*                                                                               
*   CHECK FOR POSSIBLE PARTIAL PAYMENT AGAINST ONE ONLY BUY                     
*                                                                               
         MVI   PARTSW,C' '         TURN OFF PART-PMT SWITCH                     
*                                                                               
         CP    BUYCNT,=P'1'        ONLY ONE BUY ?                               
         BNE   NOPAY               NO - NO PARTIAL PMT ALLOWED                  
*                                                                               
         CLI   PROGPRO2+11,C'Y'    PARTIAL PMT PERMITTED ?                      
         BNE   NOPAY               NO                                           
*                                                                               
         CLI   CRCKSW,0            SEE IF REVERSING                             
         BNE   NOPAY               YES - NO PARTIAL PMT ALLOWED                 
*                                                                               
         L     R1,BTOT             TOTAL FOR INVOICES                           
*                                                                               
         LTR   R1,R1               INVOICE ZERO ?                               
         BZ    NOPAY               YES - DON'T PAY                              
         BP    PPMT10              INVOICE POSITIVE                             
*                                  R8 STILL CONTAINS TOTG FROM PAYTOTS          
         CR    R1,R8               INVOICE GT TOTG ?                            
         BL    NOPAY               NO - NOT PARTIAL IF INVC NEGATIVE            
         B     PPMT90              OK                                           
*                                                                               
PPMT10   CR    R1,R8               INVOICE GT TOTG ?                            
         BH    NOPAY               YES - NOT PARTIAL                            
*                                                                               
PPMT90   DS    0H                                                               
*                                                                               
         MVI   PARTSW,C'Y'         SET PART-PMT-SWITCH                          
         B     PAYEM                                                            
*                                                                               
*  IF THERE IS A DIFFERENCE - FIND LARGEST INVOICE AMT TO ALTER                 
*                                                                               
PROCX3   DS    0H                                                               
         LA    R1,BAMTS                                                         
         LR    R5,R1                                                            
         LA    RE,PBMXIV#Q                                                      
         XC    FULL,FULL                                                        
PROCX4   CLC   FULL(4),0(R1)                                                    
         BNL   PROCX6                                                           
         MVC   FULL(4),0(R1)                                                    
         LR    R5,R1                                                            
PROCX6   LA    R1,8(R1)                                                         
         BCT   RE,PROCX4                                                        
         LR    RF,R0               SAVE DIFFERENCE                              
         A     R0,0(R5)                                                         
         ST    R0,0(R5)                                                         
         A     RF,BTOT             ADD DIFFERENCE TO INVOICE TOTAL              
         ST    RF,BTOT                                                          
*                                                                               
*        IF DOING FX NEED TO ALLOCATE XPAYTOTS TO XBAMTS                        
*                                                                               
PROCX10  DS    0H                                                               
*                                                                               
         TM    FXSW,X'20'          SKIP IF NO FX PAYMENT                        
         BNO   *+8                                                              
         BRAS  RE,FXALLOC                                                       
*                                                                               
         B     PAYEM                                                            
*                                                                               
*AMOUNTS DISAGREE                                                               
*                                                                               
NOPAY    DS    0H                                                               
         MVI   BYTE3,C'N'                                                       
         B     PAYEM                   PAYEM WILL RETURN TO NOPAY1              
*                                                                               
NOPAY1   BRAS  RE,ICLREQ                                                        
         LA    R3,ICLERR                                                        
*                                                                               
         LA    R2,PAYMDH           CURSOR TO MEDIA                              
*                                                                               
         CLC   PAYMD+1(4),=C'TEST'                                              
         BNE   ERROR                                                            
*                                                                               
         XC    PAYMSG,PAYMSG                                                    
         FOUT  PAYMSGH,=C'ICL REQSTD. G='                                       
*                                                                               
         CLI   ICLOPT,C'N'         SEE IF AUTO ICL TO BE CREATED                
         BNE   *+10                                                             
         MVC   PAYMSG(11),BLANKS                                                
*                                                                               
*        FIND TOTAL PST FOR ALL INVOICES AND PROVINCES                          
*                                                                               
         SR    R4,R4                                                            
         LA    R0,10               10 PROVINCES                                 
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
*                                                                               
         A     R4,TOTPSTP          ACCUMULATE TOTAL PST                         
         LA    RF,PSTBCKSL(RF)     FOR ALL PROVINCES TOGETHER                   
         BCT   R0,*-8                                                           
*                                                                               
         LR    R5,R4               SAVE TOTAL PST FOR ALL INVOICES              
*                                                                               
         DROP  RF                                                               
*                                                                               
         TM    FXSW,X'80'          SKIP IF AGENCY NOT USING FX FEATURE          
         BNO   NOPAY11                                                          
*                                                                               
         XC    PAYMSG1,PAYMSG1                                                  
         FOUT  PAYMSG1H                                                         
*                                                                               
         TM    FXSW,X'20'          SKIP IF PAYMENT INVOLVES FX                  
         BO    NOPAY1A                                                          
*                                                                               
NOPAY11  DS    0H                                                               
*                                                                               
         LA    R4,PAYMSG+14                                                     
         L     R0,PAYTOTS                                          L01          
         A     R0,TOTGSTP          GST PAYABLE                     L01          
         AR    R0,R5               PST PAYABLE                     L01          
*                                                                               
         EDIT  (R0),(12,0(R4)),2,ALIGN=LEFT,MINUS=YES              L01          
*                                                                               
         AR    R4,R0                                                            
*                                                                               
         MVC   1(2,R4),=C'N='                                                   
*                                                                               
         L     R0,PAYTOTS+4                                                     
         A     R0,PAYTOTS+8                                                     
         A     R0,TOTGSTP          GST PAYABLE                     L01          
         AR    R0,R5               PST PAYABLE                     L01          
*                                                                               
         EDIT  (R0),(12,3(R4)),2,ALIGN=LEFT,MINUS=YES                           
         AR    R4,R0                                                            
         LA    R4,3(R4)                                                         
         MVC   1(5,R4),=C'N-CD='                                                
         L     R0,PAYTOTS+8                                                     
         A     R0,TOTGSTP          GST PAYABLE                     L01          
         AR    R0,R5               PST PAYABLE                     L01          
         EDIT  (R0),(12,6(R4)),2,ALIGN=YES,MINUS=YES                            
*                                                                               
         B     NOPAY2                                                           
*                                                                               
NOPAY1A  DS    0H                                                               
*                                                                               
         LA    R4,PAYMSG+14                                                     
         L     R0,PAYTOTS                                          L01          
         A     R0,TOTGSTP          GST PAYABLE                     L01          
         AR    R0,R5               PST PAYABLE                     L01          
         EDIT  (R0),(12,0(R4)),2,MINUS=YES              L01                     
         AHI   R4,12                                                            
         MVC   1(2,R4),=C'N='                                                   
         L     R0,PAYTOTS+4                                                     
         A     R0,PAYTOTS+8                                                     
         A     R0,TOTGSTP          GST PAYABLE                     L01          
         AR    R0,R5               PST PAYABLE                     L01          
         EDIT  (R0),(12,3(R4)),2,MINUS=YES                                      
         AHI   R4,12                                                            
         LA    R4,3(R4)                                                         
         MVC   1(5,R4),=C'N-CD='                                                
         L     R0,PAYTOTS+8                                                     
         A     R0,TOTGSTP          GST PAYABLE                     L01          
         AR    R0,R5               PST PAYABLE                     L01          
         EDIT  (R0),(12,6(R4)),2,MINUS=YES                                      
*                                                                               
         XC    PAYMSG1,PAYMSG1                                                  
         FOUT  PAYMSG1H                                                         
*                                                                               
         TM    FXSW,X'40'          SKIP IF NOT AN AMERICAN PUB                  
         BNO   NOPAY2                                                           
*                                                                               
         MVC   PAYMSG1+12(2),=C'FX'                                             
         LA    R4,PAYMSG1+14                                                    
         LAY   R3,XPAYTOTS         FX TOTALS                       L01          
         L     R0,0(R3)            GROSS                           L01          
         EDIT  (R0),(12,0(R4)),2,MINUS=YES              L01                     
         AHI   R4,12                                                            
         L     R0,4(R3)            CD                                           
         A     R0,8(R3)            NET-CD                                       
         EDIT  (R0),(12,3(R4)),2,MINUS=YES                                      
         AHI   R4,12                                                            
         LA    R4,3(R4)                                                         
         L     R0,8(R3)            NET-CD                                       
         EDIT  (R0),(12,6(R4)),2,MINUS=YES                                      
*                                                                               
NOPAY2   DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS - PROCERR'                
***********************************************************************         
*                                                                     *         
*        PROC SECTION ERROR MESSAGES                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PROCER1  DS    0H                                                               
*                                  CONTROL DATE ERROR                           
         LA    R2,PAYCKH           CURSOR TO CONTROL DATE                       
         LA    R3,DTERR            INVALID DATE                                 
         LHI   RE,D#CTDTE          ADBUYER CONTROL DATE ID                      
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER2  DS    0H                  MISSING TEARSHEET                            
*                                                                               
         LA    R2,PAYMDH           CURSOR TO MEDIA                              
         NI    PAYMDH+4,X'DF'      UNVALIDATE MEDIA                             
         LA    R3,PAYTERR          TEARSHEET REQUIRED                           
         LHI   RE,D#MEDCOD         ADBUYER MEDIA ID                             
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER3  DS    0H                  UNMATCHED INSERTION                          
*                                                                               
         LA    R2,PAYMDH           CURSOR TO MEDIA                              
         NI    PAYMDH+4,X'DF'      UNVALIDATE MEDIA                             
         LA    R3,PAYMERR          UNMATCHED INSERTION                          
         LHI   RE,D#MEDCOD         ADBUYER MEDIA ID                             
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER4  DS    0H                                                               
*                                                                               
         LA    R3,MAXADBPY   CANNOT PAY MORE THAN 200 BUYS THRU ADBUYER         
         LHI   RE,D#MEDCOD         ADBUYER MEDIA ID                             
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER5  DS    0H                                                               
*                                                                               
         LA    R3,MIXERR           DATES SPAN SEPARATION DATE                   
         LHI   RE,D#MEDCOD         ADBUYER MEDIA ID                             
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER6  DS    0H                                                               
*                                                                               
         LA    R2,PAYPBH           POINT TO PUB HDR                L01          
         LA    R3,1                ERROR NUMBER FOR ADBUYER                     
         XC    PAYMSG,PAYMSG                                                    
         FOUT  PAYMSGH,=C'INVALID MIX OF GST TAX %'                L01          
         LHI   RE,D#PUBCOD         PUB CODE IN ERROR FOR ADBUYER                
*                                                                               
         B     PROCERR1                                                         
*                                                                               
PROCER7  DS    0H                                                               
*                                                                               
         LA    R2,PAYPBH           POINT TO PUB HDR                L01          
         LA    R3,1                ERROR NUMBER FOR ADBUYER                     
         XC    PAYMSG,PAYMSG                                                    
         FOUT  PAYMSGH,=C'INVALID MIX OF PST TAX %'                             
         LHI   RE,D#PUBCOD         PUB CODE IN ERROR FOR ADBUYER                
*                                                                               
         B     PROCERR1                                                         
*                                                                               
PROCER8  DS    0H                  NO CANADIAN BUYS FOR SINGLY                  
*                                                                               
         LA    R2,PAYOPH           SET CURSOR TO OPTIONS FIELD                  
         OI    6(R2),X'C0'         FORCE CURSOR                                 
         LA    R3,NOCANBY          ERROR MESSAGE                                
         LHI   RE,D#MEDCOD         SET MEDIA AS ERROR FIELD FOR AB              
*                                                                               
         BRAS  RE,DISPBUY          SHOW BUY AT BOTTOM OF SCREEN                 
         NI    PAYOPH+4,X'DF'      SET TO RE-EDIT HEADLINES                     
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER9  DS    0H                  NO CD BUYS FOR SINGLY                        
*                                                                               
         LA    R2,PAYOPH           SET CURSOR TO OPTIONS FIELD                  
         OI    6(R2),X'C0'         FORCE CURSOR                                 
         LA    R3,NOCDBUY          ERROR MESSAGE                                
         LHI   RE,D#MEDCOD         SET MEDIA AS ERROR FIELD FOR AB              
*                                                                               
         BRAS  RE,DISPBUY          SHOW BUY AT BOTTOM OF SCREEN                 
         NI    PAYOPH+4,X'DF'      SET TO RE-EDIT HEADLINES                     
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER10 DS    0H                  NO BUYS ON FILE                              
*                                                                               
         LA    R2,PAYMDH                                                        
         LHI   RE,D#MEDCOD         ADBUYER MEDIA ID                             
*                                                                               
         LA    R3,NOBUYERR         NO BUYS FOUND                                
*                                                                               
         CLI   PROGPRO2+9,C'N'    CHANGE MSG IF A0A PROFILE USED                
         BNE   *+8                                                              
         CLI   PROGPRO2+10,C'N'                                                 
         BE    *+8                                                              
         LA    R3,NA0ABERR         NO A0A BUYS FOUND                            
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER11 DS    0H                  NO BUYS TO PAY                               
*                                                                               
         LA    R2,PAYMDH                                                        
         LHI   RE,D#MEDCOD         ADBUYER MEDIA ID                             
*                                                                               
         LA    R3,NOUNPERR         NO UNPAID BUYS                               
*                                                                               
         CLI   PROGPRO2+9,C'N'    CHANGE MSG IF A0A PROFILE USED                
         BNE   *+8                                                              
         CLI   PROGPRO2+10,C'N'                                                 
         BE    *+8                                                              
         LA    R3,NA0AUERR         NO UNPAID A0A BUYS FOUND                     
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER12 DS    0H                  NO BUYS TO PAY                               
*                                                                               
         LA    R2,PAYMDH                                                        
         LHI   RE,D#MEDCOD         ADBUYER MEDIA ID                             
*                                                                               
         LA    R3,PSTLIMIT         PST MUST BE UNDER $100,000.00                
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCER13 DS    0H                  NO BUYS TO PAY                               
*                                                                               
         LA    R2,PAYMDH                                                        
         LHI   RE,D#MEDCOD         ADBUYER MEDIA ID                             
*                                                                               
         LA    R3,PSTBSLIM         PST BASIS MUST BE LT $21,000,000.00          
*                                                                               
         B     PROCERR                                                          
*                                                                               
PROCERR  DS    0H                  EXIT FOR IN PROGRAM ERROR MSGS               
*                                                                               
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         STH   RE,ADBERFLD           ADBUYER CALL                               
*                                                                               
         B     ERRORXT                                                          
*                                                                               
PROCERR1 DS    0H                  EXIT IF INPROGRAM ERROR MSG                  
*                                                                               
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         STH   RE,ADBERFLD           ADBUYER CALL                               
*                                                                               
         TM    FXSW,X'80'          SKIP IF AGENCY NOT USING FX FEATURE          
         BNO   PROCERR2                                                         
*                                                                               
         XC    PAYMSG1,PAYMSG1                                                  
         FOUT  PAYMSG1H                                                         
*                                                                               
PROCERR2 DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS - PAYEM'                  
***********************************************************************         
*                                                                     *         
*        HAVE VERIFIED OKAY TO PAY - NOW DO PAYMENT                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PAYEM    DS    0H                                                               
*                                                                               
         CLI   BYTE3,C'N'          NOT PAYING ?                                 
         BE    PAYEMD              YES - SKIP "MULTI-REQHDR" WORK               
*                                                                               
         CLI   MLTREQSW,C'Y'       OUTPUT ONE REQHDR PER BUY ?                  
         BNE   PAYEMD              NO -"NORMAL" PROCESSING                      
*                                                                               
         BRAS  RE,PREPREQ     PREPARE FOR PROCESSING ONE REQHDR PER BUY         
*&&DO                                                                           
*                             PREPARE FOR PROCESSING ONE REQHDR PER BUY         
*                                                                               
         XC    GROSS(64),GROSS     CLEAR GETINS OUTPUT                          
         MVC   SPAYTOTS(PAYTOTSL),PAYTOTS   SAVE FOR "COMPLETION" MSG           
         XC    PAYTOTS(PAYTOTSL),PAYTOTS    CLEAR FOR SINGLE BUY CALC'S         
         MVC   BAMTS(8),SBAMTS     TO RETAIN THE 4 BYTES AFTER INVOICE          
*                                  AMOUNT WHICH WILL BE REPLACED LATER          
         MVC   PAYREP,PREP-PAYTOTSD+SPAYTOTS      RESTORE ANY REP CODE          
         MVC   PAYREPTP,PREPTYP-PAYTOTSD+SPAYTOTS RESTORE ANY REP TYPE          
*                                                                               
         MVC   NBAMTS(8),SBAMTS                                                 
         MVC   SAVPRX(SAVFLDL),SAVPR    SAVE TO RESTORE FOR "NEXT REC"          
         MVI   PARTSW,C' '         TURN OFF PART-PMT SWITCH                     
*&&                                                                             
PAYEMD   MVI   DMOUTBTS,0          SET FOR NO DMCB ERROR TESTS                  
*                                                                               
         CLI   PROGPRO2+0,C'I'     SEE IF CHECK DATE CALCULATED (I/P)           
         BE    *+8                 FROM INVOICE DATE (INVDATE)                  
         CLI   PROGPRO2+0,C'P'     SEE IF CHECK DATE CALCULATED (I/P)           
         BNE   PAYEM1              FROM INVOICE DATE (INVDATE)                  
*                                                                               
         OC    CKDATE,CKDATE       SEE IF CHECK DATE INPUT                      
         BNE   PAYEM1              FROM INVOICE DATE (INVDATE)                  
*                                                                               
         OC    CKDATE,CKDATE       SEE IF CHECK DATE INPUT                      
         BNZ   PAYEM1              YES - NO CALCULATION                         
*                                                                               
         OC    INVDATE,INVDATE     MAY NOT HAVE INVOICE DATE                    
         BZ    PAYEM1              IF TEST                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(3,INVDATE),(0,WORK)                                
*                                                                               
         ZIC   R4,SVDYNCD          DAYS TO ADD TO INVOICE DATE                  
*                                                                               
         CLI   CDPAY,C'N'                                                       
         BE    PAYEM0                                                           
*                                                                               
         ZIC   R4,SVDYCD           USE CD PAYMENT INCREMENT                     
*                                                                               
PAYEM0   ST    R4,DMCB+8                                                        
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+6                                          
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,CKDATE)                               
*                                                                               
         CLC   WORK+6(6),SVDATE       CAN'T BE BEFORE TODAY                     
         BNL   PAYEM0C                                                          
*                                                                               
*        PROGPRO2+3 HAS MAXIMUM NUMBER OF DAYS BEFORE TODAY                     
*        CALCULATED CONTROL DATE CAN BE                                         
*                                                                               
         CLI   PROGPRO2+3,0                                                     
         BE    PAYEM0B               IF ZERO - USE TODAY                        
*                                                                               
         LLC   R4,PROGPRO2+3         MAX FROM A0A PROFILE                       
         BRAS  RE,CKXMAXDY           CHECK FOR EXTENDED MAX DAYS                
         LLC   RE,HALF2              ADDITIONAL MAX DAYS TO CLEAR               
         AR    R4,RE                                                            
*                                                                               
         LCR   R4,R4                 MAKE MINUS                                 
         ST    R4,DMCB+8                                                        
         GOTO1 VADDAY,DMCB,SVDATE,WORK+12                                       
*                                                                               
         CLC   WORK+6(6),WORK+12                                                
         BNL   PAYEM0C                                                          
*                                                                               
         MVC   WORK+6(6),WORK+12      USE MAXIMUM DATE                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,CKDATE)                               
*                                                                               
         B     PAYEM0C                                                          
*                                                                               
PAYEM0B  GOTO1 VDATCON,DMCB,(0,SVDATE),(3,CKDATE)                               
*                                                                               
         B     PAYEM0F                                                          
*                                   IF SO USE TODAY'S DATE                      
PAYEM0C  GOTO1 VADDAY,DMCB,SVDATE,WORK+12,61                                    
*                                                                               
         CLC   WORK+6(6),WORK+12   CAN'T EXCEED 61 DAYS BEYOND TODAY            
         BNH   PAYEM0F                                                          
*                                                                               
         XC    PAYXLIN,PAYXLIN                                                  
         MVC   PAYXLIN(24),=C'CALCULATED CONTROL DATE='                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,CKDATE),(8,PAYXLIN+25)                           
*                                                                               
         FOUT  PAYXLINH                                                         
*                                                                               
         LA    R3,CHKERR                                                        
         LA    R2,PAYINVDH                                                      
         NI    PAYINVDH+4,X'DF'    INVALIDATE INVOICE DATE                      
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#INVDAT           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
*                                                                               
         B     ERROR                                                            
*                                                                               
PAYEM0F  CLI   PROGPROF+10,C'C'     SEE IF CCUSA                                
         BE    PAYEM1               YES - DON'T DISPLAY CONTROL DATE            
*                                                                               
         MVC   PAYXLIN(24),=C'CALCULATED CONTROL DATE='                         
         GOTO1 VDATCON,DMCB,(3,CKDATE),(8,PAYXLIN+25)                           
         FOUT  PAYXLINH                                                         
*                                                                               
*                                                                               
PAYEM1   DS    0H                                                               
*                                                                               
         CLI   PARTSW,C'Y'         DOING PART-PAYMENT                           
         BNE   PAYEM1F             NO                                           
*                                                                               
*   MUST REREAD ONE ONLY BUY NOW FOR FIELDS NEEDED IN PARTPMT PROC              
*                                                                               
         MVC   AREC,ABUYIO         POINT TO BUY I/O AREA                        
         MVC   KEY+27(4),BYRECADR        REPLACE DA IN KEY                      
*                                                                               
         GOTO1 GETREC                    REREAD                                 
*                                                                               
         BRAS  RE,TSTDMCB                                                       
*                                                                               
PAYEM1B  DS    0H    SEND ERROR IF ANY ADDITIONAL CHARGE ELEMENTS FOUND         
*                                                                               
         LA    R3,NOPRTADC         NO PART PMT IF HAS ADDN'L CHARGES            
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#MEDCOD           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
*                                                                               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         MVI   ELCODE,X'44'        ADDITIONAL CHARGE ELEMENT (PACELEM)          
         BRAS  RE,NEXTEL                                                        
         BE    ERROR               SEND ERROR MESSAGE                           
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         LAY   R2,SVBLBDT          POINT TO BILLABLE DATE SAVE AREA             
*                                                                               
         MVC   0(L'SVBLBDT,R2),PBDBDATE  SAVE BILLABLE DATE                     
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN BUY                     
         BNO   *+12                                                             
         BRAS  RE,BLDPST           UPDATE PST ELEMENT                           
         BNE   EXXMOD              IN CASE OF ERRORS                            
*                                                                               
*                          MODIFY BUY AND "RE-DO" GETINS  (PARTPMT)             
         BRAS  RE,PARTPMT                                                       
*                                                                               
         L     R6,GROSS            GROSS                                        
         L     R7,CSHDSC           CASH DISC                                    
         LR    R8,R6               GROSS                                        
         S     R8,AGYCOM           LESS AGENCY COMMISSION AND                   
         S     R8,CSHDSC             CASH DISC = NET/NET                        
         STM   R6,R8,PAYTOTS       REPLACE PAYABLE GROSS, CD, NET/NET           
*                                                                               
PAYEM1F  DS    0H                                                               
*                                                                               
         CLI   BYTE3,C'N'          NOT REALLY PAYING ?                          
         BE    PAYEM1J             YES - NO PAY - NO LOCK TEST NEEDED           
*                                                                               
         MVC   KEY,STARTKEY        RESTORE STARTING KEY                         
         BRAS  RE,HIGH                                                          
         BRAS  RE,TSTDMCB                                                       
*                                  TEST FOR LOCKED CLIENT OR CLT/PUB            
         MVI   BYTE4,0             C'U' IN BYTE4 = UNLOCK                       
         BRAS  RE,TSTLOK           GO TEST LOCKS - IF UNLOCKED, LOCK            
         BE    PAYEM1J             OK - CONTINUE                                
*                                                                               
         LA    R2,PAYMDH           CURSOR TO MEDIA                              
         LA    R3,DATALOCK         REC LOCKED FOR OFFLINE PROCESSING            
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#MEDCOD           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
         B     ERROR                                                            
*                                                                               
PAYEM1J  DS    0H                                                               
*                                                                               
         CLI   MLTREQSW,C'Y'       OUTPUT ONE REQHDR PER BUY ?                  
         BE    PAYEM1R           YES - CAN'T DO CLEARANCE STAT REC HERE         
*                                                                               
PAYEM1N  DS    0H                                                               
*                                                                               
         GOTOR BLDST,DMCB,PAYTOTS  UPDATE STATUS RECORD                         
         JNE   ERRORXT1                                                         
*                                  AND GET SEQUENCE NUMBER                      
         TM    FXSW,X'20'          SKIP IF NO FX TO BE PAID                     
         BZ    PAYEM1R                                                          
*                                                                               
         OI    FXSW,X'08'          NOW DO FX PAYMENT                            
         LAY   RF,XPAYTOTS                                                      
         GOTOR BLDST,DMCB,0(RF)                                                 
         JNE   ERRORXT1                                                         
         NI    FXSW,X'FF'-X'08'    TURN OFF FX INDICATOR                        
*                                                                               
PAYEM1R  DS    0H                                                               
*                                                                               
         LA    R4,15               FOR EXEC                                     
*                                                                               
         CLC   SAVPR,=C'ALL'                                                    
         BNE   *+8                                                              
         LA    R4,12                                                            
*                                                                               
         CLI   BPUB+4,X'FF'        TEST ALL ZONES/EDTS                          
         BNE   *+8                                                              
         BCTR  R4,0                YES - COMPARE ONLY THRU 8 DIGIT PUB          
         BCTR  R4,0                                                             
*                                                                               
         STC   R4,SVCLCLEN         SAVE COMPARE LENGTH                          
*                                                                               
         MVC   KEY,STARTKEY        RESTORE STARTING KEY                         
         BRAS  RE,HIGH                                                          
         B     PAYEM22                                                          
*                                                                               
PAYEM2   DS    0H                                                               
*                                                                               
         CLC   KEY,SVSRCHKY        IF SEARCH KEY CHANGED                        
         BE    PAYEM21                                                          
*                                                                               
         MVC   KEY,SVSRCHKY           RESTORE SEARCH KEY                        
*                                                                               
         BRAS  RE,HIGH                RESTORE READ SEQUENCE                     
*                                                                               
PAYEM21  DS    0H                                                               
*                                                                               
         BRAS  RE,SEQ              READ NEXT BUY                                
*                                                                               
PAYEM22  DS    0H                                                               
*                                                                               
         MVC   SVSRCHKY,KEY        SAVE SEARCH KEY                              
*                                                                               
         CLI   BYTE3,C'N'          NOT REALLY PAYING ?                          
         BE    PAYEM2A             YES - SKIP MULTI-REQ-HDR WORK                
*                                                                               
         CLI   MLTREQSW,C'Y'       OUTPUT ONE REQHDR PER BUY ?                  
         BNE   PAYEM2A                                                          
*                                                                               
         MVI   MRQSTAT,C' '        CLEARANCE STAT REC NOT YET WRITTEN           
         XC    PAYTOTS(PAYTOTSL),PAYTOTS    CLEAR FOR SINGLE BUY CALC'S         
         MVC   BAMTS(8),SBAMTS     TO RETAIN THE 4 BYTES AFTER INVOICE          
*                                                                               
         MVC   PAYREP,PREP-PAYTOTSD+SPAYTOTS      RESTORE ANY REP CODE          
         MVC   PAYREPTP,PREPTYP-PAYTOTSD+SPAYTOTS RESTORE ANY REP TYPE          
*                                                                               
PAYEM2A  CLI   KEY+25,X'FF'                                                     
         BE    PAYEM2              BYPASS FF DELETES                            
*                                                                               
         BRAS  RE,TSTDMCB                                                       
*                                                                               
         SR    R4,R4                                                            
         IC    R4,SVCLCLEN         GET LENGTH OF COMPARE                        
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   KEYSAVE(0),KEY      DONE ON CHANGE OF BASE KEY                   
         BNE   PAYEM8                                                           
*                                                                               
         CLI   KEY+3,X'21'         TEST DOING ALL PRODUCTS                      
         BNE   PAYEM2B                                                          
*                                                                               
         CLI   KEY+13,C'*'         SKIP OTHER AGY PRD                           
         BE    PAYEM2                                                           
*                                                                               
PAYEM2B  DS    0H                                                               
*                                                                               
         OC    KEY+21(3),KEY+21                                                 
         BNE   PAYEM2                                                           
*                                                                               
         CLC   KEY+16(3),BSTART    FILTER ON START/END DATES                    
         BL    PAYEM2                                                           
         CLC   KEY+16(3),BEND                                                   
         BH    PAYEM2                                                           
*                                                                               
         OC    BEST,BEST                                                        
         BZ    PAYEM2E                                                          
*                                                                               
         CLC   KEY+19(2),BEST      FILTER ON ESTIMATE                           
         BNE   PAYEM2                                                           
*                                                                               
         CLI   BLIN,0                                                           
         BE    PAYEM2E                                                          
*                                                                               
         CLC   KEY+24(1),BLIN      FILTER ON LINE NUMBER                        
         BNE   PAYEM2                                                           
*                                                                               
PAYEM2E  CLI   PROGPROF+10,C'C'    TEST CCUSA                                   
         BNE   PAYEM3                                                           
*                                                                               
         CLI   KEY+19,0            SEE IF EST NUMBER BELOW 256                  
         BE    PAYEM2F             YES - GO CHK RANGE                           
*                                  FOR EST ABOVE 255                            
         CLI   SVXFRSW,C'N'        SEE IF SUPPRESSING XFR                       
         BNE   PAYEM2              NO - THEN MUST BE OUT OF RANGE               
*                                                                               
         B     PAYEM3              MUST BE IN RANGE                             
*                                                                               
*                                                                               
PAYEM2F  CLC   KEY+19+1(1),SVXFREST   CHK VS LOW EST                            
         BL    PAYEM2                                                           
*                                                                               
         CLC   KEY+19+1(1),SVXFRESX   SEE IF PAST HIGH EST                      
         BH    PAYEM2                                                           
*                                                                               
PAYEM3   EQU   *                                                                
*                                                                               
         MVC   AREC,ABUYIO         POINT TO BUY I/O AREA                        
*                                                                               
         CLI   PARTSW,C'Y'         DOING PART PAYMENT ?                         
         BNE   PAYEM3B             NO                                           
*                                  YES                                          
*   MUST REREAD ONE ONLY BUY AGAIN (BLDST HAS "INTERVENED")                     
*                                                                               
         MVC   KEY+27(4),BYRECADR        REPLACE DA IN KEY                      
         GOTO1 GETREC                    REREAD BUY                             
         BRAS  RE,TSTDMCB                                                       
*                                                                               
PAYEM3A  DS    0H                                                               
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN BUY                     
         BNO   *+12                                                             
         BRAS  RE,BLDPST           UPDATE PST ELEMENT                           
         BNE   EXXMOD              IN CASE OF ERRORS                            
*                                                                               
*                      MODIFY BUY AND "RE-DO" GETINS (PARTPMT)                  
         BRAS  RE,PARTPMT                                                       
*                                                                               
         L     R6,GROSS            GROSS                                        
         L     R7,CSHDSC           CASH DISC                                    
         LR    R8,R6               GROSS                                        
         S     R8,AGYCOM           LESS AGENCY COMMISSION AND                   
         S     R8,CSHDSC             CASH DISC = NET/NET                        
         STM   R6,R8,PAYTOTS       REPLACE PAYABLE GROSS, CD, NET/NET           
*                                                                               
PAYEM3B  GOTO1 GETREC              READ IN RECORD                               
         BRAS  RE,TSTDMCB                                                       
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
*                                                                               
         CLI   PBDBFD,C'T'         SKIP TEST BUYS                               
         BE    PAYEM2                                                           
*                                                                               
         CLI   ADBSW,C'Y'          ADBUYER CALL ?                               
         BNE   PAYEM3B4            NO                                           
*                                  CHECK FOR SERIAL#S                           
         BRAS  RE,ADBTST           SEE IF WE SHOULD SKIP THIS BUY               
         BNE   PAYEM2              YES - SKIP                                   
*                                                                               
PAYEM3B4 DS    0H                                                               
*                                                                               
         CLI   SVREPTYP,C'S'       SEE IF PAYING SPECIAL REP BUYS               
         BNE   PAYEM3D             NO                                           
*                                                                               
         L     R5,ABUYEL           START OF BUY ELEMENTS                        
*                                                                               
         MVI   ELCODE,X'80'        LOOK FOR SPECIAL REP ELEM                    
         BRAS  RE,NEXTEL                                                        
         BNE   PAYEM2              NOT FOUND SKIP THIS BUY                      
*                                                                               
         CLC   SVREP,2(R5)         SEE IF REPS MATCH                            
         BNE   PAYEM2              NO SKIP THIS BUY                             
*                                                                               
         B     PAYEM3F                                                          
*                                                                               
PAYEM3D  DS    0H                  PREP OR NO REP                               
*                                                                               
         L     R5,ABUYEL           START OF BUY ELEMENTS                        
*                                                                               
         MVI   ELCODE,X'80'        STILL LOOK FOR SPECIAL REP ELEM              
         BRAS  RE,NEXTEL                                                        
         BE    PAYEM2              IGNORE SREP BUYS                             
*                                                                               
*        ADDITIONAL CHARGE PREPARATION ******                                   
*        BUILD TABLE OF ADDITIONAL CHARGES                                      
*                                                                               
PAYEM3F  DS    0H                                                               
*                                                                               
         MVI   CHGTYP,0            SET TO NO ADDL.CHG. FOR GETINS CALL          
         XC    ADCTBL(ADCTLEN),ADCTBL   ADDL.CHG.CODES (MAX. 10) TABLE          
         MVI   ADCEND,X'FF'        END OF TABLE                                 
*                                                                               
         LA    RF,ADCTBL                                                        
         ST    RF,ADCADDR          POINTS TO BEGINNING OF ABOVE TABLE           
*                                                                               
         USING PACELEMD,R5         ESTABLISH AS ADDITIONAL CHARGE ELM           
*                                                                               
         L     R5,ABUYEL           START OF BUY ELEMENTS                        
*                                                                               
         MVI   ELCODE,X'44'        ADDITIONAL CHARGE ELEMENT (PACELEM)          
*                                                                               
PAYEM3F2 BRAS  RE,NEXTEL           FIND FIRST/NEXT ADD CHG ELM                  
         BNE   PAYEM3S             DONE                                         
*                                                                               
         MVI   CHGTYP,C'X'     EXCLUDE ADD'L.CHG. IN 1ST GETINS CALL            
         LA    RF,ADCTBL           POINT TO ADDL.CHG. TABLE                     
*                                                                               
PAYEM3F4 CLI   0(RF),X'FF'                                                      
         BE    PAYEM3S             DONE                                         
*                                                                               
         CLI   0(RF),C' '          ANYTHING THERE ?                             
         BH    PAYEM3F6            YES - COMPARE TO PACELEM                     
*                                                                               
         MVC   0(2,RF),PACCODE     ADD THIS CODE TO TABLE                       
*                                                                               
         B     PAYEM3F2            TEST NEXT ADDL.CHG. ELEMENT                  
*                                                                               
PAYEM3F6 CLC   0(2,RF),PACCODE     IS THIS CODE ALREADY IN TABLE ?              
         BE    PAYEM3F2            YES - DO NOT ADD AGAIN                       
*                                                                               
         LA    RF,2(RF)            BUMP TO NEXT TABLE ENTRY                     
         B     PAYEM3F4            GO TEST IT                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
PAYEM3S  DS    0H                                                               
*                                                                               
         MVI   PAIDSW,0            WILL BE SET TO X'01' IF PAY ELEM             
*                                  WITH DATE IS FOUND                           
*                                                                               
         L     R5,ABUYEL           START OF BUY ELEMENTS                        
*                                                                               
         CLI   0(R5),X'20'         IF BUY DESCRIPTION ELEMENT                   
         BNE   *+8                                                              
         BRAS  RE,FIXCD               FIX CD                                    
*                                                                               
         MVI   ELCODE,X'25'        SEARCH FOR PAY ELEMENTS                      
*                                                                               
PAYEM3S5 BRAS  RE,NEXTEL                                                        
         BNE   PAYEM4              NO MORE                                      
*                                                                               
         OC    2(3,R5),2(R5)       IF PAY ELEMENT PAID                          
         BZ    PAYEM4                                                           
*                                                                               
         MVI   PAIDSW,1               SET SWITCH                                
*                                                                               
         B     PAYEM3S5                                                         
*                                                                               
PAYEM4   DS    0H                                                               
*                                                                               
******************>                NOTE: IF BUY HAS ADDN'L. CHARGES             
******************>                PAYEM7D WILL LOOP BACK TO HERE               
******************>                UNTIL ALL CHARGES ARE COMPLETED              
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN BUY                     
         BNO   *+12                                                             
         BRAS  RE,BLDPST           UPDATE PST ELEMENT                           
         BNE   EXXMOD              IN CASE OF ERRORS                            
*                                                                               
         CLI   PARTSW,C'Y'         DOING PART PAYMENT ?                         
         BE    PAYEM4K             YES - DO NOT REDO GETINS                     
*                                                                               
         CLI   CRCKSW,0           SEE IF DOING A CR/CK SWITCH                   
         BE    PAYEM4F            NO                                            
*                                                                               
***********************************************************************         
*                          MODIFY BUY AND "RE-DO" GETINS FOR CR/CK              
         GOTO1 =A(CRCKGET),DMCB,(RC),(R9),RR=RELO02                             
*                                                                               
*        CHECK FOR INVALID CONTROL DATE ERROR                                   
*                                                                               
         CLC   PAYMSG(15),=C'INVALID CONTROL'                                   
         BNE   PAYEM4B                                                          
*                                                                               
         DC    H'0'     THIS SHOULD NEVER HAPPEN                                
*                       THIS ERROR SHOULD ALWAYS                                
*                       BE CAUGHT AT PROC4B                                     
*                                                                               
PAYEM4B  DS    0H                                                               
*                                                                               
***********************************************************************         
         CLI   CRCKFND,C'X'        ANY ELEMENTS FOR CHANGE FOUND ?              
         BE    PAYEM2              NO - SKIP THIS BUY                           
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         LAY   R2,SVBLBDT          POINT TO BILLABLE DATE SAVE AREA             
*                                                                               
         MVC   0(L'SVBLBDT,R2),PBDBDATE  SAVE BILLABLE DATE                     
*                                                                               
         B     PAYEM4K             CONTINUE                                     
*                                                                               
***********************************************************************         
***********************************************************************         
*                                                                               
PAYEM4F  DS    0H                                                               
*                                                                               
*  IF DOING ADDITIONAL CHARGES, CHGTYP IS 'X' FIRST TIME HERE                   
*                                                                               
         CLI   BYTE3,C'N'          IF "DO NOT REWRITE RECORD (OR PAY)"          
         BNE   *+8                                                              
         MVI   CHGTYP,C'F'            SET CHGTYP FOR "NORMAL" GETINS            
*                                     EXCLUDES FX ADDNL CHARGE                  
*                                                                               
*                                  CHGTYP='X' - EXCLUDE ADDN'L CHARGES          
*                                  CHGTYP='A' - PARAM 6 ACTIVE FOR              
*                                               ADDN'L CHARGES                  
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         GOTO1 VGETINS,DMCB,PBUYREC,PVALUES,PBUYKPRD,(CHGTYP,0),=C'PST'X        
               ,ADCADDR                                                         
*                                                                               
         L     RF,16(R1)           ADDRESS OF GST DATA                          
*                                                                               
         LAY   R1,GVALUES          POINT TO GST DATA SAVEAREA                   
         USING GVALUES,R1                                                       
*                                                                               
         MVC   GVALUES(GVALUESL),0(RF) COPY GST DATA                            
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LA    RE,PSTAREA          PST SAVEAREA                                 
         LA    RF,PSTAREA-GVALUES(RF)  PST DATA FROM GETINS                     
*                                                                               
         MVC   0(PSTAREAL,RE),0(RF)  COPY PST DATA                              
         LA    RE,PSTAREAL(RE)    BUMP POINTERS                                 
         LA    RF,PSTAREAL(RF)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         TM    PBUYCNTL,X'80'      TEST DELETED                                 
         BZ    PAYEM4K             NO                                           
*                                                                               
         OC    PGROSS(12),PGROSS   YES - TEST PAID                              
         BNZ   PAYEM4L             YES - KEEP GOING                             
*                                                                               
*                                  UNPAID BUY                                   
*                                                                               
         CLI   CHGTYP,C'A'         IF DOING ADDITIONAL CHARGES                  
         BE    *+8                                                              
         CLI   CHGTYP,C'X'         IF DOING ADDITIONAL CHARGES                  
         BE    PAYEM4L                KEEP GOING                                
*                                                                               
         B     PAYEM2              NO -BYPASS                                   
*                                                                               
PAYEM4L  DS    0H                                                               
*                                                                               
         XC    GROSS(20),GROSS     GROSS = ZERO                                 
*                                                                               
         DROP  R1,R3                                                            
*                                                                               
PAYEM4K  DS    0H                                                               
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         CLI   PAIDSW,X'01'    SEE IF THIS BUY HAS A DATED PAY ELEM             
         BE    PAYEM5          YES THEN SKIP CHECKS                             
*                                                                               
*        CHECK TEARSHEET OPTIONS                                                
*                                                                               
         CLI   PROGPRO2+8,C'Y'     IF BUY MUST HAVE TEARSHEET                   
         BNE   PAYEM4TN                                                         
*                                                                               
         BRAS  RE,PBLOVCK          CHECK FOR OVERRIDES                          
         BE    PAYEM4TN     OVERRIDE EXISTS -SKIP PROFILE CHECK                 
*                                                                               
         TM    PBDSTAT,X'10'          OKAY IF IT HAS TEARSHEET                  
         BO    *+6                                                              
*******************************************************************             
         DC    H'0'                MUST DIE - THIS BUY SHOULD HAVE              
*                                  BEEN FOUND ON PASS 1                         
*******************************************************************             
*                                                                               
PAYEM4TN DS    0H                                                               
*                                                                               
*        CHECK MATCHED/UNMATCHED OPTIONS                                        
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         CLI   PROGPRO2+9,C'O'     PAYING ONLY MATCHED BUYS?                    
         BE    *+8                                                              
         CLI   PROGPRO2+9,C'I'     PAYING ONLY INVOICED BUYS?                   
         BNE   PAYEM4N                                                          
*                                                                               
         BRAS  RE,PBLOVCK          OVERRIDES?                                   
         BE    PAYEM4N             YES - SKIP PROFILE CHECK                     
*                                                                               
         TM    PBDSTAT,X'40'       MATCHED?                                     
         BNO   PAYEM2              NOT MATCHED - SKIP                           
         B     PAYEM5              YES MATCHED - ACCEPT                         
*                                                                               
PAYEM4N  DS    0H                                                               
*                                                                               
         CLI   PROGPRO2+9,C'Y'     ALL BUYS MUST BE MATCHED?                    
         BNE   PAYEM5                                                           
*                                                                               
         BRAS  RE,PBLOVCK          OVERRIDES?                                   
         BE    PAYEM5              YES - SKIP PROFILE CHECK                     
*                                                                               
         TM    PBDSTAT,X'40'       MATCHED?                                     
         BO    *+6                                                              
         DC    H'0'                DUMP IF NOT MATCHED                          
         DROP  R3                                                               
*                                                                               
PAYEM5   DS    0H                                                               
*                                                                               
* MATCH INVOICE#S ATTACHED TO BUY WITH INVOICE#S ON SCREEN                      
*                                                                               
*                                                                               
         CLI   PROGPRO2+9,C'I'     MATCHING TO INVOICE?                         
         JNE   PAYMINVX                                                         
*                                                                               
         BRAS  RE,PBLOVCK          OVERRIDES?                                   
         JE    PAYMINVX            YES - SKIP PROFILE CHECK                     
*                                                                               
         STCM  R5,15,REG5SAV       SAVE REGISTER 5                              
         L     R5,ABUYEL           START OF BUY ELEMENTS                        
*                                                                               
PAYMIV1B CLI   0(R5),0             END OF BUY RECORD?                           
         JE    PAYMINVD                                                         
         XC    WORK(L'PAYINV1),WORK                                             
         CLI   0(R5),PBNVELQ       NEW STYLED INVOICE ELEMENT?                  
         JNE   *+14                                                             
         MVC   WORK(L'PAYINV1),(PBNVINV#-PBNVELM)(R5)                           
         J     PAYMIV1J                                                         
         CLI   0(R5),BYCCIDQ       CUSTOM COLUMN ELEMENT?                       
         JNE   PAYMIV1E                                                         
         CLC   =AL2(8213),(BYCCSQN-BYCCELM)(R5)    STANDARD INV#?               
         JNE   PAYMIV1E                                                         
         MVC   WORK(L'PAYINV1),(BYCCDATA-BYCCELM)(R5)                           
         J     PAYMIV1J                                                         
PAYMIV1E LLC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         J     PAYMIV1B            TRY NEXT INV ELEM IN BUY RECORD              
*                                                                               
PAYMIV1J BRAS  RE,CKINVLST         INV# MATCH TO SCREEN OR XINV#LST?            
         JE    PAYMIV2F                                                         
*                                                                               
         J     PAYMIV1E            TRY NEXT INV# IN BUY RECORD                  
*                                                                               
PAYMIV2F ICM   R5,15,REG5SAV       RESTORE REGISTER 5 ==> PAY ELM               
         J     PAYMINVX            MATCHED TO INVOICE (SCREEN OR LINK)          
*                                                                               
PAYMINVD ICM   R5,15,REG5SAV       RESTORE REGISTER 5 ==> PAY ELM               
         J     PAYEM2              NO INVOICE MATCHED, SKIP BUY RECORD          
*                                                                               
PAYMINVX DS    0H                                                               
*                                                                               
         CLC   PGROSS(12),GROSS    SKIP IF BUY HAS NOT BEEN PAID FULLY          
         BNE   PAYEM5A5                                                         
*                                                                               
         OC    GROSS(20),GROSS     PROCESS FREE BUYS                            
         BNZ   PAYEM7            NOT FREE - MUST HAVE BEEN FULLY PAID           
*                                SO SKIP "CURRENT" ELEMENT                      
***                                                                             
***      SEE IF "FREE" BUY HAS BEEN PAID                                        
PAYEM5A  DS    0H                                                               
*                                                                               
         CLI   PAIDSW,X'01'   SEE IF THIS BUY HAS A DATED PAY ELEM              
         BE    PAYEM7         YES THEN I CAN SKIP "CURRENT" ELEMENT             
*                                                                               
PAYEM5A5 DS    0H                                                               
*                                                                               
*  IF PART-PAYMENT MUST RE-POINT REGISTER 5 TO UNDATED PAYELEM                  
*  THIS IS ALSO NECESSARY FOR HANDLING "ADDITIONAL CHARGES"                     
*                                                                               
         CLI   ADCTBL,C' '         DOING ADDL. CHGS. ?                          
         BH    PAYEM5AC            YES - DO SAME AS PART PMT.                   
*                                                                               
         CLI   PARTSW,C'Y'         DOING PARTIAL PAYMENT ?                      
         BNE   PAYEM5AD            NO                                           
*                                                                               
PAYEM5AC DS    0H                                                               
*                                                                               
         L     R5,ABUYEL           START OF BUY ELEMENTS                        
*                                                                               
         MVI   ELCODE,X'25'        SEARCH FOR PAY ELEMENTS                      
*                                                                               
PAYLUPF  BRAS  RE,NEXTEL           POINT R5 TO UNDATED PAYELEM                  
         BNE   PAYEM5AD                 (IF FOUND)                              
*                                                                               
         USING PPAYELD,R5          ESTABLISH PAY ELEMENT                        
*                                                                               
         OC    PPDDATE,PPDDATE     SKIP IF ELM HAS PAY DATE                     
         BNZ   PAYLUPF                                                          
*                                                                               
PAYEM5AD DS    0H                                                               
*                                                                               
         CLI   CRCKSW,0       SEE IF JUST SWITCHING CR/CK                       
         BE    PAYEM5B        NO THEN DO WHAT I USED TO DO                      
*                                                                               
***********************************************************************         
*                             FIND THE RIGHT PAY ELEM AND SWITCH                
         MVI   CRCKFND,C' '        CLEAR ELEMENT FOUND INDICATOR                
*                                                                               
         L     R5,ABUYEL           START OF BUY ELEMENTS                        
*                                                                               
         MVI   ELCODE,X'25'        SEARCH FOR PAY ELEMENTS                      
*                                                                               
PAYEM5A2 BRAS  RE,NEXTEL                                                        
         BNE   PAYEM5A7            DONE                                         
*                                                                               
         USING PPAYELD,R5          ESTABLISH PAY ELEMENT                        
*                                                                               
         CLC   PPDDATE,CRCKDAT     CHECK FOR CORRECT DATE                       
         BNE   PAYEM5A2                                                         
*                                                                               
         CLC   PPDSEQNO,CRCKSEQ    CHECK FOR CORRECT CHECK SEQUENCE             
         BNE   PAYEM5A2                                                         
*                                                                               
         MVC   WORK(1),PPREP       COPY CR OR CK INDICATOR                      
         NI    WORK,X'C0'          ONLY LEAVES X'80' OR X'40' ON                
*                                                                               
         CLC   WORK(1),CRCKSW    SEE IF IT MATCHES TYPE I'M SWITCHING           
         BNE   PAYEM5A2          WRONG TYPE - SKIP THIS ELEMENT                 
*                                                                               
         MVI   CRCKFND,C'Y'        SET ELEMENT FOUND INDICATOR                  
*                                                                               
         CLI   CRCKSW,X'40'      SEE IF SWITCHING CR TO CK                      
         BNE   PAYEM5A4                                                         
*                                                                               
         NI    PPREP,X'3F'       SET OFF X'80' OR X'40'                         
         OI    PPREP,X'80'                                                      
*                                                                               
         B     PAYEM5A2          LOOK FOR ANOTHER ELEMENT                       
*                                                                               
*        MUST BE SWITCHING CK TO CR                                             
*                                                                               
PAYEM5A4 NI    PPREP,X'3F'       SET OFF X'80' OR X'40'                         
         OI    PPREP,X'40'                                                      
         B     PAYEM5A2          LOOK FOR ANOTHER ELEMENT                       
*                                                                               
PAYEM5A7 DS    0H                                                               
*                                                                               
         CLI   CRCKFND,C'Y'        ELEMENT(S) FOUND ?                           
         BNE   PAYEM2              NO - SKIP BUY                                
*                                                                               
         B     NOPE                GO WRITE BACK RECORD                         
*                                                                               
***********************************************************************         
*                                                                               
*        PAY OF REGULAR BUY - NO CRCK SWITCH                                    
*                                                                               
PAYEM5B  DS    0H                                                               
*                                                                               
         CLI   BYTE3,C'N'          NOT REALLY PAYING ?                          
         BE    PAYEM5C             YES - SKIP MULTI-REQHDR WORK                 
*                                                                               
         CLI   MLTREQSW,C'Y'       OUTPUT ONE REQHDR PER BUY ?                  
         BNE   PAYEM5C             NO                                           
*                                                                               
         L     R3,ABUYIO           POINT TO BUYREC                              
         USING PBUYREC,R3                                                       
*                                                                               
         LAY   R2,SVBLBDT          POINT TO BILLABLE DATE SAVE AREA             
*                                                                               
         MVC   0(L'SVBLBDT,R2),PBDBDATE  SAVE BILLABLE DATE                     
*                                                                               
         CLI   MRQSTAT,C'Y'        CLRSTAT ALREADY DONE ?                       
         BE    PAYEM5C             YES                                          
*                                                                               
*  ******  DO GETINS AGAIN FOR RECALC OF SINGLE BUY PAYTOTS  ******             
*                                                                               
         XC    GROSS(64),GROSS     CLEAR GETINS OUTPUT                          
         XC    PAYTOTS(PAYTOTSL),PAYTOTS    CLEAR FOR SINGLE BUY CALC'S         
         MVC   BAMTS(BAMTL),SBAMTS TO RETAIN THE 4 BYTES AFTER INVOICE          
*                                  AMOUNT WHICH WILL BE REPLACED LATER          
*                                                                               
         MVC   PAYREP,PREP-PAYTOTSD+SPAYTOTS      RESTORE ANY REP CODE          
         MVC   PAYREPTP,PREPTYP-PAYTOTSD+SPAYTOTS RESTORE ANY REP TYPE          
*                                                                               
         GOTOR VGETINS,DMCB,PBUYREC,PVALUES,PBUYKPRD,0                          
*                                                                               
         TM    PBUYCNTL,X'80'      TEST DELETED                                 
         BNO   *+10                NO                                           
         XC    GROSS(20),GROSS     GROSS = ZERO                                 
*                                                                               
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         SR    R8,R8                                                            
*                                                                               
         A     R6,GROSS                                                         
         S     R6,PGROSS                                                        
         A     R7,CSHDSC                                                        
         S     R7,PCSHDSC                                                       
         A     R8,PYABLE           GROSS-COMM-CSH DSC                           
         S     R8,PAID                                                          
*                                                                               
         STM   R6,R8,PAYTOTS                                                    
*                                                                               
         AR    R8,R7               NET                                          
         ST    R8,BTOT                                                          
*                                                                               
         CLI   GNOPT,C'G'          IF PAYING GROSS                              
         BNE   *+8                                                              
         ST    R6,BTOT                USE GROSS AMOUNT                          
*                                                                               
         MVC   BAMTS(L'BTOT),BTOT                                               
         MVC   NPAYTOTS(NPAYTOTL),PAYTOTS     SAVE FOR CHECK REQUEST            
*                                                                               
         MVC   SVBUYKEY,KEY        SAVE TO RESTORE SEQUENCE LATER               
*                                                                               
*        REPLACE REQUEST VALUES WITH CURRENT BUY'S VALUES                       
*                                                                               
         XC    SAVPR(SAVFLDL),SAVPR                                             
         MVC   SAVPR,PBUYKPRD                                                   
         MVC   BSTART,PBUYKDAT                                                  
         MVC   BEND,PBUYKDAT                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(3,BSTART),WKSTART                                  
*                                                                               
         MVC   WKEND,WKSTART                                                    
         MVC   BPUB,PBUYKPUB                                                    
         MVC   BEST,PBUYKEST                                                    
*                                                                               
         CLI   PBUYKLIN,1                                                       
         BNH   *+10                                                             
         MVC   BLIN,PBUYKLIN                                                    
*                                                                               
         GOTOR BLDST,DMCB,PAYTOTS                                               
         JNE   ERRORXT1                                                         
*                                  UPDATE STATUS RECORD                         
*                                  AND GET SEQUENCE NUMBER                      
         DROP  R3                                                               
*                                                                               
         MVI   MRQSTAT,C'Y'        SET TO "CLRSTAT ALREADY DONE" STATUS         
*                                                                               
         MVC   KEY,SVBUYKEY        RESTORE BUY KEY                              
         BRAS  RE,HIGH             AND SEQUENCE                                 
         BRAS  RE,TSTDMCB                                                       
         MVC   AREC,ABUYIO         POINT TO BUY I/O AREA                        
*                                  YES                                          
*   MUST REREAD BUY AGAIN (BLDST HAS "INTERVENED")                              
*                                                                               
         GOTO1 GETREC                    REREAD BUY                             
         BRAS  RE,TSTDMCB                                                       
*                                                                               
         CLI   CHGTYP,C'X'         "EXCLUDE ADD'L CHG" GETINS NEEDED ?          
         BE    PAYEM4F             YES - GO REDO GETINS FOR UPDATE              
*                                    OF "NON-ADD'L CHG" PAY ELEM                
*                                                                               
PAYEM5C  DS    0H                                                               
*                                                                               
         LA    R4,PAYTOTS          POINT TO MAIN TOTALS                         
*                                                                               
         CLI   CHGTYP,C'A'         IF DOING AN ADDITIONAL CHARGE                
         BNE   PAYEM5C1                                                         
*                                                                               
         L     RF,ADCADDR             ADDRESS OF ADDL.CHG. CODE                 
         CLC   =C'FX',0(RF)           AND THIS IS FX ADDITIONAL CHARGE          
         BNE   *+10                                                             
         LAY   R4,XPAYTOTS               POINT TO FX TOTALS                     
*                                                                               
PAYEM5C1 DS    0H                                                               
*                                                                               
         USING PAYTOTSD,R4         ESTABLISH TOTALS AREA                        
*                                                                               
         CLI   0(R5),X'25'         SKIP IF NOT A PAY ELEMENT                    
         BNE   PAYEM5G                                                          
*                                                                               
         TM    PPDSTAT-PPAYELEM(R5),X'01' DELETE IF IT HAD SEQ 2                
         BO    PAYEM5E                                                          
*                                                                               
         CLI   1(R5),PPDSTAT-PPAYELEM       "OLD" SHORT ELEM?                   
         BE    PAYEM5E                       YES - DELETE                       
*                                                                               
         LHI   RF,PPACCODE-PPAYELEM NORMAL LENGTH                               
*                                                                               
         OC    PSTATSQ2,PSTATSQ2   IF SECONDARY SEQ # PRESENT                   
         BNZ   PAYEM5E              DELETE - TOO COMPLICATED OTHERWISE          
*                                                                               
         CLM   RF,1,1(R5)          SKIP IF ADDL.CHG. ELEM                       
         BNE   PAYEM5D                                                          
*                                                                               
         CLI   CHGTYP,C'A'                   DOING ADDL.CHG. ?                  
         BE    PAYEM5E                       YES - TOO SHORT - DELETE           
*                                                                               
         B     PAYEM6                        NO - OK - FILL IN ELEM             
*                                                                               
PAYEM5D  DS    0H                                                               
*                                                                               
         CLI   CHGTYP,C'A'                   DOING ADDL.CHG. ?                  
         BE    PAYEM6                        YES - FILL IN ELEM                 
*                                                                               
PAYEM5E  DS    0H                  ***** DELETE EXISTING NULL ELEMENT           
*                                                                               
         GOTO1 VRECUP,DMCB,(1,ABUYIO),(R5),0                                    
*                                                                               
PAYEM5G  DS    0H                                                               
*                                                                               
         XC    WORK,WORK           INIT NEW ELEMENT                             
*                                                                               
         MVC   WORK(2),=X'2518'    SET CODE AND BASE LENGTH                     
*                                                                               
         OC    PSTATSQ2,PSTATSQ2   IF SECONDARY SEQ PRESENT                     
         BZ    *+8                                                              
         MVI   WORK+1,X'1A'           RESET BASE LENGTH                         
*                                                                               
         CLI   CHGTYP,C'A'         DOING ADDL.CHG. ?                            
         BNE   PAYEM5P             NO - SHORTER ELEMENT IS OK                   
*                                                                               
         L     R6,GROSS            TEST GROSS                                   
         S     R6,PGROSS                                                        
         BNZ   PAYEM5J             NOT ZERO - ADD ELEMENT                       
*                                                                               
         L     R6,PYABLE           TEST ANY PAID                                
         S     R6,PAID                                                          
         BZ    PAYEM6B             PAID AND GROSS ARE BOTH ZERO                 
*                                    SO DO NOT ADD ELEMENT                      
PAYEM5J  DS    0H                                                               
*                                                                               
         LLC   RF,WORK+1           BUMP LENGTH BY 2                             
         AHI   RF,2                                                             
         STC   RF,WORK+1                                                        
*                                                                               
PAYEM5P  DS    0H                  ADD NEW ELEMENT TO RECORD                    
*                                                                               
         GOTO1 VRECUP,DMCB,(1,ABUYIO),WORK,(R5)                                 
*                                                                               
         USING PPAYELD,R5          ESTABLISH NEW PAY ELEMENT                    
*                                                                               
PAYEM6   DS    0H                  BUILD NEW PAY ELEMENT                        
*                                                                               
         MVC   PPDDATE(3),BTODAY   PAID TODAY                                   
         MVC   PPDCKDAT,CKDATE     SET CHECK CONTROL DATE                       
*                                                                               
         LM    R6,R8,GROSS                                                      
         S     R6,PGROSS                                                        
         S     R7,PAGYCOM                                                       
*                                                                               
         CLI   NCDSW,C'N'          SKIP IF PAID WITH CD                         
         BE    *+8                                                              
         S     R8,PCSHDSC             SUBTRACT CASH DISCOUNT                    
*                                                                               
         STM   R6,R8,GROSS                                                      
*                                                                               
         MVC   PPGROSS(12),GROSS   SET GROSS AMOUNT                             
*                                                                               
         CLC   PAYMD+1(4),=C'AUTO'                                              
         JNE   *+8                                                              
         OI    PPDSTAT,X'08'       SCRIPT PAY UPLOAD                            
*                                                                               
         CLC   =C'AUTP',PAYMD+1                                                 
         JNE   *+8                                                              
         OI    PPDSTAT,X'04'       SCRIPT AUTOPAY UPLOAD                        
*                                                                               
         CLI   NCDSW,C'L'         SEE IF PAID  - LOST CD                        
         BNE   *+8                                                              
         OI    PPDSTAT,X'40'                                                    
*                                                                               
         CLI   BPUB+4,X'FF'        SEE IF PAID ACROSS ZONES/EDTS                
         BNE   *+8                                                              
         OI    PPDSTAT,X'20'       SET ON BIT IN PPDSTAT                        
*                                                                               
         MVC   PPDSEQNO,PSTATSQ    PUT CLEARANCE STATUS SEQUENCE NO.            
*                                  INTO PAYELEM                                 
*                                                                               
         OC    PSTATSQ2,PSTATSQ2   IF SECONDARY SEQ PRESENT                     
         BZ    PAYEM6A1                                                         
*                                                                               
         OI    PPDSTAT,X'01'          SET INDICATOR                             
*                                                                               
         LLC   R1,PPAYELEM+1          GET ELEMENT LENGTH                        
         SHI   R1,2                   DECREMENT BY 2                            
         LA    R1,PPAYELEM(R1)        POINT TO SPOT FOR SECONDARY SQN           
         MVC   0(2,R1),PSTATSQ2       SET SECONDARY SEQ NO.                     
*                                                                               
PAYEM6A1 DS    0H                                                               
*                                                                               
         CLI   CHGTYP,C'A'         DOING ADDL.CHG. ?                            
         BNE   PAYEM6A             NO                                           
*                                                                               
         L     RF,ADCADDR          ADDRESS OF ADDL.CHG. CODE                    
         MVC   PPACCODE,0(RF)      PUT THIS CODE INTO RECORD                    
*                                                                               
PAYEM6A  DS    0H                                                               
*                                                                               
         OC    PREP,PREP           IF PAYING A REP                              
         BZ    PAYEM6B                                                          
*                                                                               
         PACK  DUB,PREP               SET REP CODE IN ELEMENT                   
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   PPREP,HALF                                                       
*                                                                               
PAYEM6B  DS    0H                                                               
*                                                                               
         LA    R7,PBAMTS           TEST FOR CR OR CK                            
         LA    R6,PBMXIV#Q                                                      
*                                                                               
PAYEM6D  DS    0H                                                               
*                                                                               
         USING BAMTD,R7            ESTABLISH BAMT FIELDS                        
*                                                                               
         CLI   BAMTTYPE,C'2'       CR                                           
         BNE   *+8                                                              
         OI    PPREP,X'40'                                                      
*                                                                               
         CLI   BAMTTYPE,C'3'       CK                                           
         BNE   *+8                                                              
         OI    PPREP,X'80'                                                      
*                                                                               
         LA    R7,BAMTL(R7)        NEXT SET OF BAMT FIELDS                      
*                                                                               
         BCT   R6,PAYEM6D                                                       
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         CLI   PBUYKPRD,C'*'       FINAL CHECK FOR OTHER AGY PRD                
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* MARK PAID WITH GST IF CANADIAN AND THERE IS GST                  L01          
*                                                                               
         TM    PBDCNDA,X'80'                                       L01          
         BNO   PAYEM7                                              L01          
*                                                                               
PAYEM6F  DS    0H                                                               
*                                                                               
         OC    SVPAYGST,SVPAYGST                                                
         BZ    NOPE                                              BUG02          
*                                                                               
         CLI   SVPAYGST,255                                                     
         BE    NOPE                                              BUG02          
*                                                                               
         OI    PBDCNDA,X'01'       PAID WITH GST                   L01          
*                                                                               
NOPE     DS    0H                                                  L01          
*                                                                               
*        MARK PAID WITH PST IF ANY PROVINCE COLLECTED PST                       
*                                                                               
         LA    R0,10               10 PROVINCES                                 
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
*                                                                               
PAYEM6L  DS    0H                                                               
*                                                                               
         OC    PROV,PROV           SKIP IF NO PROVINCE                          
         BZ    PAYEM6C                                                          
*                                                                               
         OC    PAYPSTAX,PAYPSTAX   SKIP IF NO TAX FOR PROVINCE                  
         BZ    PAYEM6C                                                          
*                                                                               
         CLI   PAYPSTAX,255                                                     
         BE    PAYEM6C                                                          
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
*                                                                               
         OI    PBDCNDA,X'02'       PAID WITH PST                   L01          
*                                                                               
PAYEM6C  DS    0H                                                               
*                                                                               
         LA    RF,PSTBCKSL(RF)     FOR ALL PROVINCES TOGETHER                   
         BCT   R0,PAYEM6L                                                       
*                                                                               
         DROP  RF,R3                                                            
*                                                                               
PAYEM7   DS    0H                                                               
*                                                                               
         CLI   BYTE3,C'N'          DO NOT REWRITE RECORD                        
         BE    PAYEM2                                                           
*                                                                               
         CLI   CRCKSW,0            SEE IF SWITCHING CR/CK                       
         BNE   PAYEM7K             YES - GO REWRITE RECORD                      
*                                                                               
         CLI   CHGTYP,C'A'         DOING ADDL.CHG'S NOW ?                       
         BE    PAYEM7H             YES                                          
*                                                                               
         CLI   CHGTYP,C'X'         DOING "EXCLUDE" ADDL.CHG'S NOW ?             
         BNE   PAYEM7K             NO - NOT "A" AND NOT "X" - DONE              
*                                                                               
         CLI   ADCTBL,C' '         "X" GETINS DONE - ANY ADDL.CHG. ?            
         BNH   PAYEM7K             NO - DONE - GO REWRITE RECORD                
*                                  PREP. FOR ADDITIONAL CHARGE GETINS           
         MVI   CHGTYP,C'A'         SET FOR GETINS                               
         LA    RF,ADCTBL           POINT TO UNIQUE ADD'L CODES TABLE            
*                                                                               
PAYEM7D  ST    RF,ADCADDR          SAVE THIS ADDRESS                            
*                                                                               
         B     PAYEM4              BACK TO "BEFORE" GETINS                      
*                                                                               
PAYEM7H  DS    0H                                                               
*                                                                               
         L     RF,ADCADDR                                                       
         LA    RF,2(RF)            BUMP TO NEXT CHARGE CODE                     
*                                                                               
         CLI   0(RF),X'FF'         END OF TABLE ?                               
         BE    PAYEM7K             YES - DONE - GO WRITE RECORD                 
*                                                                               
         CLI   0(RF),C' '          ANY MORE ADDL.CHG. ?                         
         BH    PAYEM7D             YES - GO "BACK" TO PROCESS                   
***********************************************************************         
*                                                                               
PAYEM7K  DS    0H                                                               
*                                                                               
         CLI   BYTE3,C'N'          NOT REALLY PAYING ?                          
         BE    PAYEM7V             YES - SKIP MULTI-REQHDR WORK                 
*                                                                               
PAYEM7L  DS    0H                                                               
*                                                                               
         MVC   AREC,ABUYIO         POINT TO BUY I/O AREA                        
         GOTO1 PUTREC                                                           
         BRAS  RE,TSTDMCB                                                       
*                                                                               
         BRAS  RE,BLDMQ            ADD TO MQ MESSAGE                            
*                                                                               
         CLI   MLTREQSW,C'Y'       OUTPUT ONE REQHDR PER BUY ?                  
         BNE   PAYEM7V             NO - SKIP MULTI-REQHDR WORK                  
*                                                                               
         CLI   MRQSTAT,C'Y'        CLRSTAT DONE ?                               
         BNE   PAYEM7V             NO - SKIP THIS BUY - CHECK REQ               
*                                    WITHOUT CLR STAT REC ILLOGICAL             
         MVC   SVBUYKEY,KEY        SAVE TO RESTORE SEQUENCE LATER               
*                                                                               
*          REPLACE PAYTOTS FOR THE CHECK REQUEST WITH THE                       
*            VALUES OBTAINED FROM GETINS IN PAYEM5B                             
*                                                                               
******   XC    PAYTOTS(PAYTOTSL),PAYTOTS                                        
******   MVC   PAYTOTS(NPAYTOTL),NPAYTOTS     FOR CHECK REQUEST                 
*                                                                               
         CLI   SCRCOMSW,C'Y'       SCREEN COMMENTS ENTERED ?                    
         BE    PAYEM7S             YES - USE THEM                               
*                                                                               
*                     STORE SPECIAL REMITTANCE COMMENTS (SRC) IF FOUND          
*                                                                               
         LAY   R2,REQHDR           ESTABLISH REQ HDR "AREA"                     
         USING REQHDR,R2                                                        
*                                                                               
         LA    R3,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
         LA    R1,SRCOMM1                                                       
         MVC   0(L'SRCOMM1,R1),BLANKS                                           
         AHI   R1,L'SRCOMM1                                                     
         JCT   R3,*-10                                                          
         MVI   QCOM,0              RESET COMMENT COUNTERS                       
*                                                                               
         LA    R1,SRCOMM1          POINT TO 1ST "SRC" COMMENT                   
         LA    R3,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
         L     R5,ABUYEL           POINT TO START OF BUY ELEM'S                 
         MVI   ELCODE,X'6A'        "SRC" COMMENT ELEM CODE                      
*                                                                               
PAYEM7N  BRAS  RE,NEXTEL                                                        
         BNE   PAYEM7R             NO MORE COMMENTS - ISSUE REQUEST             
         ZIC   RF,1(R5)            ELEM LENGTH                                  
         SHI   RF,3                PREP FOR EX MOVE                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),2(R5)       MOVE COMMENT FROM ELEM                       
         LA    R1,L'SRCOMM1(R1)    NEXT COMMENT OUTPUT AREA                     
         BCT   R3,PAYEM7N                                                       
         DROP  R2                                                               
*                                                                               
PAYEM7R  DS    0H                                                               
*                                                                               
         LA    R2,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
         SR    R2,R3               ACTUAL NUMBER OF SRC COMMENTS                
         STC   R2,BAMTS+6          BAMTCOM#                                     
*                                                                               
PAYEM7S  DS    0H                                                               
*                                                                               
         GOTOR CHCKREQ,DMCB,PAYTOTS  ISSUE MAIN CHECK REQUEST                   
*                                                                               
         GOTOR ADDCD,DMCB,PAYTOTS    ADD CD TO CLRST RECORD                     
*                                                                               
         TM    FXSW,X'20'          IF THERE IS AN FX PAYMENT                    
         BNO   PAYEM7T                                                          
*                                                                               
         OI    FXSW,X'08'          INDICATE DOING FX CHECK REQUEST              
         LAY   RF,XPAYTOTS                                                      
*                                                                               
         GOTOR CHCKREQ,DMCB,0(RF)  ISSUE FX CHECK REQUEST                       
*                                                                               
         LAY   RF,XPAYTOTS                                                      
         GOTOR ADDCD,DMCB,0(RF)      ADD CD TO CLRST RECORD                     
*                                                                               
         NI    FXSW,X'FF'-X'08'    TURN OFF INDICATOR                           
*                                                                               
PAYEM7T  DS    0H                                                               
*                                                                               
         MVC   KEY,SVBUYKEY        RESTORE BUY KEY                              
         BRAS  RE,HIGH             AND SEQUENCE                                 
         BRAS  RE,TSTDMCB                                                       
*                                                                               
         MVC   SAVPR(SAVFLDL),SAVPRX   RESTORE REQ SCREEN VALUES                
*                                                                               
         MVI   MRQSTAT,C' '        RESET TO "CLRSTAT NOT DONE" STATUS           
*                                                                               
         B     PAYEM2              NEXT RECORD                                  
*                                                                               
PAYEM7V  DS    0H                                                               
*                                                                               
         CLI   PARTSW,C'Y'         PART PMT BEING DONE ?                        
         BNE   PAYEM2              NO - NEXT RECORD                             
*                                                                               
PAYEM8   CLI   BYTE3,C'N'          SEE IF NOT REALLY PAYING                     
         BE    NOPAY1              GO DO MESSAGES                               
*                                                                               
         CLI   MLTREQSW,C'Y'       OUTPUT ONE REQHDR PER BUY ?                  
         BE    PAYEM45             YES - FINISH                                 
*                                                                               
         CLI   PROGPROF+10,C'C'    CHK FOR CCUSA                                
         BNE   PAYEM9                                                           
*                                                                               
         GOTO1 =A(PAYCC),DMCB,(RC),RR=RELO02                                    
*                                                                               
PAYEM9   DS    0H                                                               
*                                                                               
         GOTOR CHCKREQ,DMCB,PAYTOTS                                             
*                                                                               
         GOTOR ADDCD,DMCB,PAYTOTS  ADD CD TO CLRST RECORD                       
*                                                                               
         TM    FXSW,X'20'          IF THERE IS AN FX PAYMENT                    
         BNO   PAYEM9T                                                          
*                                                                               
         OI    FXSW,X'08'          INDICATE DOING FX CHECK REQUEST              
         LAY   RF,XPAYTOTS                                                      
*                                                                               
         GOTOR CHCKREQ,DMCB,0(RF)  ISSUE FX CHECK REQUEST                       
*                                                                               
         LAY   RF,XPAYTOTS                                                      
         GOTOR ADDCD,DMCB,0(RF)      ADD CD TO CLRST RECORD                     
*                                                                               
         NI    FXSW,X'FF'-X'08'    TURN OFF INDICATOR                           
*                                                                               
PAYEM9T  DS    0H                                                               
*                                                                               
* PUT OUT CHECK TOTAL AMOUNTS                                                   
*                                                                               
         CLI   CRCKSW,0        SEE IF SWITCHING CR/CK                           
         BE    PAYEM45         NO                                               
*                                                                               
*        SWITCH SIGNS OF BAMTS AND TOTALS AND CR/CK THEN DO THE OTHER           
*        CHCKREQ                                                                
*        NOTE DON'T NEED TO GO TO PAYCC SINCE NOTHING IS TRANSFERRED            
*                                                                               
         LM    R6,R8,PAYTOTS   (TOTG,TOTCD,TOTNLCD)                             
         LCR   R6,R6                                                            
         LCR   R7,R7                                                            
         LCR   R8,R8                                                            
         STM   R6,R8,PAYTOTS                                                    
*                                                                               
         L     R6,BTOT                                                          
         LCR   R6,R6                                                            
         ST    R6,BTOT                                                          
*                                                                               
         L     R6,TOTGSTP                                                       
         LCR   R6,R6                                                            
         ST    R6,TOTGSTP                                                       
*                                                                               
         LA    R6,GSTAMTS                                                       
         LA    R7,PBMXIV#Q         MAX NUMBER OF SUPPORTED INVOICES             
*                                                                               
PAYEM20  L     R0,0(R6)                                                         
         LCR   R0,R0                                                            
         ST    R0,0(R6)                                                         
         LA    R6,4(R6)                                                         
         BCT   R7,PAYEM20                                                       
*                                                                               
*        ALLOCATE PST TO INVOICES                                               
*                                                                               
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
         LA    R5,10               10 PROVINCES                                 
*                                                                               
PPSTPSTL DS    0H                                                               
*                                                                               
         L     R0,TOTPSTP          CHANGE SIGN OF TOTAL PROV PST AMOUNT         
         LCR   R0,R0                                                            
         ST    R0,TOTPSTP                                                       
*                                                                               
         L     R0,TOT$BSP          CHANGE SIGN OF TOTAL PROV PST BASIS          
         LCR   R0,R0                                                            
         ST    R0,TOT$BSP                                                       
*                                                                               
         LA    R7,PSTAMTS          POINT TO PST BY INVOICE BUCKETS              
         LA    R8,PST$BSS          POINT TO PST BASIS BY INV BUCKETS            
         LA    R4,PBMXIV#Q         MAX NUMBER OF SUPPORTED INVOICES             
*                                                                               
PPSTAMTL DS    0H                                                               
*                                                                               
         ICM   R0,15,0(R7)         CHANGE SIGN OF PST AMOUNT                    
         BZ    PPSTAMT1                                                         
*                                                                               
         LCR   R0,R0                                                            
         ST    R0,0(R7)                                                         
*                                                                               
PPSTAMT1 DS    0H                                                               
*                                                                               
         ICM   R0,15,0(R8)         CHANGE SIGN OF PST AMOUNT                    
         BZ    PPSTAMTC                                                         
*                                                                               
         LCR   R0,R0                                                            
         ST    R0,0(R8)                                                         
*                                                                               
PPSTAMTC DS    0H                                                               
*                                                                               
         LA    R7,4(R7)            NEXT INVOICE PST                             
         LA    R8,4(R8)            NEXT INVOICE PST                             
*                                                                               
         BCT   R4,PPSTAMTL                                                      
*                                                                               
PPSTAMTD DS    0H                                                               
*                                                                               
PPSTPSTC DS    0H                                                               
*                                                                               
         LA    RF,PSTBCKSL(RF)     NEXT PROVINCE                                
         BCT   R5,PPSTPSTL                                                      
*                                                                               
PPSTPSTD DS    0H                                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R6,BAMTS                                                         
         LA    R7,PBMXIV#Q         MAX NUMBER OF SUPPORTED INVOICES             
*                                                                               
PAYEM25  L     R0,0(R6)                                                         
         LCR   R0,R0                                                            
         ST    R0,0(R6)                                                         
         CLI   4(R6),C'2'          SEE IF WAS CR                                
         BNE   PAYEM30                                                          
         MVI   4(R6),C'3'          SWITCH TO CK                                 
         B     PAYEM35                                                          
*                                                                               
PAYEM30  CLI   4(R6),C'3'          SEE IF WAS CK                                
         BNE   PAYEM35                                                          
         MVI   4(R6),C'2'          SWITCH TO CR                                 
*                                                                               
PAYEM35  LA    R6,8(R6)                                                         
         BCT   R7,PAYEM25                                                       
*                                                                               
PAYEM40  DS    0H                                                               
*                                                                               
         GOTOR CHCKREQ,DMCB,PAYTOTS                                             
*                                                                               
         GOTOR ADDCD,DMCB,PAYTOTS  ADD CD TO CLRST RECORD                       
*                                                                               
         XC    PAYMSG,PAYMSG                                                    
*                                                                               
         MVC   PAYMSG(16),=C'CR CHANGED TO CK'                                  
*                                                                               
         CLI   CRCKSW,X'40'                                                     
         BE    *+10                                                             
         MVC   PAYMSG(16),=C'CK CHANGED TO CR'                                  
*                                                                               
         FOUT  PAYMSGH                                                          
*                                 CLOBBER "REV"                                 
*                                 TO PREVENT ACCIDENTAL REVERSALS               
         MVC   PAYOP(3),=C'***'                                                 
         FOUT  PAYOPH                                                           
*                                 TO PREVENT ACCIDENTAL REVERSALS               
         TM    FXSW,X'80'          SKIP IF AGENCY NOT USING FX FEATURE          
         BNO   PAYEM44                                                          
*                                                                               
         XC    PAYMSG1,PAYMSG1                                                  
         FOUT  PAYMSG1H                                                         
*                                                                               
PAYEM44  DS    0H                                                               
*                                                                               
         B     PAYEM50                                                          
*                                                                               
PAYEM45  DS    0H                                                               
*                                                                               
         CLI   MLTREQSW,C'Y'       OUTPUT ONE REQHDR PER BUY ?                  
         BNE   PAYEM45K            NO                                           
         MVC   PAYTOTS(PAYTOTSL),SPAYTOTS       RESTORE PAYTOTS FROM            
*                                               PROC... ACCUMULATIONS           
PAYEM45K DS    0H                                                               
         FOUT  PAYMSGH,=C'AMOUNTS AGREE. CHECK TOTALS N='                       
*                                                                               
         CLC   PAYMD+1(4),=C'AUTO' SCRIPT PAY UPLOAD?                           
         JE    PAYEM45X                                                         
         CLC   PAYMD+1(4),=C'AUTP' SCRIPT AUTOPAY UPLOAD?                       
         JE    PAYEM45X                                                         
*                                                                               
         CLI   PARTSW,C'Y'                                                      
         BNE   *+10                                                             
         MVC   PAYMSG(14),=C'PART PAYMENT. '                                    
         LA    R4,PAYMSG+30                                                     
         LM    R6,R8,PAYTOTS                                                    
         AR    R8,R7                                                            
         A     R8,TOTGSTP          ADD IN GST                      L01          
*                                                                               
*        ADD IN PST                                                             
*                                                                               
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
         LA    R5,10               10 PROVINCES                                 
*                                                                               
         A     R8,TOTPSTP          ADD PST FOR PROVINCE                         
         LA    RF,PSTBCKSL(RF)    NEXT PROVINCE                                 
         BCT   R5,*-8                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         EDIT  (R8),(12,0(R4)),2,ALIGN=LEFT,MINUS=YES                           
         CLI   ADBPART,C'Y'        DOING ADBUYER PART PMT. ?                    
         BNE   PAYEM45N            NO                                           
*                                                                               
         ST    R8,FULL             YES                                          
         L     RE,ADBWRKA          ESTABLISH ADBUYER WORK AREA                  
         USING ADBWRKD,RE                                                       
         LA    RF,ADBYTBL          POINT TO START OF "PAID BUYS" TABLE          
         USING ADBYRPLY,RF                                                      
         MVC   ABRPAMT,FULL        SAVE PART AMOUNT FOR ADB REPLY               
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
PAYEM45N DS    0H                                                               
*                                                                               
         CLI   NCDSW,C'N'                                                       
         BE    PAYEM45X                                                         
*                                                                               
         AR    R4,R0                                                            
         MVC   3(3,R4),=C'CD='                                                  
         EDIT  (R7),(12,6(R4)),2,ALIGN=LEFT,MINUS=YES                           
*                                                                               
PAYEM45X DS    0H                                                               
*                                                                               
         TM    FXSW,X'80'          SKIP IF AGENCY NOT USING FX FEATURE          
         BNO   PAYEM50                                                          
*                                                                               
         XC    PAYMSG1,PAYMSG1                                                  
         FOUT  PAYMSG1H                                                         
*                                                                               
         TM    FXSW,X'40'          SKIP IF NOT AN AMERICAN PUB                  
         BNO   PAYEM50                                                          
*                                                                               
         TM    FXSW,X'20'          SKIP IF NO FX AMOUNT IN PAYMENT              
         BNO   PAYEM50                                                          
*                                                                               
         CLC   PAYMD+1(4),=C'AUTO' SCRIPT PAY UPLOAD?                           
         JE    PAYEM50                                                          
         CLC   PAYMD+1(4),=C'AUTP' SCRIPT AUTOPAY UPLOAD?                       
         JE    PAYEM50                                                          
*                                                                               
         XC    PAYMSG,PAYMSG                                                    
*                                                                               
         FOUT  PAYMSGH,=C'AMOUNTS AGREE. CHECK TOTALS N='                       
*                                                                               
         LA    R4,PAYMSG+30                                                     
         LM    R6,R8,PAYTOTS                                                    
         AR    R8,R7                                                            
         A     R8,TOTGSTP          ADD IN GST                      L01          
*                                                                               
*        ADD IN PST                                                             
*                                                                               
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
         LA    R5,10               10 PROVINCES                                 
*                                                                               
         A     R8,TOTPSTP          ADD PST FOR PROVINCE                         
         LA    RF,PSTBCKSL(RF)     NEXT PROVINCE                                
         BCT   R5,*-8                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         EDIT  (R8),(12,0(R4)),2,MINUS=YES                                      
*                                                                               
         CLI   NCDSW,C'N'                                                       
         BE    PAYEM47                                                          
*                                                                               
         AHI   R4,12                                                            
         MVC   3(3,R4),=C'CD='                                                  
         EDIT  (R7),(12,6(R4)),2,MINUS=YES                                      
*                                                                               
PAYEM47  DS    0H                                                               
*                                                                               
         MVC   PAYMSG1+28(2),=C'FX'                                             
*                                                                               
         LA    R4,PAYMSG1+30                                                    
*                                                                               
         LAY   R3,XPAYTOTS         FX TOTALS                                    
*                                                                               
         LM    R6,R8,0(R3)                                                      
         AR    R8,R7                                                            
*                                                                               
         EDIT  (R8),(12,0(R4)),2,MINUS=YES                                      
*                                                                               
         CLI   NCDSW,C'N'                                                       
         BE    PAYEM47X                                                         
*                                                                               
         AHI   R4,12                                                            
         EDIT  (R7),(12,6(R4)),2,MINUS=YES                                      
*                                                                               
PAYEM47X DS    0H                                                               
*                                                                               
PAYEM50  DS    0H                                                               
*                                                                               
         MVI   BYTE4,C'U'          C'U' IN BYTE4 = UNLOCK                       
         BRAS  RE,TSTLOK           GO UNLOCK                                    
*                                                                               
         BRAS  RE,SENDMQ           SEND MQ MESSAGE                              
*                                                                               
         LA    R2,PAYMDH           CURSOR TO MEDIA                              
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TSTDMCB  DS    0H                                                               
         TM    DMCB+8,X'FD'        CHECK EVERYTHING BUT DELETED                 
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTOR VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         CLC   =C'AUTP',PAYMD+1    PRINT AUTOPAY?                               
         BNE   EXXMOD                                                           
         BRAS  RE,UPDAPY           UPDATE AUTOPAY RECORD                        
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
ERRORXT1 DS    0H                                                               
         LH    R3,ADBERNUM           GET MF ERROR CODE                          
*                                                                               
ERRORXT  DS    0H                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         J     EXIT                                                             
*                                                                               
         TITLE 'PPPAY02 -DMGR COMMUNICATION(DIRECTORY)'                         
***********************************************************************         
*                                                                     *         
*        COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         J     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         J     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         J     DIRCTRY                                                          
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         J     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         J     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         J     FILE                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * ** * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
* ROUTINE TO CHECK INVOICES ON SCREEN AND LIST FROM XINV#LST                    
* 'WORK' HAS INVOICE NUMBER TO BE COMPARED                                      
* EXIT WITH EQUAL CONDITION CODE IF INVOICE# MATCH THAT OF 'WORK'               
*                                                                               
* * * * * * * * * * * * ** * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
CKINVLST NTR1  BASE=*,LABEL=*      CHECK AGAINST ALL INVOICES                   
*                                                                               
         LA    R0,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
         LA    R2,PAYINV1H         FIRST INVOICE FIELD                          
         SR    RF,RF                                                            
*                                                                               
CKINVL20 ICM   RF,1,5(R2)          SCREEN INVOICE LENGTH IS ZERO?               
         JZ    CKINVL26                                                         
         MVC   WORK+20(11),8(R2)   INVOICE# ON SCREEN                           
         OC    WORK+20(11),BLANKS                                               
         CLC   WORK+20(11),WORK    MATCH TO INVOICE# ON SCREEN?02907206         
         JE    CKINVLEQ                                                         
*                                                                               
CKINVL26 IC    RF,0(R2)            BUMP TO NEXT INVOICE FIELD                   
         AR    R2,RF               AMOUNT FIELD                                 
         IC    RF,0(R2)                                                         
         AR    R2,RF               COMMENT FIELD                                
         IC    RF,0(R2)                                                         
         AR    R2,RF               INVOICE FIELD                                
         JCT   R0,CKINVL20                                                      
*                                                                               
         TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    CKINVLNQ                                                         
         ICM   R2,15,AXLKINV#      POINT ADDITIONAL INVOICES ARRAY              
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE ADDRESS                            
         CLI   AXLKINV#,LQ_TSINQ   ONE VALUE?                                   
         JNE   *+16                                                             
         LA    R0,1                ONE INVOICE TO PROCESS                       
         LA    R2,6(R2)            POINT TO START OF INVOICES                   
         J     CKINVL60                                                         
         OC    6(2,R2),6(R2)       HAVE NUMBER OF INVOICES?                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,6(R2)          NUMBER OF INVOICES IN ARRAY                  
         LA    R2,8(R2)            POINT TO START OF INVOICES                   
*                                                                               
CKINVL60 CLC   0(INVNUMLQ,R2),WORK                                              
         JE    CKINVLEQ                                                         
         AHI   R2,INVTOTLQ         POINT TO NEXT INVOICE IN ARRARY              
         JCT   R0,CKINVL60                                                      
         J     CKINVLNQ                                                         
*                                                                               
CKINVLEQ CR    RB,RB               EQUAL                                        
         J     *+6                                                              
CKINVLNQ LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * ** * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
CKXMAXDY NTR1  BASE=*,LABEL=*      CHECK FOR ADDITIONAL MAX DAYS                
*                                                                               
         XC    HALF2,HALF2                                                      
         L     R3,ABUYIO           ESTABLISH BUYREC                             
         USING PBUYREC,R3                                                       
*                                                                               
         XC    CHXMWORK,CHXMWORK                                                
         XC    CHXMPA0B,CHXMPA0B                                                
         MVC   CHXMWORK(4),=C'PA0B'                                             
         NI    CHXMWORK,X'BF'      MAKE SYSTEM LOWER CASE                       
         MVC   CHXMWORK+4(2),AGYALPHA                                           
         MVC   CHXMWORK+6(1),PAYMD                                              
         MVC   CHXMWORK+7(3),PBUYKCLT                                           
         CLI   SVPYOFF,C' '                                                     
         JNH   *+14                                                             
         MVI   CHXMWORK+10,C'*'                                                 
         MVC   CHXMWORK+11(1),SVPYOFF                                           
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',CHXMWORK),CHXMPA0B,VDATAMGR                     
*                                                                               
         MVC    HALF2,CHXMPA0B+3   GET BINARY ADDITIONAL MAX DAYS               
         DROP   R3                                                              
*                                                                               
         XIT1                                                                   
CHXMWORK DS    XL48                                                             
CHXMPA0B DS    XL16                A0B PROFILE VALUE                            
         LTORG                                                                  
*                                                                               
*                                                                               
SETTOLRA NTR1  BASE=*,LABEL=*      SET TOLERANCE AMOUNT                         
*                                                                               
         XC    FULL,FULL                                                        
         LHI   RE,50                                                            
         ST    RE,FULL             DEFAULT IS 50 CENTS                          
*                                                                               
         L     R3,ABUYIO           ESTABLISH BUYREC                             
         USING PBUYREC,R3                                                       
*                                                                               
         XC    WORK,WORK                                                        
         XC    STOLPA0B,STOLPA0B                                                
         MVC   WORK(4),=C'PA0B'                                                 
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PAYMD                                                  
         MVC   WORK+7(3),PBUYKCLT                                               
         CLI   SVPYOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVPYOFF                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),STOLPA0B,VDATAMGR                         
*                                                                               
         CLI    STOLPA0B+1,C'Y'    ALLOW PAY TOLERANCE?                         
         JNE    SETCCEQ                                                         
         LLC    RE,STOLPA0B+2      BINARY TOLERANCE AMOUNT (CENTS)              
         LLC    RF,STOLPA0B+4      BINARY TOLERANCE AMOUNT (DOLLARS)            
         MHI    RF,100             CONVERT TO CENTS                             
         AR     RE,RF              TOTAL TOLERANCE AMOUNT                       
         ST     RE,FULL            RETURN PROFILE CONTROLLED AMOUNT             
         DROP   R3                                                              
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
STOLPA0B DS    XL16                A0B PROFILE VALUE                            
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        PREPARE FOR PROCESSING ONE REQHDR PER BUY                    *         
*                                                                     *         
***********************************************************************         
PREPREQ  NTR1  BASE=*,LABEL=*                                                   
         XC    GROSS(64),GROSS     CLEAR GETINS OUTPUT                          
         MVC   SPAYTOTS(PAYTOTSL),PAYTOTS   SAVE FOR "COMPLETION" MSG           
         XC    PAYTOTS(PAYTOTSL),PAYTOTS    CLEAR FOR SINGLE BUY CALC'S         
         MVC   BAMTS(8),SBAMTS     TO RETAIN THE 4 BYTES AFTER INVOICE          
*                                  AMOUNT WHICH WILL BE REPLACED LATER          
         MVC   PAYREP,PREP-PAYTOTSD+SPAYTOTS      RESTORE ANY REP CODE          
         MVC   PAYREPTP,PREPTYP-PAYTOTSD+SPAYTOTS RESTORE ANY REP TYPE          
*                                                                               
         MVC   NBAMTS(8),SBAMTS                                                 
         MVC   SAVPRX(SAVFLDL),SAVPR    SAVE TO RESTORE FOR "NEXT REC"          
         MVI   PARTSW,C' '         TURN OFF PART-PMT SWITCH                     
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 -DMGR COMMUNICATION(DIRECTORY)'                         
***********************************************************************         
*                                                                     *         
*        COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DIRCTRY  NTR1  BASE=*,LABEL=*                                                   
         GOTOR VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         J     DMCHECK                                                          
         TITLE 'PPPAY02 -DMGR COMMUNICATION(FILE)'                              
***********************************************************************         
*                                                                     *         
*        COMMUNICATION WITH DATA MANAGER (FILE)                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FILE     NTR1  BASE=*,LABEL=*                                                   
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         JE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTOR VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         J     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         JNZ   DMERRS                                                           
         XIT1                                                                   
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         J     ERROR                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - BUMP TO NEXT ELMENT IN RECORD - NEXTEL'               
***********************************************************************         
*                                                                     *         
*        BUMP TO NEXT ELEMENT IN RECORD                               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
NEXTEL   DS    0H                                                 L01           
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BER   RE                                                               
         CLI   0(R5),0                                                          
         JNE   *-18                                                             
         LTR   R5,R5                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - BUMP TO NEXT FIELD ON SCREEN - BUMPFLD'               
***********************************************************************         
*                                                                     *         
*        BUMP TO NEXT FIELD ON SCREEN                                 *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
BUMPFLD2 DS    0H                                                 L01           
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*        *                                                                      
BUMPFLD  DS    0H                                                               
BUMPFLDX SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BER   RE                                                 L01           
         CLI   0(R2),0                                                          
         BR    RE                                                 L01           
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - CHECK FOR TRANSATION OVERRIDE - INIT'                 
***********************************************************************         
*                                                                     *         
*        INITIALIZE PROGRAM                                           *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LH    RE,=Y(BUYIO-GENOLD)                                              
         AR    RE,RC                                                            
         ST    RE,ABUYIO           A(PBUYREC)                                   
         LA    RE,33(RE)                                                        
         ST    RE,ABUYEL           START OF BUY ELEMENTS                        
*                                                                               
         LH    RE,=Y(CLRREC-GENOLD)                                             
         AR    RE,RC                                                            
         ST    RE,ACLRREC                                                       
*                                                                               
         LH    RE,=Y(CSHRCVC-GENOLD)                                            
         AR    RE,RC                                                            
         ST    RE,ACSHRCVC         A(CSHRCV CONTROL BLOCK)                      
*                                                                               
         USING CSHRCVD,RE          ESTABLISH CONTROL BLOCK                      
*                                                                               
         XC    CSHRCVD(CSHRCVDL),CSHRCVD   INIT CONTROL BLOCK                   
         MVI   CRIND,CRI1STQ       INDICATE FIRST TIME                          
         MVC   CRCOMA,VCOMFACS     PASS A(COMFACS)                              
*                                                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 VCALLOV,DMCB,(4,0),(RA)    OVERLAY 4                             
         CLI   4(R1),X'FF'         MUST FIND IT                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   VCSHRCV,0(R1)       SAVE ADDRESS                                 
*                                                                               
         XC    BLBFBXLE,BLBFBXLE   INIT BILL BUFFER BXLE PARMS                  
*                                                                               
         OI    DMINBTS,X'88'       SET TO PASS DELETES                          
         OI    DMOUTBTS,X'FD'                                                   
*                                                                               
*                                  TSTLNE = NEXT BYTE OF TEST LINE              
*                                  TSTADDR= TEST LINE HEADER                    
*                                  TSTLNEX= END OF TEST LINE                    
         LA    R2,PAYTSTH                                                       
         ST    R2,TSTADDR                                                       
         LA    R2,8(R2)                                                         
         ST    R2,TSTLNE                                                        
         LA    R2,77(R2)                                                        
         ST    R2,TSTLNEX                                                       
         ZAP   SVCD(2),=P'0'                                                    
*                                                                               
         L     RE,ADBWRKA          ESTABLISH ADBUYER WORK AREA                  
         USING ADBWRKD,RE                                                       
         LA    RF,ADBYTBL          POINT TO START OF "PAID BUYS" TABLE          
         ST    RF,ADBSVPT          SAVE THIS ADDRESS                            
         DROP  RE                                                               
*                                                                               
         CLC   =C'AUTP',PAYMD+1    AUTOPAY?                                     
         JNE   INITX                                                            
         LAY   RF,AUTPSER          SERIAL # IN 1ST COMMENT FIELD                
         MVC   0(L'AUTPSER,RF),PAYCMT1H   SAVE IT TO UPD AUTOPAY REC            
         MVI   PAYCMT1H+5,0        AND DELETE IT FROM SCREEN SO IT DOES         
         XC    PAYCMT1,PAYCMT1     NOT GET SAVED AS A COMMENT                   
*                                                                               
INITX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - INIT ACCUMULATORS - ACCINIT'                          
***********************************************************************         
*                                                                     *         
*        INITIALIZE ACCUMULATORS                                      *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
ACCINIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        INIT ACCUMULATORS                                                      
*                                                                               
         SR    R8,R8               PAYABLE NCD                                  
         SR    R7,R7               PAYABLE CD                                   
         SR    R6,R6               PAYABLE GROSS                                
         XC    PAYTOTS(PAYTOTSL),PAYTOTS        ACCUMULATORS                    
         MVC   PAYREP,SVREP        SAVE ANY REP CODE                            
         MVC   PAYREPTP,SVREPTYP   SAVE REP TYPE                                
*                                                                               
         LAY   RF,XPAYTOTS                                                      
         XC    0(PAYTOTSL,RF),0(RF)  FX ACCUMULATORS                            
         LAY   RE,AGYFXREP         POINT TO AGENCY FX REP                       
         MVC   FXREP-XPAYTOTS(L'FXREP,RF),0(RE)     SET FX REP CDE              
         MVI   FXREPTP-XPAYTOTS(RF),C'S'            SET AS SPECIAL REP          
*                                                                               
         XC    GSTAMTS(PBMXIV#Q*4),GSTAMTS                                      
         MVC   PAYGSTAX,=X'FFFF'  INITIALISE                                    
         MVC   SVPAYGST,=X'FFFF'  INITIALISE                                    
         XC    TOTGSTP,TOTGSTP    TOTAL GST PAYABLE                             
         MVI   THISGSTC,0         GST CODE FOR THIS BUY                         
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LAY   RF,PSTBCKS          PST DATA AREA                                
         USING PSTBCKS,RF                                                       
*                                                                               
         XC    PSTBCKS(PSTBCKSL),PSTBCKS  PST FIELDS                            
         MVC   PAYPSTAX,=X'FFFF'  INITIALISE                     L01            
         LA    RF,PSTBCKSL(RF)                                                  
         BCT   R0,*-16                                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   SPOFFSW,0           INIT SPECIAL OFFICE SWITCH                   
*                                                                               
         NI    FXSW,X'FF'-X'30'    NO FX COMPONENT AS YET                       
*                                                                               
ACCINITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS - CCINV'                  
***********************************************************************         
*                                                                     *         
*        EDIT INVOICES FOR CCUSA                                      *         
*              EACH AMOUNT MUST HAVE AN INVOICE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CCINV    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,PAYINV1H         POINT TO FIRST INVOICE FIELD                 
         LA    R8,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
*                                                                               
CCINVLP  CLI   5(R2),6             MAX 6 CHAR INV # FOR CCUSA                   
         BH    CCINVERR                                                         
*                                                                               
         CLI   5(R2),0             IF NO INVOICE ENTERED                        
         BNE   CCINVCN                                                          
*                                                                               
         ST    R2,FULL                SAVE R2                                   
*                                                                               
         BRAS  RE,BUMPFLD             BUMP TO AMOUNT FIELD                      
*                                                                               
         CLI   5(R2),0                IF AMT INPUT                              
         L     R2,FULL                   RE-POINT TO INVOICE FIELD              
         BNE   CCINVERR                  ERROR - CCUSA REQUIES INV              
*                                                                               
CCINVCN  BRAS  RE,BUMPFLD2         BUMP TO COMMENT FIELD                        
         BRAS  RE,BUMPFLD          BUMP TO NEXT INVOICE FIELD                   
         BCT   R8,CCINVLP                                                       
*                                                                               
* ADDITIONAL INVOICE# AND AMOUNTS (XINV#LST)                                    
*                                                                               
         TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    CCINVDN                                                          
*                                                                               
         LA    R2,PAYINV1H         POINT TO FIRST INVOICE FIELD                 
         ST    R2,FULL             DEFAULT ERROR TO FIRST INVOICE FLD           
*                                                                               
         ICM   R8,15,AXLKINV#      POINT ADDITIONAL INVOICES ARRAY              
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE ADDRESS                            
         CLI   AXLKINV#,LQ_TSINQ   ONE VALUE?                                   
         JNE   *+16                                                             
         LA    R0,1                ONE INVOICE TO PROCESS                       
         LA    R8,6(R8)            POINT TO START OF INVOICES                   
         J     CCINV12                                                          
         OC    6(2,R8),6(R8)       HAVE NUMBER OF INVOICES?                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,6(R8)          NUMBER OF INVOICES IN ARRAY                  
         LA    R8,8(R8)            POINT TO START OF INVOICES                   
*                                                                               
CCINV12  CLC   7(INVNUMLQ-7,R8),BLANKS                                          
         JH    CCINVERR                                                         
*                                                                               
         CLC   0(INVNUMLQ,R8),BLANKS                                            
         JH    CCINV16                                                          
*                                                                               
         CLC   INVNUMLQ(INVAMTLQ,R8),BLANKS                                     
         JNH   CCINVERR                                                         
*                                                                               
CCINV16  AHI   R8,INVTOTLQ         POINT TO NEXT INVOICE IN ARRARY              
         JCT   R0,CCINV12                                                       
*                                                                               
CCINVDN  DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         XIT1                                                                   
*                                                                               
CCINVERR DS    0H                                                               
*                                                                               
         LA    R3,2                FLDINV                                       
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   RF,D#VINVNO           OCCURS WHILE DOING AN                      
         STH   RF,ADBERFLD           ADBUYER CALL                               
*                                                                               
         LTR   RB,RB               SET NEQ CC                                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40302  VALIDATE PAY AMTS AND PROCESS - AMTVAL'                 
***********************************************************************         
*                                                                     *         
*        EDIT AMOUNTS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
AMTVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    THISBGST,THISBGST   INIT GST AND PST AREAS                       
         LA    R0,10               TEN PROVINCES                                
         LAY   RF,PSTBCKS          PST DATA AREA                                
         USING PSTBCKS,RF                                                       
         XC    THISBPST,THISBPST   INIT THIS BUY'S PST ACCUMULATORS             
         XC    THISB$BS,THISB$BS   INIT THIS BUY'S PST ACCUMULATORS             
         LA    RF,PSTBCKSL(RF)                                                  
         JCT   R0,*-16                                                          
         DROP  RF                                                               
*                                                                               
         XC    BAMTS(PBMXIV#Q*BAMTL),BAMTS                                      
*                                                                               
         XC    ADBERNUM,ADBERNUM   INIT ADBUYER ERROR NUMBER                    
*                                                                               
         LA    R2,PAYAMT1H         POINT TO FIRST AMOUNT FIELD                  
         LA    R7,BAMTS            POINT TO AMOUNT STORAGE AREAS                
         LA    R8,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
*                                                                               
         CLI   5(R2),0             FIRST AMOUNT FIELD MUST HAVE DATA            
         JE    AMTERR1                                                          
*                                                                               
AMTVALLP DS    0H                                                               
*                                                                               
         USING BAMTD,R7            ESTABLISH INV STORAGE AREAS                  
*                                                                               
         CLI   5(R2),0                                                          
         JE    AMTVAL60            SKIP IF NO INPUT                             
*                                                                               
         LLC   R5,5(R2)            GET LENGTH OF AMOUNT FIELD                   
*                                                                               
         MVI   BAMTTYPE,C'1'       ASSUME REGULAR PAYMENT                       
*                                                                               
         TM    4(R2),X'08'         OKAY IF NUMERIC FIELD                        
         JO    AMTVAL20                                                         
*                                                                               
         LA    R4,8-2(R5,R2)       POINT TO LAST 2 BYTES OF INPUT               
*                                                                               
* DETERMINE IF CR OR CK SITUATION                                               
*                                                                               
         CLC   =C'CR',0(R4)        CREDIT AMOUNT?                               
         JNE   *+16                                                             
         MVI   BAMTTYPE,C'2'       SET CREDIT AMOUNT TYPE                       
         AHI   R5,-2               DECREMENT AMOUNT INPUT LENGTH                
         J     AMTVAL20                                                         
*                                                                               
         CLC   =C'CK',0(R4)        CHECK AMOUNT?                                
         JNE   *+16                                                             
         MVI   BAMTTYPE,C'3'       SET CHECK AMOUNT TYPE                        
         AHI   R5,-2               DECREMENT AMOUNT INPUT LENGTH                
         J     AMTVAL20                                                         
*                                                                               
AMTVAL20 DS    0H                                                               
*                                                                               
         CLI   CRCKSW,0            REVERSING CR/CK?                             
         JE    AMTVAL40                                                         
*                                                                               
         MVI   MLTREQSW,0          TURN OFF SINGLY OPTION                       
*                                                                               
         CLI   BAMTTYPE,C'1'       REULAR PAYMENT?                              
         JE    AMTERR4                                                          
*                                                                               
* CAN'T MIX CR AND CK PAYMENTS                                                  
*                                                                               
         CLI   BAMTTYPE,C'2'       CR PAYMENT?                                  
         JNE   AMTVAL30                                                         
         CLI   CRCKSW,X'40'        CR ALREADY ENTERED?                          
         JE    AMTVAL25                                                         
*                                                                               
         CLI   CRCKSW,C'R'         1ST AMOUNT CHECKED?                          
         JNE   AMTERR5                                                          
*                                                                               
         MVI   CRCKSW,X'40'        SET SWITCHING CR TO CK                       
*                                                                               
AMTVAL25 MVI   BAMTTYPE,C'3'       REVERSING CR TO CK                           
*                                                                               
* NOTE THAT 2'S TO 3'S CHANGED AT PAYEM9                                        
* NEED TO SWITCH THEM BACK AND REVERSE AMOUNTS                                  
*                                                                               
         J     AMTVAL40                                                         
*                                                                               
AMTVAL30 CLI   BAMTTYPE,C'3'       CHECK PAYMENT?                               
         JNE   AMTVAL40                                                         
*                                                                               
         CLI   CRCKSW,X'80'        ALREADY ENTERED?                             
         JE    AMTVAL35                                                         
*                                                                               
         CLI   CRCKSW,C'R'         1ST AMOUNT CHECKED?                          
         JNE   AMTERR5                                                          
*                                                                               
         MVI   CRCKSW,X'80'        SET SWITCHING CK TO CR                       
*                                                                               
AMTVAL35 MVI   BAMTTYPE,C'2'       REVERSING CK TO CR                           
*                                                                               
* NOTE THAT 2'S TO 3'S CHANGED AT PAYEM9                                        
* NEED TO SWITCH THEM BACK AND REVERSE AMOUNTS                                  
*                                                                               
         B     AMTVAL40                                                         
*                                                                               
AMTVAL40 DS    0H                  VALIDATE DOLLARS ENTERED                     
*                                                                               
         GOTO1 VCASHVAL,DMCB,8(R2),(R5)                                         
         CLI   0(R1),0                                                          
         JNE   AMTERR6                                                          
         ICM   R0,15,4(R1)         GET BINARY VALUE                             
         JM    AMTERR7                                                          
         STCM  R0,15,BAMT          SAVE AMOUNT                                  
         MVI   BAMTINP,C'C'        SET HAVE INPUT                               
         CLI   BAMTTYPE,C'1'       SKIP IF REGULAR PAYMENT                      
         JE    *+10                                                             
         LNR   R0,R0               ELSE CR OR CK - REVERSE SIGN                 
         ST    R0,BAMT                                                          
*                                                                               
* CHECK FOR SPECIAL COMMENT ENTRIES                                             
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO COMMENT FIELD                        
*                                                                               
         CLC   8(3,R2),=C'XCD'     EXCLUDE CD FROM THIS INVOICE?                
         JNE   *+8                                                              
         MVI   BAMTXCD,C'X'                                                     
*                                                                               
         CLC   8(3,R2),=C'NCD'     PAY WITHOUT CD?                              
         JNE   AMTVAL45                                                         
*                                                                               
         CLI   NCDSW,C'L'          ALREADY PAYING WITH LOST CD?                 
         JE    AMTERR8                                                          
         MVI   NCDSW,C'N'          SET SWITCH                                   
         J     AMTVAL50                                                         
*                                                                               
AMTVAL45 CLC   8(3,R2),=C'LCD'     AGENCY LOST CD?                              
         JNE   AMTVAL50                                                         
*                                                                               
         CLC   AGYALPHA,=C'SJ'                                                  
         JNE   AMTERR9             ONLY SJ FOR NOW                              
*                                                                               
         CLI   NCDSW,C'N'          ALREADY PAYING W/O CD?                       
         JE    AMTERR8                                                          
         MVI   NCDSW,C'L'          SET SWITCH                                   
         J     AMTVAL50                                                         
*                                                                               
AMTVAL50 DS    0H                                                               
*                                                                               
         J     AMTVALCN                                                         
*                                                                               
* IF NO INVOICE NUMBER, TEST FOR COMMENTS                                       
*                                                                               
AMTVAL60 BRAS  RE,BUMPFLD          BUMP TO COMMENT FIELD                        
*                                                                               
         CLI   5(R2),0             HAVE COMMENT?                                
         JE    AMTVALDN                                                         
*                                                                               
         LLC   RF,BAMTCOM#         BUMP # OF 'EXTRA' COMMENTS                   
         AHI   RF,1                                                             
         STC   RF,BAMTCOM#                                                      
*                                                                               
AMTVALCN DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPFLD2         BUMP TO NEXT AMOUNT FIELD                    
*                                                                               
         CLI   5(R2),0             AMOUNT ENTERED?                              
         JE    *+8                                                              
         LA    R7,BAMTL(R7)        BUMP TO NEXT ENTRY IN AMOUNT TABLE           
*                                                                               
         JCT   R8,AMTVALLP                                                      
*                                                                               
AMTVALDN DS    0H                                                               
*                                                                               
         CLI   CRCKSW,C'R'         CR/CK SWITCH & NO CR OR CK?                  
         JE    AMTERR10                                                         
*                                                                               
* CLEAR REMAINING FIELDS ON SCREEN                                              
*                                                                               
AMTCLRLP DS    0H                                                               
         CLI   0(R2),9             END OF SCREEN?                               
         JE    AMTCLRDN                                                         
*                                                                               
         LLC   R5,0(R2)            GET FIELD LENGTH                             
         AHI   R5,-9               SUBTRACT HEADER LENGTH                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE LENGTH                 
         EX    R5,*+8                                                           
         J     *+10                                                             
         OC    8(0,R2),8(R2)       SKIP IF FIELD EMPTY                          
         JZ    AMTCLRCN                                                         
*                                                                               
         EX    R5,*+8                                                           
         J     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR SCREEN FIELD                           
*                                                                               
         FOUT  (R2)                DISPLAY IELD ON SCREEN                       
*                                                                               
AMTCLRCN DS    0H                                                               
         BRAS  RE,BUMPFLD          BUMP TO NEXT FIELD ON SCREEN                 
         JE    AMTCLRLP                                                         
*                                                                               
AMTCLRDN DS    0H                                                               
*                                                                               
         BRAS  RE,PRCXINV#         PROCESS ADDITIONAL INVOICES                  
         JNE   AMTERR11                                                         
*                                                                               
         CR    RB,RB               SET CONDITION CODE TO EQUAL                  
         J     AMTVALX                                                          
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
AMTERR1  DS    0H                  FIRST AMOUNT REQUIRED                        
         LA    R3,PPEPYRQD         SET ERROR CODE                               
         LHI   RF,D#AMTCLR         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR2  DS    0H                  INVALID INPUT                                
         LA    R3,PPEFLDNV                                                      
         LHI   RF,D#AMTCLR         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR4  DS    0H                  NO REGULAR PAY WITH CR/CK REVERSAL           
         LA    R3,PPEPYNRG                                                      
         LHI   RF,D#AMTCLR         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR5  DS    0H                  MUST BE ALL CR OR ALL CK                     
         LA    R3,PPECRCK1                                                      
         LHI   RF,D#AMTCLR         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR6  DS    0H                  INVALID DOLLAR AMOUNT                        
         LA    R3,PPENV$S                                                       
         LHI   RF,D#AMTCLR         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR7  DS    0H                  MUST BE NOT NON-NEGATIVE                     
         LA    R3,PPENOTNG                                                      
         LHI   RF,D#AMTCLR         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR8  DS    0H                  NOT NCD & LCD TOGETHER                       
         LA    R3,PPENLCD                                                       
         LHI   RF,D#AMTCLR         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR9  DS    0H                  DDS ONLY OPTION                              
         LA    R3,PPEDDSOP                                                      
         LHI   RF,D#AMTCLR         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR10 DS    0H                  NO CR OR CK FOUND FOR CR/CK SWITCH           
         LA    R3,PPECRCK2                                                      
         LA    R2,PAYINV1H         CURSOR TO 1ST INVOICE FIELD                  
         LHI   RF,D#VINVNO         SET ADBUYER FIELD CODE                       
         J     AMTERR                                                           
*                                                                               
AMTERR11 DS    0H                  ERROR FROM ADDITIONAL INVOICES               
         LA    R2,PAYINV1H         CURSOR TO 1ST INVOICE FIELD                  
         LHI   RF,D#INVNUM         ADDTIONAL INVOICE MAP CODE                   
         J     AMTERR                                                           
*                                                                               
AMTERR   DS    0H                                                               
         STH   R3,ADBERNUM         SAVE ERROR NUMBER FOR LINK                   
         STH   RF,ADBERFLD         SAVE ERROR MAP CODE FOR LINK                 
*                                                                               
         LTR   RB,RB               SET CONDITION CODE TO NOT EQUAL              
*                                                                               
AMTVALX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCXINV# NTR1  BASE=*,LABEL=*      PROCESS ADDITIONAL INVOICES                  
*                                                                               
         TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    PRCXI#_X                                                         
*                                                                               
         LA    R7,BAMTS            POINT TO AMOUNT STORAGE AREAS                
         LHI   RE,PBMXIVSQ*BAMTL                                                
         AR    R7,RE               POINT TO ADDITIONAL INVOICES AREA            
         USING BAMTD,R7            ESTABLISH INV STORAGE AREAS                  
*                                                                               
         ICM   R6,15,AXLKINV#      POINT ADDITIONAL INVOICES ARRAY              
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE ADDRESS                            
*                                                                               
         CLI   AXLKINV#,LQ_TSINQ   ONE VALUE?                                   
         JNE   PRCXI#20                                                         
         LA    R8,1                ONE INVOICE TO PROCESS                       
         LA    R6,6(R6)            POINT TO START OF INVOICES                   
         J     PRCXI#60                                                         
*                                                                               
PRCXI#20 OC    6(2,R6),6(R6)       HAVE NUMBER OF INVOICES?                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R8,R8                                                            
         ICM   R8,3,6(R6)          NUMBER OF INVOICES IN ARRAY                  
         LA    R6,8(R6)            POINT TO START OF INVOICES                   
*                                                                               
PRCXI#60 CLC   0(INVNUMLQ,R6),BLANKS                                            
         JNL   *+6                                                              
         DC    H'0'                INVALID INVOICE DATA IN ARRAY                
*                                                                               
         LA    R2,INVNUMLQ(R6)     POINT TO AMOUNT                              
         MVI   BAMTTYPE,C'1'       ASSUME REGULAR PAYMENT                       
*                                                                               
         LA    R4,INVAMTLQ-2(R2)   POINT TO LAST 2 BYTES OF AMOUNT              
         LHI   R5,INVAMTLQ         LENGTH OF AMOUNT                             
*                                                                               
         CLC   =C'CR',0(R4)        CREDIT AMOUNT?                               
         JNE   *+16                                                             
         MVI   BAMTTYPE,C'2'       SET CREDIT AMOUNT TYPE                       
         AHI   R5,-2               DECREMENT AMOUNT INPUT LENGTH                
         J     PRCXI#66                                                         
*                                                                               
         CLC   =C'CK',0(R4)        CHECK AMOUNT?                                
         JNE   *+12                                                             
         MVI   BAMTTYPE,C'3'       SET CHECK AMOUNT TYPE                        
         AHI   R5,-2               DECREMENT AMOUNT INPUT LENGTH                
*                                                                               
PRCXI#66 CLI   CRCKSW,0            REVERSING CR/CK?                             
         JE    PRCXI#80                                                         
*                                                                               
         MVI   MLTREQSW,0          TURN OFF SINGLY OPTION                       
         CLI   BAMTTYPE,C'1'       REULAR PAYMENT?                              
         JE    PRCXIER4                                                         
*                                                                               
         CLI   BAMTTYPE,C'2'       CR PAYMENT?                                  
         JNE   PRCXI#72                                                         
         CLI   CRCKSW,X'40'        CR ALREADY ENTERED?                          
         JE    PRCXI#68                                                         
*                                                                               
         CLI   CRCKSW,C'R'         1ST AMOUNT CHECKED?                          
         JNE   PRCXIER5                                                         
*                                                                               
         MVI   CRCKSW,X'40'        SET SWITCHING CR TO CK                       
*                                                                               
PRCXI#68 MVI   BAMTTYPE,C'3'       REVERSING CR TO CK                           
*                                                                               
         J     PRCXI#80                                                         
*                                                                               
PRCXI#72 CLI   BAMTTYPE,C'3'       CHECK PAYMENT?                               
         JNE   PRCXI#80                                                         
*                                                                               
         CLI   CRCKSW,X'80'        ALREADY ENTERED?                             
         JE    PRCXI#76                                                         
*                                                                               
         CLI   CRCKSW,C'R'         1ST AMOUNT CHECKED?                          
         JNE   PRCXIER5                                                         
*                                                                               
         MVI   CRCKSW,X'80'        SET SWITCHING CK TO CR                       
*                                                                               
PRCXI#76 MVI   BAMTTYPE,C'2'       REVERSING CK TO CR                           
*                                                                               
PRCXI#80 GOTO1 VCASHVAL,DMCB,0(R2),(R5)                                         
         CLI   0(R1),0                                                          
         JNE   PRCXIER6                                                         
*                                                                               
         ICM   R0,15,4(R1)         GET BINARY VALUE                             
         JM    PRCXIER7                                                         
*                                                                               
         STCM  R0,15,BAMT          SAVE AMOUNT                                  
         MVI   BAMTINP,C'C'        SET HAVE INPUT                               
         CLI   BAMTTYPE,C'1'       SKIP IF REGULAR PAYMENT                      
         JE    *+10                                                             
         LNR   R0,R0               ELSE CR OR CK - REVERSE SIGN                 
         ST    R0,BAMT                                                          
*                                                                               
* CHECK FOR SPECIAL COMMENT ENTRIES                                             
*                                                                               
         AHI   R2,INVAMTLQ         POINT TO COMMENT                             
*                                                                               
         CLC   0(3,R2),=C'XCD'     EXCLUDE CD FROM THIS INVOICE?                
         JNE   *+8                                                              
         MVI   BAMTXCD,C'X'                                                     
*                                                                               
         CLC   0(3,R2),=C'NCD'     PAY WITHOUT CD?                              
         JNE   PRCXI#84                                                         
*                                                                               
         CLI   NCDSW,C'L'          ALREADY PAYING WITH LOST CD?                 
         JE    PRCXIER8                                                         
         MVI   NCDSW,C'N'          SET SWITCH                                   
         J     PRCXI#90                                                         
*                                                                               
PRCXI#84 CLC   8(3,R2),=C'LCD'     AGENCY LOST CD?                              
         JNE   PRCXI#86                                                         
*                                                                               
         CLC   AGYALPHA,=C'SJ'                                                  
         JNE   PRCXIER9            ONLY SJ FOR NOW                              
*                                                                               
         CLI   NCDSW,C'N'          ALREADY PAYING W/O CD?                       
         JE    PRCXIER8                                                         
         MVI   NCDSW,C'L'          SET SWITCH                                   
         J     PRCXI#90                                                         
*                                                                               
PRCXI#86 CLC   0(INVSRCLQ,R2),BLANKS                                            
         JNH   PRCXI#90                                                         
         LLC   RF,BAMTCOM#         BUMP # OF 'EXTRA' COMMENTS                   
         AHI   RF,1                                                             
         STC   RF,BAMTCOM#                                                      
*                                                                               
PRCXI#90 AHI   R6,INVTOTLQ         POINT TO NEXT INVOICE IN ARRAY               
         LA    R7,BAMTL(R7)        BUMP TO NEXT ENTRY IN AMOUNT TABLE           
         JCT   R8,PRCXI#60         PROCESS NEXT INVOICE IN ARRARY               
*                                                                               
PRCXI#_X CR    RB,RB               EQUAL                                        
         XIT1                                                                   
*                                                                               
PRCXI#ER LTR   RB,RB               NOT EQUAL                                    
         XIT1  REGS=(R3)           R3 CONTAINS ERROR NUMBER                     
*                                                                               
PRCXIER4 DS    0H                  NO REGULAR PAY WITH CR/CK REVERSAL           
         LA    R3,PPEPYNRG                                                      
         J     PRCXI#ER                                                         
*                                                                               
PRCXIER5 DS    0H                  MUST BE ALL CR OR ALL CK                     
         LA    R3,PPECRCK1                                                      
         J     PRCXI#ER                                                         
*                                                                               
PRCXIER6 DS    0H                  INVALID DOLLAR AMOUNT                        
         LA    R3,PPENV$S                                                       
         J     PRCXI#ER                                                         
*                                                                               
PRCXIER7 DS    0H                  MUST BE NOT NON-NEGATIVE                     
         LA    R3,PPENOTNG                                                      
         J     PRCXI#ER                                                         
*                                                                               
PRCXIER8 DS    0H                  NOT NCD & LCD TOGETHER                       
         LA    R3,PPENLCD                                                       
         J     PRCXI#ER                                                         
*                                                                               
PRCXIER9 DS    0H                  DDS ONLY OPTION                              
         LA    R3,PPEDDSOP                                                      
         J     PRCXI#ER                                                         
*                                                                               
INVNUMLQ EQU   11                  MAX LENGTH OF INVOICE#                       
INVAMTLQ EQU   12                  MAX LENGTH OF INVOICE$                       
INVSRCLQ EQU   40                  MAX LENGHT OF COMMENTS                       
INVTOTLQ EQU   INVNUMLQ+INVAMTLQ+INVSRCLQ                                       
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - CHECK FOR TRANSATION OVERRIDE - SINGTST'              
***********************************************************************         
*                                                                     *         
*        EDIT SCREEN FOR SINGLY OPTION                                *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
SINGTST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SCRCOMSW,0          INIT SCREEN COMMENT SWITCH                   
*                                                                               
         CLI   MLTREQSW,C'Y'       SINGLY OPTION ?                              
         BNE   SINGTSTX            NO                                           
*                                                                               
         CLI   PAYCMT1H+5,0        ANYTHING IN 1ST COMMENT ?                    
         BE    *+8                 NO                                           
         MVI   SCRCOMSW,C'Y'       SCREEN COMMENT ENTERED                       
*                                                                               
         LA    RE,PAYINV2H         ONLY ALLOW ONE INVOICE, ONE AMOUNT           
         LA    RF,PAYTSTH          AND MULTIPLE COMMENTS TO BE ENTERED          
*                                                                               
SINGTLP  DS    0H                  MORE THAN ONE INVOICE/AMT TESTING            
*                                                                               
         CLI   5(RE),0             ANYTHING THERE ?  (INVOICE)                  
         BNE   SINGTNG             YES - ERROR                                  
*                                                                               
         ZIC   R0,0(RE)                                                         
         AR    RE,R0               BUMP TO NEXT HEADER                          
*                                                                               
         CLI   5(RE),0             ANYTHING THERE ?  (AMOUNT)                   
         BNE   SINGTNG             YES - ERROR                                  
*                                                                               
         ZIC   R0,0(RE)                                                         
         AR    RE,R0               BUMP TO NEXT HEADER (COMMENT)                
*                                                                               
         CLI   5(RE),0             ANYTHING THERE ?                             
         BE    *+8                 NO - CONTINUE                                
         MVI   SCRCOMSW,C'Y'       INDICATE COMMENTS ARE ON SCREEN              
*                                                                               
SINGTCN  DS    0H                                                               
*                                                                               
         ZIC   R0,0(RE)                                                         
         AR    RE,R0               BUMP TO NEXT HEADER                          
         CR    RE,RF               END OF INPUT FIELDS ?                        
         BL    SINGTLP             NO - TEST NEXT                               
*                                                                               
SINGTDN  DS    0H                                                               
*                                                                               
         B     SINGTSTX                                                         
*                                                                               
SINGTNG  DS    0H                                                               
*                                                                               
         LA    R3,MULTERR1         ONLY 1 INVOICE AND AMOUNT ALLOWED            
         STH   R3,ADBERNUM           FOR USE IF EDIT ERROR                      
         LHI   R0,D#VINVN2           OCCURS WHILE DOING AN                      
         STH   R0,ADBERFLD           ADBUYER CALL                               
*                                                                               
         LR    R2,RE               SET CURSOR                                   
*                                                                               
SINGTSTX DS    0H                                                               
*                                                                               
         XIT1  (R2)                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - CHECK FOR TRANSATION OVERRIDE - PBLOVCK'              
***********************************************************************         
*                                                                     *         
*        CHECK FOR TRANSACTION OVERRIDE                               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
PBLOVCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'OVRD',PAYMD+1    IF TEST IS BEING OVERRIDDEN                  
         BE    *+10                                                             
         CLC   =C'OVRD',PAYMD+5       'MTESTOVRD'                               
         BNE   PBLOVCKX            NE CC                                        
*                                                                               
         TM    TWAAUTH-TWAD(RA),X'80'   AND USER HAS AUTHORITY                  
         BO    PBLOVCKX               (BIT MUST BE OFF)                         
*                                                                               
         CR    RB,RB                  SET EQ CC                                 
*                                                                               
PBLOVCKX DS    0H                                                               
*                                                                               
         XIT1                       RETURN                                      
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - CHECK CASH DISCOUNT AND NV LETTER (FIXCD)'            
*                                                                               
***********************************************************************         
*                IF NCDSW = N MOVE ZEROS TO CD IN                     *         
*NTRY    R5 ==>  PBDELEM                                              *         
*                IF CD  MOVE PERCENT TO SAVE AREA                     *         
***********************************************************************         
         DS    0D                                                               
FIXCD    NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE2,0                                                          
         CLI   PROGPROF+3,C'Y'     SEE IF NOT USING NV'S                        
         JNE   FIXCD5                                                           
         USING PBDELEM,R5                                                       
         TM    PBDLETR,X'01'        SEE IF BIT ALREADY ON                       
         JO    FIXCD5               MEANS PAID OR ATTEMPTED PAY                 
         OI    PBDLETR,X'01'        TO STOP NV LETTER                           
         MVI   BYTE2,1              SET NEED TO WRITE RECORD                    
FIXCD5   CLI   BYTE3,C'N'                                                       
         JE    FIXCD8               DON'T REMOVE CD IF NOT PAYING               
         CLI   NCDSW,C'N'                                                       
         JNE   FIXCD8                                                           
         MVC   CSHDSC,=F'0'                                                     
         ZAP   PBDCD,=P'0'                                                      
         MVI   BYTE2,1              SET NEED TO WRITE RECORD                    
FIXCD8   CP    SVCD(2),=P'0'                                                    
         JNE   FIXCD10                                                          
         ZAP   SVCD(2),PBDCD                                                    
FIXCD10  CLI   BYTE2,1              SEE IF I MUST REWRITE RECORD                
         JNE   FIXCDX               NO                                          
         MVC   AREC,ABUYIO          POINT TO BUY I/O AREA                       
         GOTO1 PUTREC                                                           
         TM    DMCB+8,X'FD'        CHECK EVERYTHING BUT DELETED                 
         JZ    FIXCDX                                                           
         DC    H'0'                                                             
FIXCDX   DS    0H                                                               
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40302 - PARTIAL PAYMENT LOGIC - PARTPMT'                       
*=====================================================================*         
*---> REPLACE BUY REC COST WITH PART PAYMENT AMOUNT AND "REDO" GETINS *         
*=====================================================================*         
         SPACE 1                                                                
         DS    0D                                                               
PARTPMT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         CLI   0(R5),X'20'                                                      
         JE    *+6                                                              
         DC    H'0'                MUST BE X'20' PBDELEM                        
         USING PBDELEM,R5                                                       
         MVC   SPBDCOS,PBDCOS      SAVE BUY COST                                
         MVC   SPBDCSTY,PBDCOSTY     AND COST TYPE                              
         MVC   SPBDPRCS,PBDPRCOS     AND PREMIUM CHARGE                         
*                                                                               
         L     R1,BTOT             PART-PMT AMOUNT IS IN BTOT                   
*                                                                               
         CLI   GNOPT,C'N'          PAYMENT ENTERED AT NET ?                     
         JNE   PART40              NO - GO TO PMT ENTERED AT GROSS              
*                                                                               
         TM    PBDCNDA,X'80'       CANADIAN BUY ?                               
         JNO   PART05              NO                                           
*                                                                               
         OC    PBDTAX,PBDTAX       TAX ON BUY ?                                 
         JNZ   PART05              YES - NO GST/PST CALCULATION                 
*                                                                               
         MVC   FULL(1),PBDGST      TAKE GST CODE FROM BUY                       
         CLI   FULL,0              IF CODE NOT ENTERED IN BUY                   
         JNE   *+8                                                              
         MVI   FULL,C'S'              DEFAULT TO STANDARD                       
*                                                                               
         BAS   RE,GETCAN           CALCULATE CANADIAN GROSS FROM NET            
*                                                                               
         J     PART20              CANADIAN NET ADJUSTED - DO AGY COMM          
*                                (R1 RETURNED WITH ADJUSTED PAY AMOUNT)         
*                                                                               
PART05   DS    0H                  MUST "GROSS-UP" PAYMENT FOR GETINS           
*                                                                               
         OC    PBDTAX,PBDTAX       TAX ON BUY ?                                 
         JZ    PART20              NO                                           
*                                                                               
         CP    PBDACP,=P'-1'       100 PCT AGENCY COMMISSION ?                  
         JE    PART60              YES - NO ADJUSTMENT AND NO TAX $             
*                                                                               
         ZICM  RE,PBDTAX,3         TAX (4 DECIMALS)                             
         CVD   RE,DUB                                                           
         AP    DUB,=P'1000000'     DUB NOW HAS DIVISION FACTOR                  
         MVC   WORK+20(8),DUB      SAVE DIVISION FACTOR IN WORK+20              
         CVD   R1,DUB              ENTERED PART-PAY AMOUNT                      
         ZAP   WORK(16),DUB        PAY AMT TO WORK                              
         MP    WORK(16),=P'10000000'   SET FOR DIVISION                         
         MVC   DUB,WORK+20         DIVISION FACTOR                              
         DP    WORK(16),DUB+4(4)   ADJUSTED PAY AMT                             
         CP    WORK+11(1),=P'0'    SEE IF NEGATIVE                              
         JNL   PART15              NO                                           
         CP    WORK+11(1),=P'-5'                                                
         JH    PART18                                                           
         SP    WORK(12),=P'10'     ROUNDING                                     
         J     PART18                                                           
PART15   CP    WORK+11(1),=P'5'                                                 
         JL    PART18                                                           
         AP    WORK(12),=P'10'     ROUNDING                                     
PART18   DP    WORK(12),=P'10'     BACK TO TWO DECIMALS                         
         MVC   DUB,WORK+2          ADJUSTED PAY AMT TO DUB                      
         CVB   R1,DUB                AND THEN TO R1                             
*                                                                               
PART20   DS    0H                  NOW ADJUST FOR AGENCY COMMISSION             
*                                                                               
         CLI   PBDCOSIN,C'S'       BUY MADE WHERE COST = NET                    
         JE    PART60              NO AGENCY COMMISSION                         
*                                                                               
         ZAP   WORK(16),=P'100000'                                              
*                                                                               
         SP    WORK(16),PBDACP     100.000 MINUS AGY COMM (3 DECIMALS)          
         MP    WORK(16),=P'10'     FOR 4 DECIMALS                               
         MVC   WORK+20(8),WORK+8   SAVE DIVISION FACTOR IN WORK+20              
         CVD   R1,DUB              ENTERED OR ADJUSTED PAY AMOUNT               
         ZAP   WORK(16),DUB        PAY AMT TO WORK                              
         MP    WORK(16),=P'10000000'   SET FOR DIVISION                         
         MVC   DUB,WORK+20         DIVISION FACTOR                              
         DP    WORK(16),DUB+4(4)   ADJUSTED PAY AMT                             
         CP    WORK+11(1),=P'0'    SEE IF NEGATIVE                              
         JNL   PART30              NO                                           
         CP    WORK+11(1),=P'-5'                                                
         JH    PART35                                                           
         SP    WORK(12),=P'10'     ROUNDING                                     
         J     PART35                                                           
PART30   CP    WORK+11(1),=P'5'                                                 
         JL    PART35                                                           
         AP    WORK(12),=P'10'     ROUNDING                                     
PART35   DP    WORK(12),=P'10'     BACK TO TWO DECIMALS                         
         MVC   DUB,WORK+2          ADJUSTED PAY AMT TO DUB                      
         CVB   R1,DUB                AND THEN TO R1                             
*                                                                               
         J     PART60              DONE WITH PAY ENTERED AT NET                 
*                                                                               
*                                  WHEN THERE IS TAX ON BUY                     
*                                  MUST ADJUST GROSS PAY AMOUNT                 
*                                  TO GROSS $ NOT INCLUDING TAX                 
*                                  BEFORE CALLING GETINS AGAIN                  
PART40   DS    0H                                                               
*                                                                               
         TM    PBDCNDA,X'80'       CANADIAN BUY ?                               
         JNO   PART45              NO                                           
*                                                                               
         OC    PBDTAX,PBDTAX       TAX ON BUY ?                                 
         JNZ   PART45              YES - NO GST/PST CALCULATION                 
*                                                                               
         MVC   FULL(1),PBDGST      TAKE GST CODE FROM BUY                       
         CLI   FULL,0              IF CODE NOT ENTERED IN BUY                   
         JNE   *+8                                                              
         MVI   FULL,C'S'              DEFAULT TO STANDARD                       
*                                                                               
         BAS   RE,GETCAN           CALCULATE CANADIAN GROSS                     
*                                                                               
         J     PART60              CANADIAN GROSS ADJUSTED                      
*                                (R1 RETURNED WITH ADJUSTED PAY AMOUNT)         
PART45   DS    0H                                                               
*                                                                               
         OC    PBDTAX,PBDTAX       TAX ON BUY ?                                 
         JZ    PART60              NO                                           
         CP    PBDACP,=P'-1'       100 PCT AGENCY COMMISSION ?                  
         JE    PART60              YES - NO ADJUSTMENT  (NO TAX $)              
*                                                                               
         ZAP   WORK(16),=P'100000'                                              
*                                                                               
         CLI   PBDCOSIN,C'S'       BUY MADE WHERE COST = NET                    
         JE    PART50              NO AGENCY COMMISSION                         
*                                                                               
         SP    WORK(16),PBDACP     100.000 MINUS AGY COMM (3 DECIMALS)          
PART50   MP    WORK(16),=P'10'     FOR 4 DECIMALS                               
         ZICM  RE,PBDTAX,3         TAX (4 DECIMALS)                             
         CVD   RE,DUB                                                           
         MP    WORK(16),DUB+3(5)                                                
         DP    WORK(16),=P'10000000'    BACK TO 4 DECIMALS                      
         AP    WORK(11),=P'100000' WORK NOW HAS DIVISION "FACTOR"               
         CVD   R1,DUB              PART-PAY AMOUNT                              
         MVC   WORK+20(8),DUB      SAVE PAY AMT IN WORK+20                      
         ZAP   DUB,WORK+7(4)       DIVISION "FACTOR" TO DUB                     
         ZAP   WORK(16),WORK+20(8)    PAY AMT TO WORK                           
         MP    WORK(16),=P'1000000'   SET FOR DIVISION                          
         DP    WORK(16),DUB+4(4)   ADJUSTED PAY AMT                             
         CP    WORK+11(1),=P'0'    SEE IF NEGATIVE                              
         JNL   PART52              NO                                           
         CP    WORK+11(1),=P'-5'                                                
         JH    PART55                                                           
         SP    WORK(12),=P'10'     ROUNDING                                     
         J     PART55                                                           
PART52   CP    WORK+11(1),=P'5'                                                 
         JL    PART55                                                           
         AP    WORK(12),=P'10'     ROUNDING                                     
PART55   DP    WORK(12),=P'10'     BACK TO TWO DECIMALS                         
         MVC   DUB,WORK+2          ADJUSTED PAY AMT TO DUB                      
         CVB   R1,DUB                AND THEN TO R1                             
*                                                                               
PART60   DS    0H                                                               
         CVD   R1,DUB                                                           
         MVC   PBDCOS,DUB+3        REPLACE BUY COST WITH PART AMOUNT            
         MVI   PBDCOSTY,C'T'       REPLACE COST TYPE                            
         ZAP   PBDPRCOS,=P'0'      ZERO PREMIUM CHARGE                          
*                                                                               
         TM    PBDCNDA,X'80'       SKIP IF NOT CANADIAN BUY                     
         JNO   PART60C                                                          
         BRAS  RE,BLDPST           UPDATE PST ELEMENT                           
         JE    *+6                 SHOULD NOT HAPPEN - ERRORS SHOULD            
         DC    H'0'                  HAVE BEEN CAUGHT BEFORE THIS               
*                                                                               
PART60C  XC    THISBGST,THISBGST                                                
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LAY   RF,PSTBCKS          PST DATA AREA                                
         USING PSTBCKS,RF                                                       
*                                                                               
         XC    THISBPST,THISBPST                                                
         LA    RF,PSTBCKSL(RF)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
*        MUST SWITCH PAY ELEM CODES TO X'AA'                                    
*        SO CAN USE GETINS TO RETURN PAY DATA FOR THIS "COST"                   
*                                                                               
         MVI   ELCODE,X'25'                                                     
PART70   BRAS  RE,NEXTEL                                                        
         JNE   PART70X                                                          
         MVI   0(R5),X'AA'                                                      
         J     PART70                                                           
*                                                                               
         DROP  R5                                                               
*                                                                               
PART70X  DS    0H                                                               
*                                                                               
         XC    GROSS(64),GROSS     CLEAR GETINS OUTPUT                          
*                                                                               
*  ******  DO GETINS AGAIN FOR RECALC OF GST, PST, GROSS, ETC.  ******          
*                                                                               
         L     RF,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,RF                                                       
*                                                                               
         GOTOR VGETINS,DMCB,PBUYREC,PVALUES,PBUYKPRD,0,=C'PST'                  
*                                              PAYABLE GST TAX                  
         DROP  RF                                                               
*                                                                               
         L     RF,16(R1)          ADDRESS OF GST DATA                           
         LH    R1,=Y(GVALUES-GENOLD)                                            
         LA    R1,GENOLD(R1)                                                    
         USING GVALUES,R1                                                       
*                                                                               
         MVC   GVALUES(GVALUESL),0(RF) COPY GST DATA                            
*                                                                               
         MVC   TOTGSTP,GSTTAX      REPLACE TOTAL WITH "PARTIAL"                 
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LA    RE,PSTAREA          PST SAVEAREA                                 
         LA    RF,PSTAREA-GVALUES(RF)  PST DATA FROM GETINS                     
*                                                                               
         SR    R8,R8               CLEAR FOR ACCUMULATION                       
         SR    R5,R5               CLEAR FOR ACCUMULATION                       
PART80   DS    0H                                                               
*                                                                               
         MVC   0(PSTAREAL,RE),0(RF)  COPY PST DATA                              
         A     R8,4(RF)       ADD "RECALCULATED" PARTIAL PST TAX                
         A     R5,24(RF)      ADD PARTIAL PST TAX $ BASIS                       
         LA    RE,PSTAREAL(RE)    BUMP POINTERS                                 
         LA    RF,PSTAREAL(RF)                                                  
         BCT   R0,PART80                                                        
*                                                                               
         ST    R8,TOTPSTP          REPLACE TOTAL WITH "PARTIAL"                 
*                                              PAYABLE PST TAX                  
         ST    R5,TOT$BSP          REPLACE TOTAL WITH "PARTIAL"                 
*                                         PAYABLE TAX $ BASIS                   
         DROP  R1                                                               
*                                                                               
PART95   DS    0H                                                               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         CLI   0(R5),X'20'                                                      
         JE    *+6                                                              
         DC    H'0'                MUST BE X'20' PBDELEM                        
         USING PBDELEM,R5                                                       
         MVC   PBDCOS,SPBDCOS      RESTORE BUY COST                             
         MVC   PBDCOSTY,SPBDCSTY      AND UNITS INDICATOR                       
         MVC   PBDPRCOS,SPBDPRCS      AND PREMIUM CHARGE                        
         DROP  R5                                                               
*                                                                               
*        MUST SWITCH PAY ELEM CODES BACK TO X'25'                               
*                                                                               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         MVI   ELCODE,X'AA'                                                     
PART95L  BRAS  RE,NEXTEL                                                        
         JNE   PARTXIT                                                          
         MVI   0(R5),X'25'         SWITCH                                       
         J     PART95L                                                          
*                                                                               
PARTXIT  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        FOR CANADIAN BUYS FIND GST CODE, PERCENT AND BASIS           *         
*        ALSO SIMILAR INFORMATION FOR EACH PROVINCE WITH PST          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETCAN   NTR1                                                                   
*                                                                               
*                                                                               
*---------------------------> VERIFY GST CODE TO TABLE                          
*                                                                               
*        GST/PST DETERMINED BY INSERT DATE                                      
*                                                                               
*                                                                               
         L     RF,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,RF                                                       
*                                                                               
         MVC   WGSTDATE,PBUYKDAT   INIT GST DATE WITH INSERT DATE               
*                                                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
*        TABLE IS CL1   GST CODE                                                
*                 XL1   SALES TAX BASIS X'00' NET+GST                           
*                                       X'01' NET                               
*                 XL2   GST PERCENT TO 3 DECIMALS                               
*                 XL3   START DATE BINARY                                       
*                 XL1   SPARE                                                   
*                                                                               
         LA    R5,PGSTTAB                                                       
*                                                                               
GTCGSTLP DS    0H                                                               
*                                                                               
         CLC   0(1,R5),FULL        FIND GST CODE IN TABLE                       
         BNE   GTCGSTCN                                                         
*                                                                               
         CLC   WGSTDATE,4(R5)      FIND STARTING DATE                           
         BNL   GTCGSTFD                                                         
*                                                                               
GTCGSTCN DS    0H                                                               
*                                                                               
         LA    R5,8(R5)            BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R5),X'FF'         IF NOT END OF TABLE                          
         BNE   GTCGSTLP               GO CHECK NEXT TABLE ENTRY                 
*                                                                               
         DC    H'0'                BAD GST CODE                                 
*                                                                               
GTCGSTFD DS    0H                                                               
*                                                                               
         LH    R9,=Y(GVALUES-GENOLD)                                            
         AR    R9,RC                                                            
         USING GVALUES,R9          ESTABLISH GVALUES AND PST DATA               
*                                                                               
         XC    GSTCODE(5),GSTCODE  CLEAR CODE, PCT, & BASIS                     
         XC    PSTCODE(5),PSTCODE         (BOTH GST AND PST)                    
*                                                                               
         OC    2(2,R5),2(R5)       SKIP IF NO GST TAX PCT                       
         JZ    GTCGSTX                                                          
*                                                                               
         MVC   GSTBASIS,1(R5)      SAVE BASIS BYTE                              
         MVC   GSTCODE(1),0(R5)    SAVE CODE                                    
         MVC   GSTPCT,2(R5)        SAVE PERCENT                                 
*                                                                               
GTCGSTX  DS    0H                                                               
*                                                                               
*                                  ******    NOW LOOK FOR PST DATA              
*                                                                               
         L     R6,ABUYEL           POINT TO FIRST ELEMENT IN BUYREC             
         SR    R0,R0                                                            
*                                                                               
GETC10   DS    0H                                                               
*                                                                               
         CLI   0(R6),0             EOR - NO PST ELEMENT                         
         JE    GETC30                 SKIP PST CALCULATIONS                     
*                                                                               
         CLI   0(R6),PBYPSTQ       LOOKING FOR BUY PST ELEMENT                  
         JE    GETC12                                                           
*                                                                               
         IC    R0,1(R6)            GET ELEMENT LENGTH                           
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         J     GETC10                                                           
*                                                                               
GETC12   DS    0H                                                               
*                                                                               
         MVC   WRKPSTC,PBYPSTC-PBYPSTEL(R6)   SAVE PST CODES                    
*                                                                               
*        FIND PROVINCE WITH PST CODE                                            
*                                                                               
         LA    R7,1                PROVINCE COUNTER                             
         LA    R6,WRKPSTC          PST CODES BY PROVINCE                        
*                                                                               
GETC14   DS    0H                                                               
*                                                                               
         CLI   0(R6),0             SKIP IF NO PST CODE PRESENT                  
         JE    GETC28                                                           
*                                                                               
*        FIND PROVINCIAL CODE                                                   
*                                                                               
         LR    R1,R7               COPY PROVINCE COUNTER                        
         BCTR  R1,0                DECREMENT FOR INDEXING                       
         SLL   R1,1                EACH ENTRY IS 2 BYTES                        
         LA    R1,PROVTAB(R1)      POINT TO PROVINCE CODE                       
*                                                                               
*        FIND PST TABLE ENTRY FOR PROVINCE AND PST CODE                         
*                                                                               
         LA    R8,PPSTTAB          POINT TO PST TABLE                           
         LA    RE,PPSTTABN         NUMBER OF ENTRIES IN TABLE                   
*                                                                               
GETC18   DS    0H                                                               
*                                                                               
         CLC   0(2,R8),0(R1)       MATCH ON PROVINCE CODE                       
         JE    *+10                                                             
         CLC   0(2,R8),=C'ZZ'      OR DEFAULT CODE                              
         JNE   GETC20                                                           
*                                                                               
         CLC   2(1,R8),0(R6)       MATCH ON PST CODE                            
         JNE   GETC20                                                           
*                                                                               
         CLC   WGSTDATE,5(R8)      PST/GST MUST BE AFTER INIT DATE              
         JL    GETC20                                                           
*                                                                               
         J     GETC24              TABLE ENTRY FOUND                            
*                                                                               
GETC20   DS    0H                                                               
*                                                                               
         LA    R8,PPSTTABL(R8)     NEXT TABLE ENTRY                             
         BCT   RE,GETC18                                                        
*                                                                               
         J     GETC26              SKIP PST SEARCH                              
*                                                                               
GETC24   DS    0H                  GOT PST CODE ENTRY                           
*                                                                               
         MVC   PSTPROV,0(R1)       SET PROVINCE CODE                            
         MVC   PSTCODE(1),0(R6)    SET PST CODE                                 
*                                                                               
*                                                                               
         CLC   PSTPROV,=C'BC'      DOING BRITISH COLUMBIA?                      
         JNE   GETC24B                                                          
         CLC   WGSTDATE,=X'710401' DATE BEFORE APR1/2013                        
         JL    GETC24B                                                          
         J     GETC24C             DON'T CLEAR GST %                            
*                                                                               
GETC24B  CLI   PSTCODE,C'H'        SEE IF HST CODE FOUND                        
         JNE   *+10                                                             
         XC    GSTPCT,GSTPCT       CLEAR GST PERCENTAGE                         
*                                  SO GST WILL ALWAYS BE ZERO                   
*                                                                               
GETC24C  MVC   PSTPCT,3(R8)        SET PST PER CENT                             
         MVC   PSTBASIS,8(R8)      SET PST BASIS                                
*                                                                               
GETC26   DS    0H                                                               
*                                                                               
         J     GETC30              DONE - ONE PSTPCT IS ENOUGH                  
*                                                                               
GETC28   DS    0H                                                               
*                                                                               
         LA    R6,1(R6)            BUMP CODE POINTER                            
         LA    R7,1(R7)            BUMP PROVINCE COUNTER                        
         CH    R7,=H'10'           MAX 10 PROVINCES                             
         JNH   GETC14                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
*                     ******  COMPUTE GST/PST DIVISION FACTOR  ******           
*********************************************************************           
*                                                                               
GETC30   DS    0H                                                               
*                                                                               
         CLI   GNOPT,C'N'          PAYMENT ENTERED AT NET ?                     
         JNE   GETCG10             NO - GO TO PMT ENTERED AT GROSS              
*                                                                               
         L     RF,=F'100000'       100% NET (3 DECIMALS)                        
         AH    RF,GSTPCT           ADD GST PERCENT                              
*                                                                               
         ST    RF,PSTTAX           SAVE NET + GST PERCENT                       
*                                                                               
         LA    R7,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,R7                                                       
*                                                                               
         LA    R0,10               10 PROVINCES                                 
*                                                                               
GETC40   DS    0H                                                               
*                                                                               
         OC    PSTPROV,PSTPROV     SKIP IF NO MORE PROVINCES WITH PST           
         JNZ   GETC44              FINISH UP - ONE PROVINCE IS ENOUGH           
         LA    R7,PSTAREAL(R7)     BUMP TO NEXT PST AREA                        
         BCT   R0,GETC40           TEST NEXT                                    
         LR    RE,RF               NET + GST PERCENT TO RE                      
         J     GETC75              DONE - NO PST CODE FOUND                     
*                                                                               
GETC44   DS    0H                                                               
*                                                                               
         TM    PSTBASIS,X'01'      PST ON NET ONLY ?                            
         JNO   GETC48              NO - PST ON NET+GST                          
         AH    RF,PSTPCT           ADD PST PERCENT                              
         LR    RE,RF               NET + GST PERCENT + PST PCT TO RE            
         J     GETC75              DONE WITH DIVISION FACTOR                    
*                                                                               
GETC48   DS    0H                                                               
*                                                                               
         SR    RE,RE                                                            
         AH    RE,PSTPCT           ADD PST PERCENT                              
         MR    RE,RE               COMPUTE PST PCT = PST X (NET + GST)          
         D     RE,=F'100000'       BACK TO 3 DECIMALS                           
*                                                                               
*                                  RF HAS RESULT OF (PST X (NET + GST))         
*                                                                               
         L     RE,PSTTAX           NET + GST PERCENT                            
         AR    RE,RF                                                            
*                                                                               
***       NOW GROSS UP PART-PMT USING RE VALUE AS DIVISION FACTOR               
*                                                                               
GETC75   DS    0H                                                               
         L     R1,BTOT             PART-PMT AMOUNT IS IN BTOT                   
         CVD   R1,DUB              ENTERED OR ADJUSTED PAY AMOUNT               
         ZAP   WORK(16),DUB        PAY AMT TO WORK                              
         MP    WORK(16),=P'1000000'   SET FOR DIVISION                          
         CVD   RE,DUB              DIVISION FACTOR                              
         DP    WORK(16),DUB+4(4)   ADJUSTED PAY AMT                             
         CP    WORK+11(1),=P'0'    SEE IF NEGATIVE                              
         JNL   GETC80              NO                                           
         CP    WORK+11(1),=P'-5'                                                
         JH    GETC85                                                           
         SP    WORK(12),=P'10'     ROUNDING                                     
         J     GETC85                                                           
GETC80   CP    WORK+11(1),=P'5'                                                 
         JL    GETC85                                                           
         AP    WORK(12),=P'10'     ROUNDING                                     
GETC85   DP    WORK(12),=P'10'     BACK TO TWO DECIMALS                         
         MVC   DUB,WORK+2          ADJUSTED PAY AMT TO DUB                      
         CVB   R1,DUB                AND THEN TO R1                             
*                                                                               
GETCXIT  DS    0H                                                               
         XIT1  REGS=(R1)           PASS BACK ADJUSTED PAY AMT IN R1             
*                                                                               
         DROP  R7                                                               
*                                                                               
GETCG10  DS    0H                  ADJUST CANADIAN ENTRY AT GROSS               
*                                                                               
         L     R7,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R7                                                       
*                                                                               
         CP    PBDACP,=P'-1'       100 PCT AGENCY COMMISSION ?                  
         JE    GETCGXT             YES - NO ADJUSTMENT AND NO TAX $             
*                                                                               
         DROP  R7                                                               
*                                                                               
         LA    R7,PSTAREA          ESTABLISH PSTAREA                            
         USING PSTAREA,R7                                                       
*                                                                               
         LA    R0,10               10 PROVINCES                                 
*                                                                               
GETCG15  DS    0H                                                               
*                                                                               
         OC    PSTPROV,PSTPROV     SKIP IF NO MORE PROVINCES WITH PST           
         JNZ   GETCG20             FINISH UP - ONE PROVINCE IS ENOUGH           
         LA    R7,PSTAREAL(R7)     BUMP TO NEXT PST AREA                        
         BCT   R0,GETCG15          TEST NEXT                                    
*****    B     GETCG20             NO PST CODE FOUND                            
*                                                                               
GETCG20  DS    0H                                                               
*                                                                               
         L     R5,ABUYEL           POINT TO FIRST ELEMENT IN BUYREC             
         CLI   0(R5),X'20'                                                      
         JE    *+6                                                              
         DC    H'0'                MUST BE X'20' PBDELEM                        
         USING PBDELEM,R5                                                       
*                                                                               
         ZAP   WORK(16),=P'100000'                                              
*                                                                               
         CLI   PBDCOSIN,C'S'       BUY MADE WHERE COST = NET                    
         JE    GETCG22             NO AGENCY COMMISSION                         
*                                                                               
         SP    WORK(16),PBDACP     100.000 MINUS AGY COMM (3 DECIMALS)          
*                                  WORK HAS "NET" FACTOR                        
         DROP  R5                                                               
GETCG22  DS    0H                                                               
*                                                                               
         LH    R1,GSTPCT                                                        
         CVD   R1,DUB              GSTPCT TO DUB                                
         ZAP   WORK+16(16),DUB       AND TO WORK+16                             
         MP    WORK+16(16),WORK+8(8)   GSTPCT X ("NET" FACTOR)                  
*                                                                               
         LH    R1,PSTPCT                                                        
         CVD   R1,DUB              PSTPCT TO DUB                                
         ZAP   WORK+32(16),DUB       AND TO WORK+32                             
         MP    WORK+32(16),WORK+8(8)   PSTPCT X ("NET" FACTOR)                  
*                                                                               
         TM    PSTBASIS,X'01'      PST ON NET ONLY ?                            
         JO    GETCG24             YES                                          
         MVC   WORK(16),WORK+16    GST "FACTOR" TO WORK                         
         MP    WORK(16),DUB        GST "FACTOR" X PSTPCT                        
         DP    WORK(16),=P'100000'   BACK TO 10 DECIMAL PLACES                  
*                                                                               
         AP    WORK+32(16),WORK(12)    ADD GST FACTOR X PSTPCT                  
*                                        TO PST FACTOR                          
GETCG24  DS    0H                                                               
         AP    WORK+32(16),WORK+16(16)  ADD GST FACTOR TO PST FACTOR            
         AP    WORK+32(16),=P'10000000000'    ADD 100%                          
*                                                                               
         L     R1,BTOT             BTOT CONTAINS ENTERED OR                     
         CVD   R1,DUB                ADJUSTED PAY AMOUNT                        
         ZAP   WORK(16),DUB        PAY AMT TO WORK                              
         MP    WORK(16),=P'100000000000'   SET FOR DIVISION                     
*                                                                               
         MVC   DUB,WORK+40         DIVISION FACTOR FROM GETCG22 ABOVE           
*                                                                               
         DP    WORK(16),DUB+2(6)   ADJUSTED PAY AMT                             
         CP    WORK+09(1),=P'0'    SEE IF NEGATIVE                              
         JNL   GETCG30             NO                                           
         CP    WORK+09(1),=P'-5'                                                
         JH    GETCG35                                                          
         SP    WORK(10),=P'10'     ROUNDING                                     
         J     GETCG35                                                          
GETCG30  CP    WORK+09(1),=P'5'                                                 
         JL    GETCG35                                                          
         AP    WORK(10),=P'10'     ROUNDING                                     
GETCG35  DP    WORK(10),=P'10'     BACK TO TWO DECIMALS                         
         MVC   DUB,WORK            ADJUSTED PAY AMT TO DUB                      
         CVB   R1,DUB                AND THEN TO R1                             
*                                                                               
GETCGXT  DS    0H                                                               
         J     GETCXIT                                                          
*                                                                               
*                                                                               
WGSTDATE DS    XL3                 INSERT DATE FROM BUYREC (PBUYKDAT)           
WRKPSTC  DS    XL10                PST CODES FROM BUYREC PST ELEMENT            
*                                                                               
         DROP  R7,R9                                                            
*                                                                               
       ++INCLUDE PGSTTAB                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - DETERMINE A0A PAY OPTIONS - A0AOPT'                   
*===============================================================*               
*                                                               *               
*        DETERMINE PAY & TEARSHEET OPTIONS +8 THROUGH +10       *               
*        IF +8-+10 ARE ALL NULLS USE OLD OPTIONS IN +4 TO SET   *               
*           THEM                                                *               
*        IF ANY IS  'P' THEN SET FROM OPTIONS IN PUB RECORD     *               
*                                                               *               
*EXIT    CC    0  CLIENT DOES NOT HAVE TO HAVE PAID ITS BILL    *               
*                                                               *               
*===============================================================*               
         SPACE 2                                                                
         DS    0D                                                               
A0AOPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    PROGPRO2+8(3),PROGPRO2+8 IF ALL NEW OPTIONS ARE NULL             
         JNZ   A0AOPT10                                                         
*                                  THEN USE PROGPRO2+4 TO SET THEM              
*                                                                               
         MVC   PROGPRO2+8(3),=C'NNN' SET NEW OPTIONS TO DEFAULT                 
*                                                                               
         CLI   PROGPRO2+4,C'T'     IF ALL BUYS MUST HAVE TEARSHEET              
         JE    *+8                                                              
         CLI   PROGPRO2+4,C'S'                                                  
         JNE   *+8                                                              
         MVI   PROGPRO2+8,C'Y'        SET  TEARSHEET OPTION                     
*                                                                               
         CLI   PROGPRO2+4,C'Y'     IF ALL BUYS MUST BE MATCHED                  
         JE    *+8                                                              
         CLI   PROGPRO2+4,C'S'                                                  
         JNE   *+12                                                             
         MVI   PROGPRO2+9,C'Y'        SET ALL MATCHED OPT NORMAL PAY            
         MVI   PROGPRO2+10,C'Y'       SET ALL MATCHED OPT FOR $MAT/PAY          
*                                                                               
         CLI   PROGPRO2+4,C'O'     IF ONLY MATCHED BUYS                         
         JNE   *+12                                                             
         MVI   PROGPRO2+9,C'O'        SET ONLY MATCHED OPT NORMAL PAY           
         MVI   PROGPRO2+10,C'O'       SET ONLY MATCHED OPT FOR $MAT/PAY         
*                                                                               
         CLI   PROGPRO2+4,C'I'     IF ONLY BUYS MATCHED TO INVOICE              
         JNE   *+12                                                             
         MVI   PROGPRO2+9,C'O'        SET ONLY MATCHED OPT NORMAL PAY           
         MVI   PROGPRO2+10,C'I'       SET ONLY INVOICE OPT FOR $MAT/PAY         
*                                                                               
         J     A0AOPTX                                                          
*                                                                               
A0AOPT10 DS    0H                                                               
*                                                                               
         LA    R5,SVREPELM         POINT TO SAVED REPELM                        
         USING PUBREPD,R5          ESTABLISH AS REP ELEMENT                     
*                                                                               
         CLI   PUBREPEL,0          DONE IF NO ELEMENT EXISTS                    
         JE    A0AOPTX                                                          
*                                                                               
         CLI   PUBREPEL+1,53       DONE IF OLD ELEMENT FORMAT                   
         JL    A0AOPTX                                                          
*                                                                               
         CLI   PROGPRO2+8,C'P'     IF OPTION DEPENDS ON PUB                     
         JNE   *+10                                                             
         MVC   PROGPRO2+8(1),PUBPCTL1    REPLACE WITH OPTION IN PUB             
*                                                                               
         CLI   PROGPRO2+9,C'P'     IF OPTION DEPENDS ON PUB                     
         JNE   *+10                                                             
         MVC   PROGPRO2+9(1),PUBPCTL2    REPLACE WITH OPTION IN PUB             
*                                                                               
         CLI   PROGPRO2+10,C'P'    IF OPTION DEPENDS ON PUB                     
         JNE   *+10                                                             
         MVC   PROGPRO2+10(1),PUBPCTL3   REPLACE WITH OPTION IN PUB             
*                                                                               
A0AOPTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - BUILD PST ELEMENT - BLDPST'                           
*===============================================================*               
* OVERRIDE PST VALUES IN BUY PST ELEMENT WITH INPUT             *               
* ALSO OVERRIDE GST                                             *               
* IF NO PAID SPOTS                                              *               
*===============================================================*               
         SPACE 2                                                                
BLDPST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SVGST,SVGST         SKIP IF NO GST OVERRIDE                      
         BNZ   *+10                                                             
         OC    SVPSTEL,SVPSTEL     AND  IF NO PST OVERRIDES                     
         JZ    BLDPSTOK                                                         
*                                                                               
*        IF THERE ARE ANY PAY ELEMENTS                                          
*           ERROR IF PSTCODE/GSTCODE BEING CHANGED                              
*           ELSE CHANGE/ADD UPDATED PST ELEMENT TO BUY                          
*           ELSE CHANGE     GST CODE IN BUY                                     
*                                                                               
         L     R6,ABUYEL           POINT TO START OF BUY ELEMENTS               
         SR    R0,R0                                                            
*                                                                               
BLDPSTPL DS    0H                                                               
*                                                                               
         USING PPAYELEM,R6         ESTABLISH AS PAY ELEMENT                     
*                                                                               
         CLI   PPAYELEM,0             DONE IF END OF RECORD REACHED             
         JE    BLDPSTPD                                                         
*                                                                               
         CLI   PPAYELEM,X'25'      WANT PAY ELEMENTS                            
         JNE   BLDPSTPC                                                         
*                                                                               
         OC    PPDDATE,PPDDATE     THAT HAVE BEEN PAID                          
         JZ    BLDPSTPC                                                         
*                                                                               
         CLC   PPDDATE,=AL1(94,5,1)  ON OR AFTER MAY 1, 1994                    
         JNL   BLDPSTPF                                                         
*                                                                               
BLDPSTPC DS    0H                                                               
*                                                                               
         ICM   R0,1,1(R6)                                                       
         AR    R6,R0                                                            
         J     BLDPSTPL                                                         
*                                                                               
*===================================================================*           
* UNFORTUNATELY, NEED TO ALLOW OVERRIDE IF IT IS THE SAME AS EXISTS *           
* THIS HAPPENS IN CASES WHERE SOME BUYS HAVE BEEN PAID FOR          *           
* PREVIOUS MONTHS AND OTHERS HAVE NOT BEEN PAID AT ALL, SO THEY     *           
* HAVE NO EXISTING OVERRIDES AND USER MUST INPUT THEM               *           
*===================================================================*           
*                                                                               
BLDPSTPF DS    0H                                                               
*                                                                               
         OC    SVGST,SVGST         IF GST CODE BEING CHANGED                    
         BZ    BLDGST10                                                         
*                                                                               
         L     R6,ABUYEL           POINT TO START OF BUY ELEMENTS               
         USING PBDELEM,R6          ESTABLISH BUY DECRIPTION ELM                 
*                                                                               
         CLC   PBDGST,SVGST        GST CODES MUST MATCH                         
         BNE   BLDGST1E                                                         
*                                                                               
BLDGST10 DS    0H                                                               
*                                                                               
         BAS   RE,GETPSTEL         TEST FOR EXISTING PST ELEMENT                
         JNE   BLDPSTER            IF NOT THERE, NO MATCH --> ERROR             
*                                                                               
*        R6 ==> PBYPST ELEMENT                                                  
*                                                                               
         USING PBYPSTEL,R6         ESTABLISH PST ELEMENT                        
*                                                                               
         LA    R0,10               SET LOOP COUNTER                             
         LA    RE,PBYPSTC          PST CODES IN RECORD                          
         LA    RF,SVPSTEL+2        PST OVERRIDES                                
*                                                                               
BLDPSTCL DS    0H                                                               
*                                                                               
         CLI   0(RF),0             SKIP IF OVERRIDE NOT INPUT                   
         JE    BLDPSTCC                                                         
*                                                                               
         CLI   0(RE),0             IF NO CURRENT PST FOR PROVINCE               
         JNE   *+12                                                             
         CLI   0(RF),C'X'          AND OVERRIDE IS 'X'                          
         JE    BLDPSTCC               OKAY                                      
*                                                                               
         CLC   0(1,RF),0(RE)          MUST EQUAL VALUE IN ELEMENT               
         JNE   BLDPSTER                                                         
*                                                                               
BLDPSTCC DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            BUMP POINTERS                                
         LA    RF,1(RF)                                                         
         BCT   R0,BLDPSTCL                                                      
*                                                                               
         J     BLDPSTOK            ALL OKAY                                     
*                                                                               
BLDPSTPD DS    0H                                                               
*                                                                               
*        UPDATE OR ADD PST ELEMENT                                              
*                                                                               
         L     R6,ABUYEL           POINT TO START OF BUY ELEMENTS               
         USING PBDELEM,R6          ESTABLISH BUY DECRIPTION ELM                 
*                                                                               
         CLI   SVGST,0             IF GST BEING OVERRIDDEN                      
         BZ    *+10                                                             
         MVC   PBDGST,SVGST           UPDATE GST CODE                           
*                                                                               
         OC    SVPSTEL,SVPSTEL     SKIP IF NO PST OVERRIDE                      
         BZ    BLDPSTOK                                                         
*                                                                               
         BAS   RE,GETPSTEL         FIND PSTEL                                   
         JE    BLDPST20                                                         
*                                                                               
*        NO EXISTING ELEMENT                                                    
*                                                                               
*                                  ADD ELEMENT AT END OF RECORD                 
         GOTOR VRECUP,DMCB,(C'P',ABUYIO),SVPSTEL,(R6)                           
*                                                                               
BLDPSTPX DS    0H                                                               
*                                                                               
******   J     BLDPSTOK                                                         
*                                                                               
*        UPDATE PST ELEMENT AT R6                                               
*                                                                               
BLDPST20 DS    0H                                                               
*                                                                               
         USING PBYPSTEL,R6         ESTABLISH PST ELEMENT                        
*                                                                               
         LA    R0,10               SET LOOP COUNTER                             
         LA    RE,PBYPSTC          PST CODES IN RECORD                          
         LA    RF,SVPSTEL+2        PST OVERRIDES                                
*                                                                               
BLDPSTLP DS    0H                                                               
*                                                                               
         MVC   0(1,RE),0(RF)          MOVE VALUE TO ELEMENT                     
*                                                                               
         CLI   0(RE),C'X'          CHANGE 'X' TO NULLS                          
         JNE   *+12                                                             
         MVI   0(RE),0                                                          
         J     BLDPSTCN                                                         
******                                                                          
******   CLI   0(RE),C'H'          IF HST                                       
******   BNE   BLDPSTCN                                                         
******                                                                          
******   L     R6,ABUYEL           POINT TO START OF BUY ELEMENTS               
******   USING PBDELEM,R6          ESTABLISH BUY DECRIPTION ELM                 
******                                                                          
******   MVI   PBDGST,0            SET TO NO GST                                
******                                                                          
BLDPSTCN DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            BUMP POINTERS                                
         LA    RF,1(RF)                                                         
         BCT   R0,BLDPSTLP                                                      
*                                                                               
BLDPSTDN DS    0H                                                               
*                                                                               
BLDPSTOK DS    0H                                                               
         CR    RB,RB               EQ CC                                        
         J     BLDPSTX             DONE                                         
*                                                                               
*        ERROR ROUTINE                                                          
*                                                                               
BLDGST1E LA    R3,PPEPDIFX         ALREADY PAID WITH DIFFERENT GST              
         STH   R3,ADBERNUM           FOR USE IF ADB CALL                        
         B     BLDPSTES                                                         
*                                                                               
BLDPSTER LA    R3,BADPSTPD                                                      
         STH   R3,ADBERNUM           FOR USE IF ADB CALL                        
*                                                                               
BLDPSTES LA    R2,PAYOPH           INVALID PST OVERRIDE                         
         LHI   RF,D#MEDCOD           FOR USE IF DOING AN                        
         STH   RF,ADBERFLD           ADBUYER CALL                               
*                                                                               
         L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
*                                                                               
         GOTOR VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
         OI    6(R2),OI1C          INSERT CURSOR                                
*                                                                               
         L     R4,ERRAREA                                                       
*                                                                               
         FOUT  (R4)                                                             
*                                                                               
         LTR   RB,RB               NEQ CC                                       
*                                                                               
BLDPSTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        FIND PST ELEMENT IN BUY RECORD                                         
*                                                                               
GETPSTEL NTR1                                                                   
*                                                                               
         L     R6,ABUYEL           POINT TO START OF BUY ELEMENTS               
         SR    R0,R0                                                            
*                                                                               
GETPSTLP DS    0H                                                               
*                                                                               
         USING PBYPSTEL,R6         ESTABLISH AS BUY PST ELEMENT                 
*                                                                               
         CLI   PBYPSTEL,0          DONE IF END OF RECORD REACHED                
         JE    GETPSTDN                                                         
*                                                                               
         CLI   PBYPSTEL,X'84'      WANT PST ELEMENT                             
         JE    GETPSTFD                                                         
*                                                                               
GETPSTCN DS    0H                                                               
*                                                                               
         ICM   R0,1,1(R6)                                                       
         AR    R6,R0                                                            
         J     GETPSTLP                                                         
*                                                                               
GETPSTFD DS    0H                  ELEMENT FOUND                                
         CR    RB,RB               EQ CC                                        
         J     GETPSTX                                                          
*                                                                               
GETPSTDN DS    0H                  NO ELEMENT FOUND                             
         LTR   RB,RB               NEQ CC                                       
*                                                                               
GETPSTX  DS    0H                                                               
         XIT1  REGS=(R6)                                                        
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         TITLE 'PPPAY02 - INVOICE CHECKING REQUEST (ICLREQ)'                    
*                                                                               
***********************************************************************         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
*                                  INVOICE CHECKING REQUEST                     
ICLREQ   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,REQ                                                           
         LH    R2,=Y(REQHDR-GENOLD)                                             
         AR    R2,RC                                                            
         USING REQHDR,R2           ESTABLISH REQUEST RECORD                     
*                                                                               
         CLC   WKSTART+4(2),=C'00'                                              
         JNE   *+8                                                              
         MVI   QSTART+5,C'1'                                                    
         MVI   REQNUMB,X'15'                                                    
         MVC   QCODE(2),=C'21'                                                  
         MVC   QBINPAY(4),BTOT                                                  
         MVC   QUESTR(12),PAYRQ                                                 
         CLC   QUESTR(4),=4C' '                                                 
         JNE   *+10                                                             
         MVC   QUESTR,=C'ACCNTG'                                                
*                                                                               
         CLI   ICLOPT,C'N'         SEE IF AUTO ICL TO BE PRODUCED               
         JE    ICLREQX             NO                                           
         BRAS  RE,WTREQ                                                         
*                                                                               
ICLREQX  DS    0H                                                               
         XIT1                                                                   
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         TITLE 'PPPAY02 - CHECK PUB BILL PAID OPTION - OPTCHK'                  
*===============================================================*               
*                                                               *               
*        CHECK IF CLIENT REQUIRES CASH TO BE RECEIVED FOR       *               
*          BILLS INVOLVING THIS PUB                             *               
*                                                               *               
*EXIT    CC    0  CLIENT DOES NOT HAVE TO HAVE PAID ITS BILL    *               
*                                                               *               
*===============================================================*               
         SPACE 2                                                                
         DS    0D                                                               
OPTCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,SVREPELM         POINT TO SAVED REPELM                        
         USING PUBREPD,R5          ESTABLISH AS REP ELEMENT                     
*                                                                               
         CLI   PUBREPEL,0          OKAY TO PAY IF NO ELEMENT EXISTS             
         JE    OPTCHKOK                                                         
*                                                                               
         CLI   PUBREPEL+1,45       OKAY TO PAY IF OLD ELEMENT FORMAT            
         JL    OPTCHKOK                                                         
*                                                                               
         TM    PUBCCTL,X'01'       IF CLIENT MUST HAVE PAID BILLS               
         JNO   OPTCHKOK                                                         
*                                                                               
         LTR   RB,RB                  NE CC                                     
         J     OPTCHKX                                                          
*                                                                               
OPTCHKOK DS    0H                                                               
*                                                                               
         CR    RB,RB               SET = CC                                     
*                                                                               
OPTCHKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 -ALLOCATE XPAYTOTS TO XBAMTS - FXALLOC'                 
***********************************************************************         
*                                                                     *         
*        TAKE FX AMOUNTS AND ALLOCATE TO INVOICES BASED ON AMOUNTS    *         
*           ENTERED FOR EACH INVOICE                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
FXALLOC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,PBMXIV#Q         MAX NUMBER OF BUCKETS                        
         LAY   R2,XPAYTOTS                                                      
         USING PAYTOTSD,R2         ESTABLISH FX TOTALS                          
*                                                                               
         LA    R3,PBAMTS           POINT TO INVOICE FX AMOUNTS                  
         USING BAMTD,R3            ESTABLISH INVOICE DATA                       
*                                                                               
         XC    PBTOT,PBTOT         CLEAR FX TOTAL FOR ALL INVOICES              
         LA    R4,BAMTS            POINT TO INVOICE AMOUNTS                     
*                                                                               
         L     RF,BTOT             INVOICE TOTALS                               
         CVD   RF,DUB                                                           
*                                                                               
FXALOOP  DS    0H                                                               
*                                                                               
*              COPY INVOICE CHARACTERISTICS TO FX BUCKETS                       
         MVC   BAMTTYPE(4),BAMTTYPE-BAMT(R4)                                    
*                                                                               
         ICM   RF,15,0(R4)         INVOICE AMOUNT                               
         BZ    FXACONT                NO INVOICE AMOUNT                         
*                                                                               
         CVD   RF,PL8              CVD                                          
         ZAP   PL16,PL8            MOVE TO WORK AREA                            
         SRP   PL16,5,0            *10**5 FOR ROUNDING                          
*                                                                               
         DP    PL16,DUB            RATIO OF INVOICE TO TOTAL - 5 DECS           
         ZAP   PL16,PL16(8)                                                     
*                                                                               
         L     RF,PTOTNLCD         GET FX NET LESS CD                           
         A     RF,PTOTCD           PLUS CD FOR NET                              
*                                                                               
         CLI   GNOPT,C'G'          IF PAYING GROSS                              
         BNE   *+8                                                              
         L     RF,PTOTG                USE GROSS                                
*                                                                               
         CVD   RF,PL8              CVD                                          
*                                                                               
         MP    PL16,PL8            PORTION ALLOCATED TO THIS INVOICE            
         SRP   PL16,64-5,5         ROUND TO PENNIES                             
         CVB   RF,PL16+8           CVB                                          
         ST    RF,BAMT             SAVE FX AMOUNT FOR INVOICE                   
*                                                                               
         A     RF,PBTOT            UPDATE FX INVOICE TOTAL                      
         ST    RF,PBTOT                                                         
*                                                                               
FXACONT  DS    0H                                                               
*                                                                               
         LA    R4,BAMTL(R4)        NEXT INVOICE AMOUNT                          
         LA    R3,BAMTL(R3)        NEXT FX INVOICE AMOUNT                       
*                                                                               
         BCT   R0,FXALOOP          NEXT INVOICE                                 
*                                                                               
FAXADONE DS    0H                                                               
*                                                                               
*  IF THERE IS A DIFFERENCE IN FX TOTALS DUE TO ROUNDING, ALTER                 
*        ADD DIFFERENCE TO LARGEST FX INVOICE  AMOUNT                           
*                                                                               
FXAIV    DS    0H                                                               
*                                                                               
         L     R0,PTOTNLCD         NET LESS CD                                  
         A     R0,PTOTCD           PLUS CD = NET                                
*                                                                               
         CLI   GNOPT,C'G'          IF PAYING GROSS                              
         BNE   *+8                                                              
         L     R0,PTOTG               USE GROSS                                 
*                                                                               
         S     R0,PBTOT            DIFF BETWEEN FILE AND INVS                   
         BZ    FXAIVX              NO DIFFERENCE - DONE                         
*                                                                               
         LA    R1,PBAMTS           FX INVOICE AMOUNTS                           
         LR    R5,R1               FX INVOICE AMOUNTS                           
         LA    RE,PBMXIV#Q         MAX INVOICES                                 
         XC    FULL,FULL                                                        
*                                                                               
FXAIVLP  DS    0H                                                               
*                                                                               
         L     RF,0(R1)            GET FX INVOICE AMOUNT                        
         LPR   RF,RF               ABSOLUTE VALUE                               
*                                                                               
         C     RF,FULL             IF FX INVOICE LARGER THAN PRIOR IV           
         BNL   FXAIVCN                                                          
*                                                                               
         ST    RF,FULL                SAVE IT AS LARGEST                        
         LR    R5,R1                  SAVE FX INVOICE POINTER                   
*                                                                               
FXAIVCN  DS    0H                                                               
         LA    R1,BAMTL(R1)        BUMP TO NEXT FX INVOICE                      
         BCT   RE,FXAIVLP          LOOP TO NEXT FX INVOICE                      
*                                                                               
         A     R0,0(R5)            DIFFERENCE ADDED TO LARGEST FX INV           
         ST    R0,0(R5)                                                         
*                                                                               
FXAIVX   DS    0H                                                               
*                                                                               
FXALLOCX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 -BUILD BASIC REQUEST RECORD - REQ'                      
***********************************************************************         
*                                                                     *         
*        INIT REQUEST RECORD WITH FIELDS COMMON TO BOTH TYPES         *         
*           OF REQUESTS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
REQ      NTR1  BASE=*,LABEL=*                                                   
         LH    R2,=Y(REQHDR-GENOLD)                                             
         AR    R2,RC                                                            
         USING REQHDR,R2           ESTABLISH REQUEST RECORD                     
*                                                                               
         XC    REQHDR(26),REQHDR                                                
*                                                                               
         LA    R1,QIAREA           INIT 5 CARDS WORTH                           
         LA    R0,10               40*10 = 5*80                                 
         MVC   0(40,R1),BLANKS     BLANKS IS ONLY 50 LONG                       
         LA    R1,40(R1)                                                        
         JCT   R0,*-10                                                          
*                                                                               
         MVC   QAGY(2),AGYALPHA                                                 
         MVC   QMED(1),PAYMD                                                    
         MVC   QCLT(3),PAYCL                                                    
         MVC   QPRD(3),SAVPR                                                    
         MVC   QSTART(12),WKSTART                                               
*                                                                               
         GOTOR VPUBEDIT,DMCB,BPUB,(C'Q',WORK)                                   
*                                                                               
         CLI   BPUB+4,X'FF'                                                     
         JNE   *+10                                                             
         MVC   WORK+8(3),=C'ZZZ'                                                
*                                                                               
         MVC   QPUB(11),WORK                                                    
*                                                                               
REQX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         TITLE 'PPPAY02 -WRITE REQUEST - WTREQ'                                 
***********************************************************************         
*                                                                     *         
*        WRITE REQUEST TO FILE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
WTREQ    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LH    R2,=Y(REQHDR-GENOLD)   ESTABLISH REQUEST RECORD                  
         AR    R2,RC                                                            
         USING REQHDR,R2                                                        
*                                                                               
         OC    QUESTR(12),BLANKS   FORCE UPPERCASE                              
*                                                                               
         GOTOR VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',REQHDR,REQHDR               
*                                                                               
         TM    8(R1),X'FF'         CHECK FOR ERRORS                             
         JZ    WTREQX                                                           
         DC    H'0'                NO ERRORS TOLERATED                          
*NOP*    BZ    *+12                                                             
*NOP*    MVI   8(R1),X'FF'                                                      
*NOP*    BAS   RE,TSTDMCB                                                       
*                                                                               
WTREQX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         TITLE 'PPPAY02 - TEST FOR OFFLINE LOCKING  - TSTLOK'                   
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BYTE4,C'U'          IS THIS AN UNLOCK                            
         JE    TSTLOK50            YES                                          
*                                                                               
         LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
*                                                                               
******   XC    LKUPKEY,LKUPKEY                                                  
         XC    WORK,WORK                                                        
L        USING LKKEYD,WORK                                                      
*                                                                               
         MVC   L.LOCKAGY,PBUYKAGY                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,PBUYKMED                                               
         MVC   L.LOCKCLT,PBUYKCLT                                               
         DROP  RE                                                               
*                                                                               
TSTLOK20 L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB,('LKTESTQ',WORK),VCOMFACS                              
         CLI   4(R1),1             TEST LOCKED                                  
         JE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         JE    TSTLOK20                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT ?                                 
         JNE   TSTLOK28            NO                                           
*                                                                               
         MVC   L.LOCKCLT,SVCLPROF+6   USE MASTER CLIENT NOW                     
         DROP  L                                                                
*                                                                               
TSTLOK24 L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB,('LKTESTQ',WORK),VCOMFACS                              
         CLI   4(R1),1             TEST LOCKED                                  
         JE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         JE    TSTLOK24                                                         
*                                  CHECK CLIENT/PUB LOCK                        
TSTLOK28 LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
*                                                                               
         XC    WORK,WORK                                                        
L        USING LKKEYD,WORK                                                      
*                                                                               
         MVC   L.LOCKAGY,PBUYKAGY                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT/PUB LOCK                              
         MVC   L.LOCKMED,PBUYKMED                                               
         MVC   L.LOCKCLT,PBUYKCLT                                               
         MVC   L.LOCKPUB,PBUYKPUB                                               
         XC    L.LOCKPUB,=4X'FF'   TO ELIMINATE X'00' FIELDS                    
         DROP  RE                                                               
*                                                                               
TSTLOK30 L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB,('LKTESTQ',WORK),VCOMFACS                              
         CLI   4(R1),1             TEST LOCKED                                  
         JE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         JE    TSTLOK30                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT ?                                 
         JNE   TSTLOK40            NO                                           
*                                                                               
         MVC   L.LOCKCLT,SVCLPROF+6   USE MASTER CLIENT NOW                     
         DROP  L                                                                
*                                                                               
TSTLOK34 L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB,('LKTESTQ',WORK),VCOMFACS                              
         CLI   4(R1),1             TEST LOCKED                                  
         JE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         JE    TSTLOK34                                                         
*                                  ADD A PUB LOCK                               
TSTLOK40 DS    0H                                                               
         J     TSTLKEQ             DON'T ISSUE LOCKS                            
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB,('LKLOCKQ',WORK),VCOMFACS                              
         CLI   4(R1),1             TEST ALREADY LOCKED                          
         JE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         JE    TSTLOK40                                                         
         CLI   4(R1),0             ANY OTHER ERROR  SEND                        
         JNE   TSTLKNEQ            RECORD LOCKED MESSAGE                        
         J     TSTLKEQ                                                          
*                                                                               
*                                  REMOVE A PUB LOCK                            
TSTLOK50 DS    0H                                                               
         J     TSTLKEQ             DON'T UNLOCK                                 
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB,('LKUNLKQ',WORK),VCOMFACS                              
         CLI   4(R1),2             TEST TABLE BUSY                              
         JE    TSTLOK50                                                         
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         J     *+6                                                              
TSTLKNEQ LTR   RB,RB               REC LOCKED - SEND MESSAGE                    
TSTLOKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
******LKUPKEY  DS    XL16                DATA LOCKING KEY                       
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - PRINT BUY DETAILS IN PAYTST LINE'                     
*=================================================================*             
*   DISPLAY BUY KEY IN LOWER PART OF SCREEN (PAYTST LINE)         *             
*   IN THE EVENT OF AN ERROR IN THE "SINGLY" OPTION               *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
DISPBUY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PAYTST,PAYTST       CLEAR LINE                                   
         OI    PAYTSTH+6,X'08'     HIGH INTENSITY                               
         FOUT  PAYTSTH                                                          
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         LA    R4,PAYTST+1         POINT TO SCREEN LINE                         
         MVC   0(16,R4),=C'DISALLOWED BUY -'                                    
         LA    R4,17(R4)                                                        
*                                                                               
         MVC   0(4,R4),=C'PUB:'                                                 
         LA    R4,4(R4)                                                         
         GOTO1 VPUBEDIT,DMCB,PBUYKPUB,(C'Q',0(R4))                              
         LA    R4,18(R4)                                                        
*                                                                               
         MVC   0(4,R4),=C'PRD:'                                                 
         LA    R4,4(R4)                                                         
         MVC   0(L'PBUYKPRD,R4),PBUYKPRD                                        
         LA    R4,5(R4)                                                         
*                                                                               
         MVC   0(4,R4),=C'EST:'                                                 
         LA    R4,4(R4)                                                         
         EDIT  (2,PBUYKEST),(4,0(R4)),ALIGN=LEFT                                
         AR    R4,R0                                                            
         LA    R4,2(R4)                                                         
*                                                                               
         MVC   0(5,R4),=C'DATE:'                                                
         LA    R4,5(R4)                                                         
         MVI   BYTE,8              PUT OUT MMMDD/YY                             
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+8                                                              
         MVI   BYTE,9              JUST PUT OUT MONTH/YEAR                      
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(BYTE,0(R4)) BUY DATE OR MONTH         
*                                                                               
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+14                                                             
         XC    3(3,R4),3(R4)       DON'T WANT THE YEAR                          
         SH    R4,=H'2'      BACK-UP SO LINE NUMBER WILL FOLLOW MTH             
*                                                                               
         CLI   PBUYKLIN,1          IF BUYLINE 1                                 
         BE    DSPBYX               DONE                                        
         MVI   5(R4),C'-'          ELSE SHOW ME THE BUYLINE                     
*                                                                               
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    DSPBY30                                                          
*                                  DISPLAY 100 - 239 AS A0 - N9                 
         DP    DUB,=P'10'                                                       
         UNPK  7(1,R4),DUB+7(1)                                                 
         OI    7(R4),X'F0'                                                      
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,SBUYATAB(RF)                                                  
         MVC   6(1,R4),0(RF)                                                    
         B     DSPBYX              DONE                                         
*                                                                               
DSPBY30  OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R4),DUB                                                      
         CLI   6(R4),C'0'                                                       
         BNE   DSPBYX                                                           
         MVC   6(2,R4),7(R4)                                                    
*                                                                               
*                                                                               
DSPBYX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
SBUYATAB DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         TITLE 'PPPAY02 -OUTPUT BUY INFO FOR ADBUYER REPLY'                     
***********************************************************************         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
ABDTLWRK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
         L     R8,ADBWRKA          ESTABLISH ADBUYER WORK AREA                  
         USING ADBWRKD,R8                                                       
         L     R7,ADBSVPT          ADDRESS OF NEXT ADBYTBL ENTRY                
         USING ADBYRPLY,R7                                                      
*                                                                               
         XC    ADBYRPLY(ABRPLNTH),ADBYRPLY      CLEAR REPLY FIELDS              
         MVI   ABRPLNTH(R7),X'FF'  SET END OF REPLY DATA INDICATOR              
*                                                                               
         L     R6,ABUYEL           POINT TO 1ST ELEM IN PBUYREC                 
ABTST10  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    ABTSTX              SERIAL# ELEM NOT FOUND                       
         CLI   0(R6),X'99'         SERIAL# ELEM ?                               
         BE    ABHAVE              YES - GO PROCESS                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               NEXT ELEMENT                                 
         B     ABTST10             TEST NEXT                                    
*                                                                               
ABHAVE   DS    0H                                                               
         USING PSERELEM,R6         POINTING TO X'99' SERIAL# ELEM               
         UNPK  ABRPSER#,PSERNUM                                                 
         OI    ABRPSER#+8,X'F0'                                                 
         DROP  R6                                                               
*                                                                               
ABTSTX   DS    0H                                                               
         CLI   PARTSW,C'Y'         IF PART PMT, ABRPAMT WAS STORED IN           
         BE    ABTSTX4               PAYEM45 - SKIP BELOW NET CALC              
         L     R0,GROSS            GET THE NET PAID FOR REPLY                   
         S     R0,PGROSS                                                        
         CLI   GSTINDET,C'Y'       INCLUDE GST IN AMOUNTS                       
         BNE   *+8                                                              
         A     R0,THISBGST         THIS BUY'S PAYABLE GST                       
         CLI   GNOPT,C'G'                                                       
         BE    *+12                                                             
         A     R0,PAGYCOM                                                       
         S     R0,AGYCOM                                                        
         ST    R0,FULL                                                          
         MVC   ABRPAMT,FULL        REPLY AMOUNT IN BINARY                       
ABTSTX4  MVI   ADBYTBLX,X'FF'      MEANS DATA HAS BEEN STORED                   
*                                                                               
*NOP*    CP    SERTBL(L'SERTBL),=P'0'  SERIAL#S SENT BY ADBUYER ?               
*NOP*    BH    ABDTLWX             YES - END OF REPLY OUTPUT                    
*                                                                               
*                 OUTPUT ADDITIONAL REPLY DATA                                  
         GOTO1 VPUBEDIT,DMCB,PBUYKPUB,(C'S',ABRPPUB)    PUB                     
         MVC   ABRPPRD,PBUYKPRD                         PRODUCT                 
         EDIT  (2,PBUYKEST),(3,ABRPEST),ALIGN=LEFT      ESTIMATE                
*                                                       DATE                    
         XC    WORK,WORK                                                        
         LA    R4,WORK             POINT TO REPLY DATE                          
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(7,0(R4))       MMMDD                  
*                                                                               
         CLI   PBDFREQ,C'M'        MONTHLY?                                     
         BNE   *+14                                                             
         XC    3(2,R4),3(R4)       DON'T WANT THE DAY                           
         SH    R4,=H'2'      BACK-UP SO LINE NUMBER WILL FOLLOW MTH             
*                                                                               
         CLI   PBUYKLIN,1          IF BUYLINE 1                                 
         BE    ABDBY39              DONE                                        
         MVI   5(R4),C'-'          ELSE SHOW ME THE BUYLINE                     
*                                                                               
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         CP    DUB,=P'100'                                                      
         BL    ABDBY30                                                          
*                                  DISPLAY 100 - 255 AS A0 - P5                 
         DP    DUB,=P'10'                                                       
         UNPK  7(1,R4),DUB+7(1)                                                 
         OI    7(R4),X'F0'                                                      
*                                                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   RF,DUB                                                           
         LA    RF,ABALPHA(RF)                                                   
         MVC   6(1,R4),0(RF)                                                    
         B     ABDBY39             DONE                                         
*                                                                               
ABDBY30  OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R4),DUB                                                      
         CLI   6(R4),C'0'                                                       
         BNE   ABDBY39                                                          
         MVC   6(2,R4),7(R4)                                                    
*                                                                               
*                                                                               
ABDBY39  DS    0H                                                               
         MVC   ABRPDAT,WORK                                                     
*                                                                               
ABDTLWX  DS    0H                                                               
         L     R7,ADBSVPT          ADDRESS OF CURRENT ADBYTBL ENTRY             
         LA    R7,ABRPLNTH(R7)                                                  
         ST    R7,ADBSVPT          ADDRESS OF NEXT ADBYTBL ENTRY                
         XIT1                                                                   
*                                                                               
ABALPHA  DC    C'ABCDEFGHIJKLMNOP',X'FF'                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R7,R8                                                         
*                                                                               
         TITLE 'PPPAY02 -OUTPUT BUY FX INFO FOR ADBUYER REPLY-ADBDTLFX'         
***********************************************************************         
*                                                                     *         
*        ADD FX NET AMOUNT TO INFO FOR ADBUYER                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
ABDTLFX  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
         L     R8,ADBWRKA          ESTABLISH ADBUYER WORK AREA                  
         USING ADBWRKD,R8                                                       
         L     R7,ADBSVPT          ADDRESS OF NEXT ADBYTBL ENTRY                
         SHI   R7,ABRPLNTH         BACK UP TO PREVIOUS ENTRY                    
         USING ADBYRPLY,R7                                                      
*                                                                               
*        CALCULATE FX NET                                                       
*                                                                               
         L     R0,GROSS            GET THE NET PAID FOR REPLY                   
         S     R0,PGROSS                                                        
*                                                                               
         CLI   GNOPT,C'G'                                                       
         BE    *+12                                                             
         A     R0,PAGYCOM                                                       
         S     R0,AGYCOM                                                        
*                                                                               
         ST    R0,FULL                                                          
*                                                                               
         MVC   ABRPFX,FULL         REPLY AMOUNT IN BINARY                       
*                                                                               
ABDTLFXX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R7,R8                                                         
*                                                                               
         TITLE 'PPPAY02 -SEE IF BUY SHOULD BE INCLUDED (ADBUYER CALL)'          
***********************************************************************         
*  IF BUY SERIAL#S WERE SENT BY ADBUYER, SKIP BUYS WHOSE SERIAL#S DO  *         
*  NOT MATCH THOSE SENT - IF SERIAL#S WERE NOT SENT, DO NOT SKIP BUYS *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
ADBTST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
         L     R8,ADBWRKA          ESTABLISH ADBUYER WORK AREA                  
         USING ADBWRKD,R8                                                       
*                                                                               
         CP    SERTBL(L'SERTBL),=P'0'  SERIAL#S SENT BY ADBUYER ?               
         BNH   KEEPBY              NO - CONTINUE PROCESSING BUY                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* SERIAL#S SENT BY ADBUYER SO SKIP ANY BUY WITHOUT A MATCHING SERIAL#           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         L     R6,ABUYEL           POINT TO 1ST ELEM IN PBUYREC                 
TSTSER   DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    SKIPBY              SERIAL# ELEM NOT FOUND                       
         CLI   0(R6),X'99'         SERIAL# ELEM ?                               
         BE    HAVESER             YES - GO TEST IT                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               NEXT ELEMENT                                 
         B     TSTSER              TEST NEXT                                    
*                                                                               
HAVESER  DS    0H                                                               
         USING PSERELEM,R6         POINTING TO X'99' SERIAL# ELEM               
         LA    R5,SERTBL           PREP FOR SERIAL# SEARCH                      
HAVSER20 DS    0H                                                               
         CLI   0(R5),X'FF'         END OF TABLE ?                               
         BE    SKIPBY              YES - SKIP BUY (NO MATCHING SERIAL#)         
         CP    0(5,R5),PSERNUM     BUY SERIAL# EQUAL TO SENT SERIAL# ?          
         BE    KEEPBY              YES - CONTINUE PROCESSING BUY                
         LA    R5,L'SERTBL(R5)     BUMP TO NEXT ENTRY                           
         B     HAVSER20            TEST NEXT                                    
KEEPBY   DS    0H                                                               
         CR    RB,RB               RETURN EQUAL CONDITION                       
         B     ADBTSTX                                                          
SKIPBY   DS    0H                                                               
         LTR   RB,RB               RETURN NOT EQ (NOT ZERO) CONDITION           
ADBTSTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R6,R8                                                         
*                                                                               
         TITLE 'PPPAY02 -CHECK REQUEST - CHCKREQ'                               
***********************************************************************         
*                                                                     *         
*        ISSUE CHECK REQUEST                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CHCKREQ  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GENOLD,RC           ESTABLISH WORKING STORAGE                    
*                                                                               
         L     R8,0(R1)            POINT TO TOTALS FOR CHECK                    
         USING PAYTOTSD,R8         ESTABLISH TOTALS FOR CHECK                   
*                                                                               
         CLC   CKDATE,=3X'FF'      SPECIAL IF SUPPRESSING CHECK                 
         BE    CRCKSUP                                                          
*                                                                               
         BRAS  RE,CALCCTX          CALCULATE CANADIAN TAX IF NEEDED             
*                                                                               
*        IF PAYING GROSS NEED TO CALCULATE NET FOR EACH                         
*              INVOICE AMOUNT                                                   
*                                                                               
         CLI   GNOPT,C'G'          SKIP IF NOT PAYING GROSS                     
         BNE   CRGRSN                                                           
*                                                                               
         BRAS  RE,DELCTX           REMOVE GST/PST FROM INVOICE AMOUNTS          
*                                                                               
*        SAVE CURRENT GROSS VALUES FOR CHECK REQUEST                            
*                                                                               
         LA    R0,PBMXIV#Q         MAX NUMBER OF INVOICES                       
         LAY   RF,BINVGRS          DISPLACEMENT TO GROSS BUCKETS                
         LA    R4,PBAMTS           INVOICE DATA                                 
         USING BAMTD,R4            ESTABLISH INVOICE DATA                       
*                                                                               
CRGRSLP  MVC   0(4,RF),BAMT        SAVE CURRENT GROSS VALUE                     
         LA    R4,BAMTL(R4)        BUMP TO NEXT INVOICE                         
         LA    RF,4(RF)            BUMP TO NEXT GROSS BUCKET                    
         JCT   R0,CRGRSLP          LOOP THROUGH INVOICES                        
*                                                                               
CRGRSDN  DS    0H                                                               
*                                                                               
         BRAS  RE,CALCNET          CALCULATE NET FROM GROSS AMOUNT              
*                                                                               
         BRAS  RE,RESCTX           RESTORE GST/PST TO INVOICE AMOUNTS           
*                                                                               
         B     CRGNOPTX                                                         
*                                                                               
CRGRSN   DS    0H                                                               
*                                                                               
*        PAYING NET                                                             
*                                                                               
         BRAS  RE,DELCTX           REMOVE GST/PST FROM INVOICE AMOUNTS          
*                                                                               
         BRAS  RE,CALCGRS          GET GROSS FOR INVOICE AMOUNTS                
*                                  SAVED IN BINVGRS                             
*                                                                               
         BRAS  RE,RESCTX           RESTORE GST/PST TO INVOICE AMOUNTS           
*                                                                               
CRGNOPTX DS    0H                                                               
*                                                                               
         LAY   R6,BINVGRS          INVOICE GROSS BUCKETS                        
*                                                                               
         LAY   R3,REQHDR           ESTABLISH REQUEST RECORD                     
         USING REQHDR,R3                                                        
*                                                                               
         LA    R4,PBAMTS           ESTABLISH INVOICE AMOUNTS AREA               
         USING BAMTD,R4                                                         
*                                                                               
         LA    R5,PBMXIV#Q         MAX NUMBER OF INVOICES                       
         LA    R2,PAYINV1H         FIRST INVOICE FIELD ON SCREEN                
         L     R7,PTOTCD           CASH DISCOUNT                                
         J     CR00A100                                                         
*                                                                               
CR00     CHI   R5,(PBMXIV#Q-PBMXIVSQ)                                           
         JNH   *+12                                                             
         BAS   R9,CBUMPFLD         BUMP TO NEXT FIELD                           
         JNE   CR00A100                                                         
         TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    CHCKREQX                                                         
*                                                                               
         CHI   R5,(PBMXIV#Q-PBMXIVSQ)                                           
         JL    CR00A050                                                         
         ICM   R2,15,AXLKINV#      POINT ADDITIONAL INVOICES ARRAY              
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE ADDRESS                            
         CLI   AXLKINV#,LQ_TSINQ   ONE VALUE?                                   
         JNE   *+12                                                             
         LA    R2,6(R2)            POINT TO START OF INVOICES                   
         J     CR00A100                                                         
         LA    R2,8(R2)            POINT TO START OF INVOICES                   
         J     CR00A100                                                         
*                                                                               
CR00A050 AHI   R2,INVTOTLQ         POINT TO NEXT INVOICE IN ARRAY               
*                                                                               
CR00A100 OC    BAMT,BAMT           DONE IF NO INVOICE AMOUNT                    
         JNZ   CR00A200                                                         
*                                                                               
         CLI   BAMTINP,C'C'        AND NO DATA ENTERED ON LINE                  
         JNE   CHCKREQX                                                         
*                                                                               
CR00A200 BRAS  RE,CREQ             FORMAT BASIC REQUEST                         
*                                                                               
         MVC   QSEQNUM,PSTATSQ     SEQUENCE NUMBER                              
*                                                                               
         CLC   PROGPROF+14(2),=C'  '    ACC AGY OVERRIDE                        
         JNH   CR00A500                                                         
         CLC   PROGPROF+14(2),=C'00'                                            
         JE    CR00A500                                                         
*                                                                               
         MVC   QAGYCODE,PROGPROF+14      COL 9 + 10                             
*                                                                               
CR00A500 MVC   QOFFICE,SVCLOFF                                                  
*                                                                               
         TM    SPOFFSW,X'40'       IF SPECIAL OFFICE BEING USED                 
         BNO   CR00B                                                            
*                                                                               
         MVC   QOFFICE(1),SPOFF       USE SPECIAL OFFICE CODE                   
         MVC   QOFFICE+1(1),SPOFF2    2ND CHARACTER                             
         OC    QOFFICE,=C'  '         JUST IN CASE                              
*                                                                               
CR00B    EQU   *                                                                
*                                                                               
         CLC   WKSTART(6),WKEND    SKIP IF START/END DTES THE SAME              
         BE    CR00C                                                            
*                                                                               
         CLC   WKSTART+4(2),=C'00' IF MONTH PAY RANGE                           
         BNE   *+10                                                             
CR00C    MVC   QEND(6),BLANKS         CLEAR END DATE                            
*                                                                               
         CLI   BLIN,0              SKIP IF NO LINE NUMBER                       
         BE    CR00D                                                            
*                                                                               
         MVC   QEND,BLANKS         CLEAR END DATE                               
*                                                                               
         SR    R0,R0               STORE LINE # IN END DATE                     
         IC    R0,BLIN                                                          
         CVD   R0,DUB                                                           
*                                                                               
         CP    DUB,=P'100'                                                      
         BL    CR00C8                                                           
*                                                                               
         DP    DUB,=P'10'          CHANGE LINE NUMBERS 100-255                  
         OI    DUB+7,X'0F'         TO A0 TO P5                                  
         UNPK  QEND+1(1),DUB+7(1)                                               
         ZAP   DUB,DUB(6)                                                       
         SP    DUB,=P'10'                                                       
         CVB   R1,DUB                                                           
         LA    R1,ALPHTAB(R1)                                                   
         MVC   QEND(1),0(R1)                                                    
*                                                                               
         B     CR00D                                                            
*                                                                               
ALPHTAB  DC    C'ABCDEFGHIJKLMNOP'                                              
*                                                                               
CR00C8   OI    DUB+7,X'0F'                                                      
         UNPK  QEND(2),DUB         LINE NO IN QEND                              
*                                                                               
CR00D    EQU   *                                                                
*                                                                               
         MVI   REQNUMB,30          SET CHECK REQUEST ID                         
         MVC   QCODE(2),=C'30'                                                  
*                                                                               
         XC    QGST,QGST                CLEAR GST                               
         MVC   QPAYDATE(3),BTODAY       PAID TODAY                              
         MVC   QPAYDATE+3(3),CKDATE     CHECK CONTROL DATE                      
*                                                                               
         OC    CKDATE,CKDATE       MUST HAVE CHECK CONTROL DATE                 
         BNZ   *+6                                                              
         DC    H'0'                 TRAP                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(3,INVDATE),(2,QINVDTE) COMPRESS DATE               
*                                                                               
         OC    PREP,PREP           IF PAYING REP PRESENT                        
         BZ    *+10                                                             
         MVC   QREP,PREP              SET IN REQUEST                            
*                                                                               
         MVC   QEST,BEST           SET ESTIMATE IN REQUEST                      
*                                                                               
CR00E    DS    0H                                                               
*                                                                               
         CHI   R5,(PBMXIV#Q-PBMXIVSQ)                                           
         JNH   *+14                                                             
         MVC   QINV,8(R2)          SET INVOICE NUMBER FROM SCREEN               
         J     *+10                                                             
         MVC   QINV(INVNUMLQ),0(R2)                                             
*                                                                               
         LA    R1,L'QINV           LENGTH OF INVOICE                            
         LA    RF,QINV                                                          
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
*                                                                               
         MVC   QTYP,BAMTTYPE       SET CHECK REQUEST TYPE                       
         MVC   QPAY,BAMT           BINARY CHECK AMOUNT                          
*                                                                               
         XC    SWLAST,SWLAST       INIT LAST INVOICE SWITCH                     
*                                                                               
         CHI   R5,1                SET SWITCH IF LAST INVOICE                   
         BE    CR01A                                                            
*                                                                               
         OC    BAMT+BAMTL,BAMT+BAMTL     OR NEXT INVOICE IS ZERO                
         BNE   CR01AA                                                           
*                                                                               
         CLI   BAMTINP+BAMTL,C'C'        AND NO ENTRY ON LINE                   
         BE    CR01AA                                                           
*                                                                               
CR01A    MVI   SWLAST,C'Y'                                                      
*                                                                               
CR01AA   CLI   NCDSW,C'N'          SKIP IF PAYING WITH CD                       
         BNE   CR01B                                                            
*                                                                               
         XC    QCD,QCD                QLCD AND QCD ARE THE SAME                 
         MVI   QCDIND,C'C'            VALUE IN QCDIND DETERMINES                
*                                                                               
         B     CR03                                                             
*                                                                               
CR01B    CLI   SWLAST,C'Y'         IF LAST INVOICE                              
         BNE   CR01C                                                            
*                                                                               
         LR    R9,R7                  REMAINING CASH DISCOUNT                   
         SR    R7,R7                                                            
*                                                                               
         B     CR02                                                             
**                                                                              
**  NOTE IF THERE IS ANY CD LEFT IT WILL BE ON THE LAST                         
**  INVOICE EVEN IF THAT INVOICE WAS "XCD" (EXCLUDE CD)                         
**  ALSO IF THE ONLY INVOICE IS "XCD" IT WILL STILL GET ANY CD                  
**                                                                              
*                                  CALCULATE CD FOR REQUEST                     
*                                                                               
*                                       SUBTRACT FROM TOTAL CD                  
*                                       ON LAST INVOICE MOVE REMAINING          
*                                            TOTAL CD TO REQUEST                
CR01C    DS    0H                                                               
*                                                                               
         CLI   BAMTXCD,C'X'        MEANS EXCLUDE CD FROM THIS INVOIVE           
         BNE   CR01C5                                                           
*                                                                               
         SR    R9,R9               NO CD FOR THIS BAMT                          
         B     CR02                                                             
*                                                                               
CR01C5   DS    0H                                                               
*                                                                               
         ZAP   DUB,SVCD            TO HALF WORD ALIGN                           
         CVB   RF,DUB                ! AND CONVERT PS TO BINARY                 
         STCM  RF,3,HALF             ! CD PER CENT                              
*                                                                               
         L     R9,BAMT             GET INVOICE AMT                              
*                                                                               
         TM    FXSW,X'08'          SKIP IF FX CHECK REQUEST                     
         BO    CR03PSTD                                                         
*                                                                               
         OC    TOTGSTP,TOTGSTP          PAYABLE GST               L01           
         BZ    CR03AX                   NONE TO BE ALLOCATED     L01            
*                                                                               
         LR    RF,R4         MUST CALCULATE DISPLACEMENT INTO GSTAMTS           
         LA    RE,PBAMTS                                           L01          
         SR    RF,RE                                              L01           
         SRL   RF,1                DIV  X 2 (BAMTS ARE 8 LONG)    L01           
         LA    RE,GSTAMTS(RF)                                     L01           
         S     R9,0(RE)        REMOVE GST FROM BAMT                L01          
*                                                                               
CR03AX   DS    0H                                                               
*                                                                               
*        REMOVE ANY PST FROM BAMT                                               
*                                                                               
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
         LA    R0,10               10 PROVINCES                                 
*                                                                               
CR03PSTL DS    0H                                                               
*                                                                               
         OC    PROV,PROV           SKIP IF NO PROVINCE                          
         BZ    CR03PSTC                                                         
         OC    TOTPSTP,TOTPSTP     SKIP IF NO PST FOR PROVINCE                  
         BZ    CR03PSTC                                                         
*                                                                               
         LR    R1,R4         MUST CALCULATE DISPLACEMENT INTO PSTAMTS           
         LA    RE,PBAMTS                                          L01           
         SR    R1,RE                                              L01           
         SRL   R1,1                DIV  X 2                       L01           
         LA    RE,PSTAMTS(R1)                                     L01           
         S     R9,0(RE)        REMOVE PST FROM BAMT                L01          
*                                                                               
CR03PSTC DS    0H                                                               
*                                                                               
         LA    RF,PSTBCKSL(RF)     NEXT PROVINCE                                
         BCT   R0,CR03PSTL                                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
CR03PSTD DS    0H                                                               
*                                                                               
         SR    RE,RE                                                            
         LR    RE,R9               CD TO WORK REGISTER                          
*                                                                               
         MH    RE,HALF             BAMT(-GST-PST) * C.D.                        
         SRDA  RE,32                                                            
         AHI   RF,50                                                            
         D     RE,=F'1000'         R9 CONTAINS CD FOR BAMT                      
*                                                                               
         LR    R9,RF               RESTORE CD                                   
*                                                                               
         CR    R7,R9               R7 CONTAINS TOTAL CD                         
         BL    *+10                COMPARE TOTAL CD TO BAMT CD                  
         SR    R7,R9               REDUCE  TOTAL CD BY BAMT CD                  
         B     CR02                                                             
*                                                                               
         LR    R9,R7               PUT IN REMAINDER                             
         SR    R7,R7               CLEAR REMAINDER // NO MORE CD LEFT           
*                                  CASH DISCOUNT                                
CR02     LR    R0,R9                                                            
         CVD   R0,DUB                                                           
         XC    QCD,QCD             QLCD AND QCD ARE THE SAME      L01           
         MVI   QCDIND,0                                           L01           
*                                  CASH DISCOUNT                                
         CLI   NCDSW,C'L'          SEE IF CD LOST                               
         BE    CR02B                                                            
*                                  CASH DISCOUNT                                
         STCM  R0,15,QCD                                                        
*                                                                               
         MVI   QCDIND,C'C'         CD INDICATOR                  L01            
*                                  CASH DISCOUNT                                
         B     CR03                                                             
*                                                                               
CR02B    STCM  R0,15,QLCD                                                       
         MVI   QCDIND,C'L'         LOST CD INDICATOR             L01            
         B     CR03                                                             
*                                  WRITE REQUEST                                
CR03     DS    0H                                                               
*                                                                               
*        SAVE CD FOR CLRST RECORD                                               
*                                                                               
         LR    RF,R4         MUST CALCULATE DISPLACEMENT INTO BCDS              
         LA    RE,PBAMTS                                                        
         SR    RF,RE                                                            
         SRL   RF,1                DIV  X 2                                     
         LA    RE,BCDS(RF)                                                      
*                                                                               
         MVC   0(4,RE),QCD         SAVE CD                                      
*                                                                               
         MVI   Q2GNIND,0           CLEAR INDICATOR                              
*                                                                               
         TM    FXSW,X'08'          SKIP IF FX CHECK REQUEST                     
         BO    CR03A1                                                           
*                                                                               
         OC    TOTGSTP,TOTGSTP          PAYABLE GST               L01           
         BZ    CR03A                    NONE TO BE ALLOCATED     L01            
*                                                                               
         LR    RF,R4         MUST CALCULATE DISPLACEMENT INTO GSTAMTS           
         LA    RE,PBAMTS                                           L01          
         SR    RF,RE                                              L01           
         SRL   RF,1                DIV  X 2                       L01           
         LA    RE,GSTAMTS(RF)                                     L01           
         MVC   QGST,0(RE)                                         L01           
         MVC   QGSTCOD,THISGSTC    GST CODE                       L01           
*                                                                               
CR03A    DS    0H                                                               
*                                                                               
         BAS   RE,SETPST           SET PST IN REQUEST                           
*                                                                               
CR03A1   DS    0H                                                               
*                                                                               
*                                                                               
* SINGLY OPTION IS OVERRIDEN WHEN XSIN IS USED                                  
* EXTRACT BILLABLE DATE IF IT'S STILL AVAILABLE                                 
*                                                                               
         LR    RF,R5               SAVE R5 (INVOICE POINTER)                    
*                                                                               
         OC    SVBLBDT,SVBLBDT     HAVE BILLABLE DATE?                          
         JNZ   CR03A1_X                                                         
         CLI   MLTOPTSW,C'X'       XSIN OPTION USED?                            
         JNE   CR03A1_X                                                         
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         CLI   0(R5),X'20'         HAVE MAIN BUY ELEMENT?                       
         JNE   CR03A1_X                                                         
         MVI   ELCODE,X'99'        LOOK FOR BUY SERIAL# ELEMENT                 
         BRAS  RE,NEXTEL                                                        
         JNE   CR03A1_X            SKIP IF NOT A VALID BUY RECORD               
         L     R5,ABUYEL           POINT TO START OF BUY ELEMENTS               
         USING PBDELEM,R5                                                       
         MVC   SVBLBDT,PBDBDATE                                                 
         DROP  R5                                                               
*                                                                               
CR03A1_X LR    R5,RF               RESTORE R5 (INVOICE POINTER)                 
*                                                                               
         LAY   RF,SVPID                                                         
         MVC   Q2PID,0(RF)         SET PAYER'S PID                              
         MVC   Q2SEQ2,PSTATSQ2     SET SECONDARY SEQUENCE NUMBER                
*                                                                               
* BILLABLE DATE IS ALWAYS PRESENT FOR SINGLY                                    
*                                                                               
         MVC   Q2BLBDT,SVBLBDT     SET BUY'S BILLABLE                           
*                                                                               
         L     RF,0(R6)            GET INVOICE GROSS                            
         CVD   RF,DUB              CVD                                          
         ZAP   Q2GRS,DUB           SET INVOICE GROSS                            
*                                                                               
         CLI   GNOPT,C'N'          IF PAYING NET                                
         BNE   *+8                                                              
         OI    Q2GNIND,X'80'          THEN GROSS WAS CALCULATED                 
*                                                                               
         CHI   R5,(PBMXIV#Q-PBMXIVSQ)                                           
         JNH   *+8                                                              
         BAS   R9,CBUMPFD2         POINT TO COMMENT FIELD                       
*                                                                               
*                                  COMMENTS                                     
*                                                                               
         SR    R0,R0                                                            
*                                                                               
         CLI   MLTREQSW,C'Y'       MULTI REQHDRS OPTION ?                       
         BNE   *+12                NO                                           
         CLI   SCRCOMSW,C'Y'       SCREEN COMMENTS ENTERED ?                    
         BNE   *+8                 NO - NO "EXTRA COMMENTS"                     
*                                                                               
         ICM   R0,1,BAMTCOM#       NUMBER OF EXTRA COMMENTS                     
*                                                                               
         LA    R1,QCOMM1           START OF COMMENT STORAGE AREA                
*                                                                               
         CHI   R5,(PBMXIV#Q-PBMXIVSQ)                                           
         JNH   CR0400                                                           
*                                                                               
CRCOMLP  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          COMMENT LENGTH                               
         BZ    CRCOMCN             SKIP IF NO COMMMENT ENTERED                  
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),8(R2)       MOVE COMMENT TO REQUEST                      
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R1),BLANKS      FORCE UPPERCASE                              
*                                                                               
         LA    R1,L'QCOMM1(R1)     BUMP TO NEXT COMMENT AREA                    
*                                                                               
CRCOMCN  DS    0H                                                               
*                                                                               
         LTR   R0,R0               DONE IF NO MORE EXTRA COMMENTS               
         BZ    CRCOMDN                                                          
*                                                                               
         BCTR  R5,0                DECREMENT INVOICE COUNTER                    
         BCTR  R0,0                DECREMENT COMMENT COUNTER                    
*                                                                               
         BAS   R9,CBUMPFD2         BUMP TO NEXT COMMENT FIELD                   
         BAS   R9,CBUMPFLD                                                      
*                                                                               
         J     CRCOMLP                                                          
*                                                                               
CRCOMDN  J     CR0500                                                           
*                                                                               
CR0400   TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    CR0500                                                           
         CHI   R5,(PBMXIV#Q-PBMXIVSQ)                                           
         JH    CR0500                                                           
         MVC   0(INVSRCLQ,R1),(INVNUMLQ+INVAMTLQ)(R2)                           
         OC    0(L'QCOMM1,R1),BLANKS                                            
*                                                                               
CR0500   BAS   RE,CWTREQ           WRITE REQUEST                                
*                                                                               
         LA    R4,BAMTL(R4)        NEXT INVOICE AREA                            
         LA    R6,4(R6)            NEXT INVOICE GROSS AMOUNT                    
         JCT   R5,CR00                                                          
*                                                                               
         B     CHCKREQX                                                         
*                                                                               
*        SUPPRESS CHECK                                                         
*                                                                               
CRCKSUP  DS    0H                                                               
*                                                                               
         XC    PAYCK,PAYCK                                                      
         MVC   PAYCK(16),=C'CHECK SUPPRESSED'                                   
         FOUT  PAYCKH                                                           
*                                                                               
         B     CHCKREQX                                                         
*                                                                               
CHCKREQX DS    0H                                                               
*                                  ADD CD TO CLRST RECORD                       
         XIT1                                                                   
*                                                                               
FIXUCHAR CLI   0(RF),X'4A'         UNPRINTABLE ARACTERS?                        
         JNL   *+12                                                             
FXUCH10  MVI   0(RF),C' '          GET RID OF UNPRINTABLE CHARACTERS            
         J     FXUCH30                                                          
         CLI   0(RF),X'51'         BYPASS ASCII CHARACTERS THAT                 
         JL    FXUCH30             FAILED EBCDIC TRANSLATION                    
         CLI   0(RF),X'59'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'62'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'69'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'70'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'78'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'80'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'80'                                                      
         JE    FXUCH10                                                          
         CLI   0(RF),X'8A'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'90'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'9A'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'A1'                                                      
         JNH   FXUCH10                                                          
         CLI   0(RF),X'AA'                                                      
         JL    FXUCH30                                                          
         CLI   0(RF),X'BF'                                                      
         JNH   FXUCH10                                                          
*                                                                               
FXUCH30  AHI   RF,1                BUMP TO NEXT CHAR IN COMMENT                 
         JCT   R1,FIXUCHAR                                                      
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
         TITLE 'FORMAT BASIC CHECK REQUEST - CREQ'                              
***********************************************************************         
*                                                                     *         
*        FORMAT BASIC CHECK REQUEST                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CREQ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R2,REQHDR           ESTABLISH REQUEST RECORD                     
         USING REQHDR,R2                                                        
*                                                                               
         XC    REQHDR(26),REQHDR                                                
*                                                                               
         LA    R1,QIAREA           INIT 5 CARDS WORTH                           
         LA    R0,10               40*10 = 5*80                                 
         MVC   0(40,R1),BLANKS     BLANKS IS ONLY 50 LONG                       
         LA    R1,40(R1)                                                        
         JCT   R0,*-10                                                          
*                                                                               
         MVI   QCODE,C' '                                                       
         MVC   QCODE+1(79),QCODE                                                
         MVC   QAGY(2),AGYALPHA                                                 
         MVC   QMED(1),PAYMD                                                    
         MVC   QCLT(3),PAYCL                                                    
         MVC   QPRD(3),SAVPR                                                    
         MVC   QSTART(12),WKSTART                                               
*                                                                               
         GOTO1 VPUBEDIT,DMCB,BPUB,(C'Q',WORK)                                   
*                                                                               
         CLI   BPUB+4,X'FF'                                                     
         BNE   *+10                                                             
         MVC   WORK+8(3),=C'ZZZ'                                                
*                                                                               
         MVC   QPUB(11),WORK                                                    
*                                                                               
CREQX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         TITLE 'WRITE REQUESTS - CWTREQ'                                        
***********************************************************************         
*                                                                     *         
*        WRITE REQUESTS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CWTREQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R2,REQHDR           ESTABLISH REQUEST RECORD                     
         USING REQHDR,R2                                                        
*                                                                               
* COUNT NUMBER OF COMMENTS AND PASS IN ZCONT                                    
*                                                                               
         MVI   BYTE2,PBMXIV#Q      DEFAULT TO MAX NUMBER OF INVOICES            
         CLI   MLTREQSW,C'Y'       MULTI-HDR OUTPUT REQUEST ?                   
         BNE   CWTREQ05            NO                                           
         CLI   SCRCOMSW,C'Y'       SCREEN COMMENTS ENTERED ?                    
         BE    CWTREQ05            YES - USE THEM, NOT SRC COMMS                
         MVC   QCOMM1(200),SRCOMM1                                              
         LA    R0,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
         STC   R0,BYTE2                                                         
         J     CWTREQ06                                                         
*                                                                               
CWTREQ05 LA    R0,PBMXIV#Q         MAX NUMBER OF SUPPORTED INVOICES             
CWTREQ06 LA    R1,QCOMM1           POINT TO SAVED INVOICE COMMENTS              
*                                                                               
CWTREQ11 CLC   0(40,R1),BLANKS     HAVE COMMENT?                                
         BE    CWTREQ12                                                         
         LA    R1,40(R1)                                                        
         BCT   R0,CWTREQ11                                                      
*                                                                               
CWTREQ12 LLC   R1,BYTE2                                                         
         SR    R1,R0               GET NUMBER OF COMMENTS                       
         STC   R1,QCOM                                                          
         OI    QCOM,X'20'          INDICATE NEW REQUEST FORMAT                  
*                                                                               
         LA    R1,1(R1)            SET TO ROUND UP                              
         SRL   R1,1                GIVES NUMBER OF 80 BYTE COMMENTS             
         LA    R1,2(R1)            ADD 2 FOR 160 BYTES ALWAYS PRESENT           
         BCTR  R1,0                THEN PASS N'CARDS - 1                        
         SLL   R1,4                SHIFT TO LEFT NIBBLE                         
         STC   R1,REQFLAG          SET NUMBER OF CARDS                          
*                                                                               
         OC    QUESTR(12),BLANKS                                                
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',REQHDR,REQHDR               
*                                                                               
         TM    8(R1),X'FF'                                                      
         BZ    *+12                                                             
         MVI   8(R1),X'FF'                                                      
         BAS   RE,CTSTDMCB                                                      
*                                                                               
         XIT1                                                                   
*                                                                               
CBUMPFD2 SR    RE,RE               WAS R0                                       
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
CBUMPFLD SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),9                                                          
         BCR   8,R9                                                             
         CLI   0(R2),0                                                          
         BR    R9                                                               
         SPACE 2                                                                
CTSTDMCB DS    0H                                                               
         TM    DMCB+8,X'FD'        CHECK EVERYTHING BUT DELETED                 
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE ' PPPAY02 - CALCULATE CANADIAN TAXES - CALCCTX'                  
*=========================================================*                     
*                                                                     *         
* SUBROUTINE SPLITS INVOICE AMOUNTS INTO INVOICE DOLLARS  *                     
* AND TAX AMOUNTS.                                        *                     
* THE TOTAL DOLLARS ARE GROSS OR NET BY AGENCY OPTION     *                     
*                                                                     *         
*=========================================================*                     
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
CALCCTX  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GENOLD,RC           ESTABLISH WORKING STORAGE                    
         USING PAYTOTSD,R8         ESTABLISH TOTALS                             
*                                                                               
*        CALCULATE GST                                                          
*                                                                               
*=========================================================*                     
* SUBROUTINE SPLITS INVOICE AMOUNTS INTO INVOICE DOLLARS  *                     
* AND GST AMOUNTS. GST FOR EACH INVOICE IS CALCULATED     *                     
* BY   INVAMT*TOTAL_GST/TOTAL DOLLARS                     *                     
*=========================================================*                     
*                                                                               
         TM    FXSW,X'08'          SKIP FOR FX REQUESTS                         
         BO    CTXPSTX                                                          
*                                                                               
         OC    TOTGSTP,TOTGSTP     SKIP IF NO GST INVOLVED                      
         BZ    CTXGSTX                                                          
*                                                                               
         L     R1,PTOTG            TOTAL GROSS                                  
*                                                                               
         CLI   GNOPT,C'G'          SKIP IF INVOICE DOLLARS ARE GROSS            
         BE    CTXGST10                                            L01          
*                                                                               
         L     R1,PTOTNLCD         NET/NET                                      
         A     R1,PTOTCD           TRUE NET                        L01          
*                                                                               
CTXGST10 DS    0H                                                               
*                                                                               
         A     R1,TOTGSTP          ADD TOTAL GST TO TOTAL INVOICE               
*                                                                               
*        ADD IN TOTAL PST DOLLARS                                               
*                                                                               
         LAY   RF,PSTBCKS                                                       
         USING PSTBCKS,RF                                                       
*                                                                               
         LA    R0,10               TEN PROVINCES                                
*                                                                               
         A     R1,TOTPSTP          ADD IN ALL THE PST                           
         LA    RF,PSTAREAL(RF)                                                  
         BCT   R0,*-8                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
*        R1 HAS TOTAL OF ALL MONIES TO BE PAID                                  
*              INVOICE AMOUNTS PLUS CANADIAN TAXES                              
*                                                                               
*                                                                               
*        ALLOCATE GST TO INVOICES BASED ON                                      
*              PERCENTAGE INVOICE AMOUNT REPRESENTS OF TOTAL                    
*              OF ALL INVOICES                                                  
*                                                                               
         LA    R5,GSTAMTS          POINT TO GST BUCKETS                         
         LA    R0,PBMXIV#Q         MAX INVOICES                                 
         LA    R9,PBAMTS           POINT TO INVOICE BUCKETS                     
         USING BAMTD,R9            ESTABLISH INVOICE AMOUNTS                    
*                                                                               
CTXGSTLP DS    0H                                                               
*                                                                               
         OC    BAMT,BAMT           IF NO $ BUMP TO NEXT                         
         BZ    CTXGSTCN                                                         
*                                                                               
         L     RF,BAMT             GET INDIVIDUAL INVOICE DOLLARS               
*                                                                               
         AR    RF,RF               X 2                                          
*                                                                               
         M     RE,TOTGSTP          X TOTAL GST                                  
*                                                                               
         DR    RE,R1               DIVIDE BY TOTAL INVOICE DOLLARS              
*                                                                               
         LTR   RF,RF               ANY REMAINDER                                
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
*                                                                               
         SRA   RF,1                DIVIDE BY 2                                  
*                                                                               
         ST    RF,0(R5)            SAVE THE GST AMOUNT                          
*                                                                               
CTXGSTCN DS    0H                                                               
*                                                                               
         LA    R9,BAMTL(R9)        BUMP TO NEXT INVOICE BUCKET                  
         LA    R5,4(R5)            BUMP TO NEXT GST BUCKET                      
         BCT   R0,CTXGSTLP                                                      
*                                                                               
CTXGSTDN DS    0H                                                               
*                                                                               
*========================================================*                      
* NOW ADD THE GST AMOUNTS AND CHECK SUM TO TOTAL GST     *                      
* IF THEY ARE NOT EQUAL ADJUST THE LARGEST GST AMOUNT    *                      
* BY THE DIFFERENCE                                      *                      
*========================================================*                      
*                                                                               
         LA    R0,PBMXIV#Q         NUMBER OF INVOICES                           
         LA    RF,GSTAMTS          INVOICE GST BUCKETS                          
         LR    R1,RF               SET POINTER TO BIGGEST                       
         SR    R5,R5               CLEAR ACCUM                                  
*                                                                               
CTXGSUML DS    0H                                                               
*                                                                               
         A     R5,0(RF)            ADD INDIVIDUAL GST TO SUM                    
*                                                                               
         CLC   0(4,RF),0(R1)       DETERMINE LARGEST GST AMOUNT                 
         BNH   *+6                                                              
         LR    R1,RF               SAVE POINTER TO LARGER                       
*                                                                               
CTXGSUMC DS    0H                                                               
*                                                                               
         LA    RF,4(RF)            BUMP TO NEXT GST BUCKET                      
         BCT   R0,CTXGSUML                                                      
*                                                                               
CTXGSUMD DS    0H                                                               
*                                                                               
         L     RE,TOTGSTP          LOAD TOTGST                                  
*                                                                               
         SR    RE,R5               GET THE DIFFERENCE                           
*                                                                               
         L     RF,0(R1)            ADJUST THE LARGEST GST                       
         AR    RF,RE               AND STORE IT BACK                            
         ST    RF,0(R1)                                                         
*                                                                               
CTXGSTX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* SUBROUTINE SPLITS INVOICE AMOUNTS INTO PUB DOLLARS      *                     
* AND PST AMOUNTS. PST FOR EACH INVOICE IS CALCULATED     *                     
* BY   INVAMT*TOTAL_PST/TOTAL DOLLARS                     *                     
* THE TOTAL DOLLARS ARE GROSS OR NET BY AGENCY OPTION     *                     
*=========================================================*                     
         SPACE 2                                                                
         OC    TOTPSTP,TOTPSTP     SKIP IF NO PST TAX                           
         BZ    CTXPSTX                                                          
*                                                                               
         XC    DUB,DUB                                                          
         XC    WORK,WORK                                                        
*                                                                               
*        FIND TOTAL PST FOR ALL INVOICES AND PROVINCES                          
*                                                                               
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         LA    R0,10               10 PROVINCES                                 
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
*                                                                               
         A     R2,TOTPSTP          ACCUMULATE TOTAL PST                         
         A     R3,TOT$BSP          ACCUMULATE TOTAL PST DOLLAR BASIS            
         LA    RF,PSTBCKSL(RF)     FOR ALL PROVINCES TOGETHER                   
         BCT   R0,*-12                                                          
*                                                                               
         STM   R2,R3,DUB           SAVE TOTAL PST FOR ALL INVOICES              
*                                                                               
         DROP  RF                                                               
*                                                                               
*        FIND FILE TOTAL PAYABLE AMOUNT                                         
*                                                                               
         L     R6,PTOTG            DEFAULT TO TOTAL GROSS FOR ALL INVS          
*                                                                               
         CLI   GNOPT,C'G'                                                       
         BE    *+12                                                             
         L     R6,PTOTNLCD         CHECK PROFILE FOR NET                        
         A     R6,PTOTCD           NET=NLCD+CD R6                               
*                                                                               
         A     R6,TOTGSTP          ADD IN TOTAL GST                             
         A     R6,DUB              ADD IN TOTAL PST                             
*                                                                               
*        ALLOCATE PST TO INVOICES                                               
*                                                                               
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
         LA    R5,10               10 PROVINCES                                 
*                                                                               
CTXPSTLP DS    0H                                                               
*                                                                               
         OC    PROV,PROV           SKIP IF NO PROVINCE                          
         BZ    CTXPSTCN                                                         
         OC    TOTPSTP,TOTPSTP     SKIP IF NO PST FOR PROVINCE                  
         BZ    CTXPSTCN                                                         
*                                                                               
         LA    R9,PBAMTS           POINT TO ENTERED INVOICE AMOUNTS             
         USING BAMTD,R9            ESTABLISH INVOICE AMOUNTS                    
         LA    R2,PSTAMTS          POINT TO PST BY INVOICE BUCKETS              
         LA    R3,PST$BSS          POINT TO PST $'S BY INVOICE BUCKETS          
         LA    R4,PBMXIV#Q         FIVE INVOICES                                
*                                                                               
CTXPAMTL DS    0H                                                               
*                                                                               
         ICM   R1,15,BAMT          INVOICE AMOUNT                               
         BZ    CTXPAMTC              SKIP IF NONE                               
*                                                                               
         AR    R1,R1               X 2                                          
         M     R0,TOTPSTP          TIMES TOTAL PST FOR PROVINCE                 
*                                                                               
         DR    R0,R6               DIVIDE BY FILE TOTAL                         
*                                                                               
         LTR   R1,R1               ROUND IF NECESSARY                           
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
*                                                                               
         SRA   R1,1                                                             
*                                                                               
         ST    R1,0(R2)            PST FOR THIS INV/PROVINCE                    
*                                                                               
         A     R1,WORK                                                          
         ST    R1,WORK             TOTAL PST ALLOCATED                          
*                                                                               
         ICM   R1,15,BAMT          INVOICE AMOUNT                               
         BZ    CTXPAMTC              SKIP IF NONE                               
*                                                                               
         AR    R1,R1               X 2                                          
         M     R0,TOT$BSP          TIMES TOTAL PST $'S FOR PROVINCE             
*                                                                               
         DR    R0,R6               DIVIDE BY FILE TOTAL                         
*                                                                               
         LTR   R1,R1               ROUND IF NECESSARY                           
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
*                                                                               
         SRA   R1,1                                                             
*                                                                               
         ST    R1,0(R3)            PST $'S FOR THIS INV/PROVINCE                
*                                                                               
         A     R1,WORK+4                                                        
         ST    R1,WORK+4           TOTAL PST $'S ALLOCATED                      
*                                                                               
CTXPAMTC DS    0H                                                               
*                                                                               
         LA    R9,BAMTL(R9)        NEXT INVOICE TOTAL                           
         LA    R2,4(R2)            NEXT INVOICE PST                             
         LA    R3,4(R3)            NEXT INVOICE PST $'S                         
*                                                                               
         BCT   R4,CTXPAMTL                                                      
*                                                                               
CTXPAMTD DS    0H                                                               
*                                                                               
CTXPSTCN DS    0H                                                               
*                                                                               
         LA    RF,PSTBCKSL(RF)    NEXT PROVINCE                                 
         BCT   R5,CTXPSTLP                                                      
*                                                                               
CTXPSTDN DS    0H                                                               
*                                                                               
         DROP  RF,R9                                                            
*                                                                               
*        NOW MAKE SURE THAT TOTAL PST = ACCUMULATED PST OF ALL INVOICES         
*        DUB  = TOTAL PST                                                       
*        WORK = PST AMOUNT WRITTEN TO CLEARANCE                                 
*                                                                               
         CLC   DUB(4),WORK         DONE IF ALL PST ALLOCATED                    
         BNE   *+10                                                             
         CLC   DUB+4(4),WORK+4     AND  IF ALL PST $'S ALLOCATED                
         BE    CTXPSTX                                                          
*                                                                               
*        FIND INVOICE WITH LARGEST PST                                          
*                                                                               
         SR    R3,R3               CLEAR MAX PST AMOUNT                         
*                                                                               
         LA    R5,10               10 PROVINCES                                 
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
*                                                                               
         LA    R1,PSTAMTS          DEFAULT TO FIRST PST BUCKET                  
         LA    R4,PBAMTS           DEFAULT TO FIRST INVOICE AMOUNT              
*                                  DEFAULTS USED ONLY IF ALL PST IS 0           
*                                  THIS CASE SHOULD NEVER ARISE                 
*                                                                               
CPSTPS2L DS    0H                                                               
*                                                                               
         LA    R9,PBAMTS           INVOICE AMOUNTS                              
         USING BAMTD,R9            ESTABLISH INVOICE AMOUNTS                    
         LA    R2,PSTAMTS          PST AMOUNTS FOR INVOICE                      
         LA    R0,PBMXIV#Q         FIVE INVOICES                                
*                                                                               
CPSTAM2L DS    0H                                                               
*                                                                               
         L     RE,0(R2)            PST FOR NEXT PROVINCE/INVOICE                
*                                                                               
         CLI   BAMTTYPE,C'1'       CR AND CK AMOUNTS ARE NEGATIVE               
         BE    *+6                                                              
         LNR   RE,RE                  GET ABSOLUTE VALUE                        
*                                                                               
         CR    RE,R3               SKIP IF NOT NEW MAXIMUM PST                  
         BL    CPSTPS2C                                                         
*                                                                               
         LR    R1,R2               SAVE PST BUCKET POINTER                      
         LR    R3,RE               SAVE NEW MAXIMUM PST                         
         LR    R4,R9               SAVE INVOICE POINTER                         
*                                                                               
CPSTAM2C DS    0H                                                               
*                                                                               
         LA    R9,BAMTL(R9)        NEXT INVOICE TOTAL                           
         LA    R2,4(R2)            NEXT PST AMOUNT FOR SAME INVOICE             
         BCT   R0,CPSTAM2L                                                      
*                                                                               
CPSTAM2D DS    0H                                                               
*                                                                               
CPSTPS2C DS    0H                                                               
*                                                                               
         LA    RF,PSTBCKSL(RF)     PST AREA FOR NEXT PROVINCE                   
         BCT   R5,CPSTPS2L                                                      
*                                                                               
CPSTPS2D DS    0H                                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         LR    R5,R4               POINT TO BIGGEST                             
         L     R0,DUB              PST TOT PUT TO FILE                          
         LPR   R0,R0               SO GET THE ABSOLUTE VALUE                    
         L     RF,WORK             AND GET ABSOLUTE VALUE OF TOTAL PST          
         LPR   RF,RF                                                            
         SR    RF,R0               THEN GET THE DIFFERENCE                      
*                                                                               
         L     R0,0(R1)                                                         
         SR    R0,RF               ADJUST THE LARGEST PST AMOUNT                
         ST    R0,0(R1)            AND STORE IT BACK                            
*                                                                               
*        ASSUME SAME INVOICE GETS ANY $ BASIS EXTRA                             
*                                                                               
         LR    R5,R4               POINT TO BIGGEST                             
         L     R0,DUB+4            PST TOT $'S PUT TO FILE                      
         LPR   R0,R0               SO GET THE ABSOLUTE VALUE                    
         L     RF,WORK+4           AND GET ABSOLUTE VALUE OF TOT PST $S         
         LPR   RF,RF                                                            
         SR    RF,R0               THEN GET THE DIFFERENCE                      
*                                                                               
         LA    RE,PSTAMTS          CALCULATE DISPLACEMENT OF PST AMT            
         SR    R1,RE                                                            
         LAY   R1,PST$BSS(R1)      USE IT TO GET $ BASIS                        
*                                                                               
         L     R0,0(R1)                                                         
         SR    R0,RF               ADJUST THE LARGEST PST $ AMOUNT              
         ST    R0,0(R1)            AND STORE IT BACK                            
*                                                                               
CTXPSTX  DS    0H                                                               
*                                                                               
CALCCTXX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R8,R9                                                            
*                                                                               
         TITLE 'PPPAY02 - PUT PST IN CARD - SETPST'                             
***********************************************************************         
*                                                                     *         
*        PUT PST AMOUNTS IN CARD                                      *         
*                                                                     *         
*NTRY    R2 ==>  REQUEST RECORD                                       *         
*        R4 ==>  INVOICE AMOUNT AND DATA                              *         
*        R5 ==>  INVOICE COUNTER                                      *         
*        R8 ==>  PAY TOTALS                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SETPST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R2,REQHDR           ESTABLISH REQUEST RECORD                     
         USING REQHDR,R2           ESTABLISH REQUEST RECORD                     
*                                                                               
         USING BAMTD,R4            ESTABLISH INVOICE AREA                       
*                                                                               
         USING PAYTOTSD,R8         ESTABLISH TOTALS FOR CHECK                   
*                                                                               
         LA    R3,QPST             POINT TO REQUEST PST AREA                    
*                                                                               
         LR    RE,R4         MUST CALCULATE DISPLACEMENT INTO PSTAMTS           
         LA    RF,PBAMTS                                                        
         SR    RE,RF                                                            
         SRL   RE,1                DIV  X 2 FOR INDEXING                        
*                                                                               
         LA    R6,10               10 PROVINCES                                 
*                                                                               
         LAY   RF,PSTBCKS                                                       
         USING PSTBCKS,RF                                                       
*                                                                               
SETPSTLP DS    0H                                                               
*                                                                               
         CLI   THISPSTC,C' '       IF NO PST CODE - SKIP                        
         BNH   SETPSTCN                                                         
*                                                                               
         MVC   QPSTPROV-QPSTPROV(L'QPSTPROV,R3),PROV    PROVINCE                
         MVC   QPSTCD-QPSTPROV(L'QPSTCD,R3),THISPSTC    PST CODE                
*                                                                               
         LA    R9,PSTAMTS(RE)      POINT TO PST FOR THIS INVOICE                
         ICM   R0,15,0(R9)                                                      
*                                                                               
         CLI   BAMTTYPE,C'3'       CHANGE SIGN IF A CK INVOICE                  
         BNE   *+6                                                              
         LCR   R0,R0                                                            
*                                                                               
         STCM  R0,15,QPSTAMT-QPSTPROV(R3)  PST AMOUNT                           
*                                                                               
         LA    R9,PST$BSS(RE)     POINT TO PST $ BASES FOR INVOICE              
         ICM   R0,15,0(R9)                                                      
*                                                                               
         CLI   BAMTTYPE,C'3'       CHANGE SIGN IF A CK INVOICE                  
         BNE   *+6                                                              
         LCR   R0,R0                                                            
*                                                                               
         STCM  R0,15,QPST$BS-QPSTPROV(R3) PST DOLLAR BASIS                      
*                                                                               
         LA    R3,QPSTPRVL(R3)     NEXT PROVINCIAL PST AREA                     
*                                                                               
SETPSTCN DS    0H                                                               
*                                                                               
         LA    RF,PSTBCKSL(RF)     NEXT PST AREA                                
         LA    R5,1(R5)                                                         
         BCT   R6,SETPSTLP                                                      
*                                                                               
SETPSTDN DS    0H                                                               
*                                                                               
         OI    Q2GNIND,X'40'       INDICATE PST BASIS IS BINARY                 
         OI    Q2GNIND,X'20'       INDICATE PST AMOUNT IS BINARY                
*                                                                               
         XIT1                                                                   
*                                                                               
*        TABLE OF PROVINCIAL CODES                                              
*                                                                               
PRVTAB   DS    0CL2                                                             
         DC    C'BC'                                                            
         DC    C'AL'                                                            
         DC    C'SA'                                                            
         DC    C'MA'                                                            
         DC    C'ON'                                                            
         DC    C'PQ'                                                            
         DC    C'NB'                                                            
         DC    C'NS'                                                            
         DC    C'PE'                                                            
         DC    C'NF'                                                            
         DC    X'FF'                                                            
*                                                                               
         DROP  R2,R4,RF                                                         
*                                                                               
         TITLE 'PPPAY02 - DELETE GST/PST FROM INVOICE AMOUNTS - DELCTX'         
***********************************************************************         
*                                                                     *         
*        REMOVE GST FROM INVOICE AMOUNT                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DELCTX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PAYTOTSD,R8         ESTABLISH TOTALS                             
*                                                                               
         TM    FXSW,X'08'          SKIP IF FX CHECK REQUEST                     
         BO    DELPSTX                                                          
*                                                                               
         OC    TOTGSTP,TOTGSTP     SKIP IF NO TOTAL GST PAID                    
         BZ    DELGSTX                                                          
*                                                                               
         LA    R0,PBMXIV#Q         MAX NUMBER OF INVOICES                       
         LA    RE,GSTAMTS          GST BUCKETS                                  
         LA    RF,PBAMTS           INVOICE AMOUNT BUCKETS                       
         USING BAMTD,RF            ESTABLISH AMOUNT AREA                        
*                                                                               
DLGSTLP  DS    0H                                                               
*                                                                               
         L     R1,BAMT             LOAD BAMTS                                   
         S     R1,0(RE)            REDUCE BY GST                                
         ST    R1,BAMT             SAVE NEW BAMT                                
*                                                                               
DLGSTCN  DS    0H                                                               
*                                                                               
         LA    RE,4(RE)            BUMP TO NEXT GST BUCKET                      
         LA    RF,BAMTL(RF)        BUMP TO NEXT INVOICE AMOUNT                  
*                                                                               
         BCT   R0,DLGSTLP                                                       
*                                                                               
DLGSTDN  DS    0H                                                               
*                                                                               
DELGSTX  DS    0H                                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        REMOVE PST FROM INVOICE AMOUNT                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DELPST   DS    0H                                                               
*                                                                               
         OC    TOTPSTP,TOTPSTP     SKIP IF NO PST TAX                           
         BZ    DELPSTX                                                          
*                                                                               
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
         LA    R5,10               10 PROVINCES                                 
*                                                                               
DLPSPSTL DS    0H                                                               
*                                                                               
         OC    PROV,PROV           SKIP IF NO PROVINCE                          
         BZ    DLPSPSTC                                                         
         OC    TOTPSTP,TOTPSTP     SKIP IF NO PST FOR PROVINCE                  
         BZ    DLPSPSTC                                                         
*                                                                               
         LA    R9,PBAMTS           POINT TO ENTERED INVOICE AMOUNTS             
         USING BAMTD,R9            ESTABLISH BAMTS                              
         LA    R2,PSTAMTS          POINT TO PST BY INVOICE BUCKETS              
         LA    R4,PBMXIV#Q         FIVE INVOICES                                
*                                                                               
DLPSAMTL DS    0H                                                               
*                                                                               
         ICM   R1,15,0(R2)         PST AMOUNT FOR PROVINCE/INVOICE              
         BZ    DLPSAMTC            SKIP IF NO PST                               
*                                                                               
         L     R0,BAMT             INVOICE AMOUNT                               
         SR    R0,R1               SUBTRACT OUT PST                             
         ST    R0,BAMT             UPDATE INVOICE AMOUNT                        
*                                                                               
DLPSAMTC DS    0H                                                               
*                                                                               
         LA    R9,BAMTL(R9)        NEXT INVOICE TOTAL                           
         LA    R2,4(R2)            NEXT INVOICE PST                             
*                                                                               
         BCT   R4,DLPSAMTL                                                      
*                                                                               
DLPSAMTD DS    0H                                                               
*                                                                               
DLPSPSTC DS    0H                                                               
*                                                                               
         LA    RF,PSTBCKSL(RF)     NEXT PROVINCE                                
         BCT   R5,DLPSPSTL                                                      
*                                                                               
DLPSPSTD DS    0H                                                               
*                                                                               
DELPSTX  DS    0H                                                               
*                                                                               
DELCTXX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RF,R9                                                            
*                                                                               
         TITLE 'PPPAY02 - CALCULATE NET ON GROSS ENTRIES - CALCNET'             
***********************************************************************         
*                                                                     *         
*        CALCULATE NET ON GROSS ENTRIES                               *         
*            FOR CHECK REQUEST                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CALCNET  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GENOLD,RC           ESTABLISH WORKING STORAGE                    
*                                                                               
         USING PAYTOTSD,R8         ESTABLISH TOTALS                             
*                                                                               
         LA    R4,PBAMTS           INVOICE AMTS                                 
         USING BAMTD,R4            ESTABLISH INVOICE AMOUNTS AREA               
         LA    R5,PBMXIV#Q         NUMBER OF INVOICES                           
*                                                                               
         XC    FULL,FULL           CLEAR ACCUM                                  
*                                                                               
CNLOOP   DS    0H                                                               
*                                                                               
CN01     OC    BAMT,BAMT           ANY VALUE IN BAMT(INVOICE)                   
         BNZ   CN01A               YES                                          
*                                                                               
         CLI   BAMTINP,C'C'        DONE IF NO INVOICE ENTERED                   
         BNE   CNDONE                                                           
*                                  FIND 85% OF INVOICE AMOUNT                   
CN01A    L     R1,BAMT             LOAD BAMT                                    
         M     R0,=F'170'          85 X 2                                       
         D     R0,=F'100'                                                       
*                                                                               
         LTR   R1,R1               ANY REMAINDER                                
         BM    *+8                                                              
         AH    R1,=H'1'            ROUND                                        
*                                                                               
         SRA   R1,1                DIVIDE BY 2                                  
*                                                                               
         ST    R1,BAMT             REPACE BAMT                                  
*                                                                               
         A     R1,FULL             ADD TO ACCUM                                 
         ST    R1,FULL             SAVE SUM                                     
*                                                                               
CNCONT   DS    0H                                                               
*                                                                               
         LA    R4,BAMTL(R4)        TO NEXT BAMT                                 
         BCT   R5,CNLOOP           LOOP THRU                                    
*                                                                               
CNDONE   DS    0H                                                               
*                                                                               
*        CHECK IF THERE IS A DIFFERENCE BETWEEN NET OF ENTERED AMOUNTS          
*              AND NET FROM PAID BUYS                                           
*                                                                               
         LM    R0,R1,PTOTCD        LOAD CD AND NET-CD (NO GST)                  
         AR    R1,R0               TRUE NET                                     
*                                                                               
         S     R1,FULL             FIND DIFFERANCE                              
         BE    CNNETDN               DONE IF NO DIFFERENCE FOUND                
*                                    IF DIFFERENCE FIND LARGEST AMOUNT          
         LA    R5,PBMXIV#Q         FIVE INVOICES                                
         LA    R4,PBAMTS           INVOICE AMTS                                 
         USING BAMTD,R4            ESTABLISH INVOICE AMOUNTS AREA               
         XC    FULL,FULL           CLEAR POINTER                                
*                                                                               
CNNETLP  DS    0H                                                               
*                                                                               
         CLI   BAMTTYPE,C'3'       SKIP IF CHECK RECIEVED FROM VENDOR           
         BE    CNNETCN                                                          
*                                                                               
         L     R0,BAMT             LOAD VALUE OF BAMT IN R0                     
*                                                                               
         C     R0,FULL             FIND HIGHEST VALUE                           
         BNH   CNNETCN             PASS IF EQ OR LOW                            
*                                                                               
         ST    R0,FULL             SAVE ADDRESS OF HIGHEST                      
         LR    R7,R4               SAVE IN R7 AS WELL                           
*                                                                               
CNNETCN  DS    0H                                                               
*                                                                               
         LA    R4,BAMTL(R4)        TO NEXT INVOICE                              
         BCT   R5,CNNETLP          LOOP THRU                                    
*                                                                               
         CLC   FULL(4),=F'0'       FOUND ANY (WOULD NOT IF BAMTTYPE=3)          
         BNE   CN05                                                             
*                                                                               
         LA    R7,PBAMTS           ADD DIFF TO FIRST INVOICE                    
         MVC   FULL(4),0(R7)                                                    
*                                                                               
CN05     A     R1,FULL             R1 CONTAINS DIFFERENC                        
         ST    R1,0(R7)            PLOP BACK INT BAMTS                          
CNNETDN  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PPPAY02 - CALCULATE GROSS ON NET ENTRIES - CALCGRS'             
***********************************************************************         
*                                                                     *         
*        CALCULATE GROSS ON NET INVOICE AMOUNTS                       *         
*            FOR CHECK REQUEST                                        *         
*        RESULTS ARE STORED IN BINVGRS                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CALCGRS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GENOLD,RC           ESTABLISH WORKING STORAGE                    
*                                                                               
         USING PAYTOTSD,R8         ESTABLISH TOTALS                             
*                                                                               
         CLI   GNOPT,C'G'          SKIP IF PAYING GROSS                         
         BE    CALCGRSX                                                         
*                                                                               
         LAY   RF,BINVGRS          POINT TO GROSS BUCKETS                       
         XC    0(20,RF),0(RF)      CLEAR GROSS AMOUNTS                          
*                                                                               
         LA    R4,PBAMTS           INVOICE AMTS                                 
         USING BAMTD,R4            ESTABLISH INVOICE AMOUNTS AREA               
         LA    R5,PBMXIV#Q         NUMBER OF INVOICES                           
*                                                                               
         XC    FULL,FULL           CLEAR ACCUM                                  
*                                                                               
CGLOOP   DS    0H                                                               
*                                                                               
         OC    BAMT,BAMT           ANY VALUE IN BAMT(INVOICE)                   
         BNZ   CG01A               YES                                          
*                                                                               
         CLI   BAMTINP,C'C'        DONE IF NO INVOICE ENTERED                   
         BNE   CGDONE                                                           
*                                                                               
CG01A    DS    0H                  FIND GROSS OF NET INVOICE AMOUNT             
*                                                                               
         SR    R0,R0                                                            
         L     R1,BAMT             LOAD BAMT                                    
*                                                                               
         SLA   R1,1                DOUBLE AMOUNT FOR ROUNDING                   
         M     R0,=F'100'          SCALE UP FOR 2 DECIMALS                      
         D     R0,=F'85'           SCALE UP NET AMOUNT                          
*                                                                               
         LTR   R1,R1               ANY REMAINDER                                
         BM    *+8                                                              
         AH    R1,=H'1'            ROUND                                        
*                                                                               
         SRA   R1,1                DIVIDE BY 2                                  
*                                                                               
         ST    R1,0(RF)            SAVE GROSS AMOUNT                            
*                                                                               
CGCONT   DS    0H                                                               
*                                                                               
         LA    RF,4(RF)            BUMP GROSS BUCKET POINTER                    
         LA    R4,BAMTL(R4)        TO NEXT BAMT                                 
         BCT   R5,CGLOOP           LOOP THRU                                    
*                                                                               
CGDONE   DS    0H                                                               
*                                                                               
CALCGRSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PPPAY02 - RESTORE GST/PST TO INVOICE AMOUNTS - RESCTX'          
***********************************************************************         
*                                                                     *         
*        RESTORE GST/PST TO INVOICE AMOUNTS                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0H                                                               
RESCTX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        ADD BACK IN GST IF ANY FROM GROSS                                      
*                                                                               
         USING PAYTOTSD,R8         ESTABLISH TOTALS                             
*                                                                               
         OC    TOTGSTP,TOTGSTP     TOTAL GST PAID                               
         BZ    RSGSTADX                                            L02          
*                                                                               
         LA    R0,PBMXIV#Q         MAX NUMBER OF INVOICES          L02          
         LA    RE,GSTAMTS          POINT TO GST BUCKETS            L02          
         LA    RF,PBAMTS           POINT TO INVOICE AMOUNTS        L02          
         USING BAMTD,RF            ESTABLISH INVOICE AMOUNTS AREA               
*                                                                               
RSGSTADL DS    0H                                                               
*                                                                               
         L     R1,BAMT             LOAD BAMTS                      L02          
         A     R1,0(RE)            REDUCE BY GST                   L02          
         ST    R1,BAMT             SAVE NEW BAMT                   L02          
*                                                                               
RSGSTADC DS    0H                                                               
*                                                                               
         LA    RE,4(RE)            BUMP TO NEXT GST BUCKET         L02          
         LA    RF,BAMTL(RF)        BUMP TO NEXT INVOICE BUCKET                  
         BCT   R0,RSGSTADL         LOOP                            L02          
*                                                                               
RSGSTADD DS    0H                                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
RSGSTADX DS    0H                                                               
*                                                                               
*        RESTORE ANY PST TO GROSS                                               
*                                                                               
         LAY   RF,PSTBCKS          ESTABLISH PST BUCKETS                        
         USING PSTBCKS,RF                                                       
         LA    R5,10               10 PROVINCES                                 
*                                                                               
RSPAPSTL DS    0H                                                               
*                                                                               
         OC    PROV,PROV           SKIP IF NO PROVINCE                          
         BZ    RSPAPSTC                                                         
         OC    TOTPSTP,TOTPSTP     SKIP IF NO PST FOR PROVINCE                  
         BZ    RSPAPSTC                                                         
*                                                                               
         LA    R9,PBAMTS           POINT TO ENTERED INVOICE AMOUNTS             
         USING BAMTD,R9            ESTABLISH INVOICE AMOUNTS AREA               
         LA    R2,PSTAMTS          POINT TO PST BY INVOICE BUCKETS              
         LA    R4,PBMXIV#Q         FIVE INVOICES                                
*                                                                               
RSPAAMTL DS    0H                                                               
*                                                                               
         ICM   R1,15,0(R2)         PST AMOUNT FOR PROVINCE/INVOICE              
         BZ    RSPAAMTC            SKIP IF NO PST                               
*                                                                               
         L     R0,BAMT             INVOICE AMOUNT                               
         AR    R0,R1               SUBTRACT OUT PST                             
         ST    R0,BAMT             UPDATE INVOICE AMOUNT                        
*                                                                               
RSPAAMTC DS    0H                                                               
*                                                                               
         LA    R9,BAMTL(R9)        NEXT INVOICE TOTAL                           
         LA    R2,4(R2)            NEXT INVOICE PST                             
*                                                                               
         BCT   R4,RSPAAMTL                                                      
*                                                                               
RSPAAMTD DS    0H                                                               
*                                                                               
RSPAPSTC DS    0H                                                               
*                                                                               
         LA    RF,PSTBCKSL(RF)     NEXT PROVINCE                                
         BCT   R5,RSPAPSTL                                                      
*                                                                               
RSPAPSTD DS    0H                                                               
*                                                                               
RESCTXX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R8,RF,R9                                                         
*                                                                               
         EJECT                                                                  
TSTSUB   DS    0D                                                               
         NMOD1 0,TESTSUB                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         CLC   =C'TEST',PAYMD+1                                                 
         BNE   TSTSUBX                                                          
         XC    LNE,LNE                                                          
         LA    R4,LNE                                                           
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         CLI   BPUB+4,X'FF'                                                     
         BNE   TSTSUB1                                                          
*                                                                               
         UNPK  0(3,R4),PBUYKPUB+4(2)                                            
         MVC   2(1,R4),PBUYKPUB+5                                               
         LA    R4,2(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
TSTSUB1  DS    0H                                                               
         CLC   =C'ALL',SAVPR                                                    
         BNE   *+18                                                             
         MVC   0(3,R4),PBUYREC+7                                                
         MVI   3(R4),C'/'                                                       
         LA    R4,4(R4)                                                         
         EDIT  (2,PBUYKEST),(4,0(R4)),ALIGN=LEFT                                
         AR    R4,R0                                                            
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(4,0(R4))                              
*                                                                               
         MVI   5(R4),C'/'                                                       
         LA    R4,6(R4)                                                         
         L     R0,GROSS                                                         
         S     R0,PGROSS                                                        
         CLI   GSTINDET,C'Y'       INCLUDE GST IN AMOUNTS          L02          
         BNE   *+8                                                 L02          
         A     R0,THISBGST         THIS BUY'S PAYABLE GST                       
         CLI   GNOPT,C'G'                                                       
         BE    *+12                                                             
         A     R0,PAGYCOM                                                       
         S     R0,AGYCOM                                                        
         EDIT  (R0),(12,0(R4)),2,ALIGN=LEFT                                     
         AR    R4,R0                                                            
         LR    R5,R4                                                            
         SH    R5,=H'3'                                                         
         CLC   0(3,R5),=C'.00'                                                  
         BNE   *+6                                                              
         LR    R4,R5                                                            
         CP    DUB,=P'0'                                                        
         BNM   *+12                                                             
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         L     R6,TSTLNE                                                        
*                                  GIVES LENGTH TO MOVE                         
         LA    R7,LNE                                                           
         SR    R4,R7                                                            
         LA    R7,0(R6,R4)                                                      
         C     R7,TSTLNEX                                                       
         BNH   TSTSUB2                                                          
         L     R2,TSTADDR                                                       
         BAS   R9,BUMPFLDT                                                      
         BE    TSTSUBX                  NO MORE ROOM                            
         ST    R2,TSTADDR                                                       
         LA    R6,8(R2)                                                         
         LA    R2,77(R2)                                                        
         ST    R2,TSTLNEX                                                       
*                                                                               
TSTSUB2  BCTR  R4,0                                                             
         EX    R4,TSTMVE                                                        
         LA    R4,2(R4,R6)                                                      
         ST    R4,TSTLNE                                                        
         L     R2,TSTADDR                                                       
         FOUT  (R2)                                                             
         B     TSTSUBX                                                          
*                                                                               
TSTMVE   MVC   0(0,R6),LNE                                                      
         SPACE 2                                                                
BUMPFLDT SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BCR   8,R9                                                             
         CLI   0(R2),0                                                          
         BR    R9                                                               
         SPACE 2                                                                
TSTSUBX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*********************************                                               
* BUILD CCUSA POSTING INTERFACE *                                               
*********************************                                               
         SPACE 1                                                                
PAYCC    DS    0D                                                               
         NMOD1 0,PAYCC                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         CLI   CRCKSW,0            SEE IF DOING CK/CK SWITCH                    
         BNE   PACCX      YES - THEN THERE WILL BE NOTHING TO TRANSFER          
*                                                                               
         CLI   SVXFRSW,C'N'        TEST SUPPRESS XFR                            
         BE    PACCX                                                            
         CLI   NCDSW,C'N'          SEE IF NOT TAKING CD                         
         BNE   PAYCC0                                                           
         L     R0,TOTNLCD          ADD TO NET LESS CD                           
         A     R0,TOTCD                                                         
         ST    R0,TOTNLCD          NOW NET                                      
         XC    TOTCD,TOTCD         AND CLEAR CD                                 
*                                                                               
PAYCC0   OC    TOTG,TOTG                                                        
         BNZ   PAYCC1                                                           
         OC    TOTNLCD,TOTNLCD                                                  
         BNZ   PAYCC1                                                           
         OC    TOTCD,TOTCD                                                      
         BZ    PACCX               ALL ARE ZERO SO DO NOTHING                   
*                                                                               
PAYCC1   LA    R2,PAYINV1H                                                      
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA  POINT TO INTERFACE AREA             
         USING XFRINVD,R4                                                       
         LA    R0,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
*                                                                               
PAYCC2   MVC   XFRINV,8(R2)                                                     
         ZAP   XFRGROSS,=P'0'                                                   
         ZAP   XFRNET,=P'0'                                                     
         ZAP   XFRCD,=P'0'                                                      
         MVC   XFRNARR,BLANKS                                                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO AMOUNT                              
         IC    RF,0(R2)                                                         
         AR    R2,RF               POINT TO COMMENT                             
         LLC   RE,0(R2)                                                         
         AHI   RE,-9                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   XFRNARR(0),8(R2)    MOVE NARRATIVE                               
*                                                                               
         IC    RF,0(R2)                                                         
         AR    R2,RF               POINT TO INVOICE                             
         LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC2                                                        
*                                                                               
* TRANSFER ADDITIONAL INVOICES TO ACC (XINV#LST)                                
*                                                                               
         TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    PAYCC3                                                           
*                                                                               
         ICM   R2,15,AXLKINV#      POINT ADDITIONAL INVOICES ARRAY              
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE ADDRESS                            
         CLI   AXLKINV#,LQ_TSINQ   ONE VALUE?                                   
         JNE   *+16                                                             
         LA    R0,1                ONE INVOICE TO PROCESS                       
         LA    R2,6(R2)            POINT TO START OF INVOICES                   
         J     PAYCC2M                                                          
         OC    6(2,R2),6(R2)       HAVE NUMBER OF INVOICES?                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,6(R2)          NUMBER OF INVOICES IN ARRAY                  
         LA    R2,8(R2)            POINT TO START OF INVOICES                   
*                                                                               
PAYCC2M  MVC   XFRINV,0(R2)                                                     
         ZAP   XFRGROSS,=P'0'                                                   
         ZAP   XFRNET,=P'0'                                                     
         ZAP   XFRCD,=P'0'                                                      
         MVC   XFRNARR,BLANKS                                                   
         MVC   XFRNARR(INVSRCLQ),(INVNUMLQ+INVAMTLQ)(R2)                        
         AHI   R2,INVTOTLQ         POINT TO NEXT INVOICE IN ARRARY              
         LA    R4,XFRINLEN(R4)                                                  
         JCT   R0,PAYCC2M                                                       
*                                                                               
*                                                                               
PAYCC3   LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         USING XFRINVD,R4                                                       
*                                                                               
         LA    R8,BAMTS                                                         
         LA    R0,PBMXIV#Q         MAX SUPPORTED # OF INVOICES                  
*                                                                               
PAYCC4   L     RF,0(R8)                                                         
         CLI   4(R8),C'2'          TEST CR                                      
         BE    PAYCC6                                                           
         CLI   4(R8),C'3'          OR CK                                        
         BE    PAYCC6                                                           
         B     *+6                                                              
PAYCC6   LNR   RF,RF               ALL CR'S AND CK'S ARE -                      
         CVD   RF,DUB                                                           
         ST    RF,FULL             SAVE FOR CD                                  
*                                                                               
         CLI   GNOPT,C'G'     TEST INVOICES GROSS/NET                           
         BNE   PAYCC8                                                           
* GROSS                                                                         
         ZAP   XFRGROSS,DUB                                                     
         OC    TOTG,TOTG                                                        
         BZ    PAYCC10             AVOID ZERO DIVIDE                            
         M     RE,TOTNLCD                                                       
         D     RE,TOTG                                                          
         CVD   RF,DUB                                                           
         ZAP   XFRNET,DUB                                                       
         B     PAYCC10                                                          
* NET                                                                           
PAYCC8   ZAP   XFRNET,DUB                                                       
         OC    TOTNLCD,TOTNLCD                                                  
         BZ    PAYCC10             AVOID ZERO DIVIDE                            
         M     RE,TOTG                                                          
         D     RE,TOTNLCD                                                       
         CVD   RF,DUB                                                           
         ZAP   XFRGROSS,DUB                                                     
*                                                                               
PAYCC10  L     RF,FULL             NOW DO CD                                    
         OC    TOTCD,TOTCD                                                      
         BZ    PAYCC11             AVOID ZERO DIVIDE                            
         M     RE,TOTCD                                                         
         CLI   GNOPT,C'G'          SEE IF DOING GROSS OR NET                    
         BE    PAYCC10C                                                         
         OC    TOTNLCD,TOTNLCD                                                  
         BZ    PAYCC11            AVOID ZERO DIVIDE                             
         D     RE,TOTNLCD                                                       
         B     PAYCC10D                                                         
*                                                                               
PAYCC10C OC    TOTG,TOTG          AVOID DIVIDE BY ZERO                          
         BZ    PAYCC11                                                          
*                                                                               
         D     RE,TOTG                                                          
PAYCC10D CVD   RF,DUB                                                           
         ZAP   XFRCD,DUB                                                        
*                                                                               
PAYCC11  LA    R4,XFRINLEN(R4)                                                  
         LA    R8,8(R8)            NEXT AMOUNT                                  
         BCT   R0,PAYCC4                                                        
*                                                                               
* NOW HAVE TO GET THESE GROSS/NET AMOUNTS TO EQUAL FILE AMOUNTS *               
*                                                                               
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         LA    R0,PBMXIV#Q                                                      
         ZAP   PGR,=P'0'                                                        
         ZAP   PCD,=P'0'                                                        
         ZAP   PNET,=P'0'                                                       
*                                                                               
PAYCC12  AP    PGR,XFRGROSS                                                     
         AP    PCD,XFRCD                                                        
         AP    PNET,XFRNET                                                      
         LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC12                                                       
*                                                                               
         L     R0,TOTG                                                          
         CVD   R0,DUB                                                           
         SP    PGR,DUB             SAVE DIFFERENCE GROSS                        
         L     R0,TOTCD                                                         
         CVD   R0,DUB                                                           
         SP    PCD,DUB            SAVE DIFFERENCE CD                            
         L     R0,TOTNLCD                                                       
         CVD   R0,DUB                                                           
         SP    PNET,DUB            SAVE DIFFERENCE NET                          
*                                                                               
* FIND LARGEST AMOUNT AND ADJUST                                                
*                                                                               
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         LA    R0,PBMXIV#Q                                                      
         LR    R1,R4                                                            
PAYCC14  CP    XFRGROSS-XFRINVD(6,R4),=P'0'                                     
         BE    PAYCC14A                                                         
         CP    XFRGROSS-XFRINVD(6,R1),XFRGROSS-XFRINVD(6,R4)                    
         BH    *+6                                                              
         LR    R1,R4                                                            
PAYCC14A LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC14                                                       
*                                                                               
         SP    XFRGROSS-XFRINVD(6,R1),PGR                                       
         SP    XFRNET-XFRINVD(6,R1),PNET                                        
         SP    XFRCD-XFRINVD(6,R1),PCD                                          
*                                                                               
         ZIC   R0,PROGPROF+12      GET COMM ADJ PCT                             
         CVD   R0,DUB                                                           
         ZAP   HALF,DUB                                                         
         SPACE 1                                                                
*                                                                               
* NOW CHANGE NET AMOUNTS TO REFLECT IPG COMMISSION *                            
*                                                                               
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         LA    R0,PBMXIV#Q                                                      
*                                                                               
PAYCC16  ZAP   DUB,XFRGROSS                                                     
         SP    DUB,XFRNET          GET GROSS-NET                                
         MP    DUB,HALF                                                         
         DP    DUB,=P'100'                                                      
         AP    XFRNET,DUB(6)       ADD DIFFERENCE TO NET                        
         LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC16                                                       
* SWITCH TO ACC *                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVXFRACC                                                 
******************************************* FOR TESTING                         
         CLC   T403FFD+10(2),=X'0011' TEST ID = SJR                             
         BNE   *+8                                                              
         MVI   DMCB,6                                                           
******************************************* FOR TESTING                         
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VACCPAY,DMCB,=C'POST',SVXFRDTA,PAYMSGH,VCOMFACS                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
* SWITCH BACK TO PRINT                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PACCX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40302  CHECK BILLING STATUS - CHKBLL'                          
***********************************************************************         
*                                                                     *         
*        CHECK STATUS OF BUY'S BILLS                                  *         
*        1. INSERTION HAS BEEN BILLED                                 *         
*        2. CASH HAS BEEN RECEIVED FOR BILL                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
CHKBLL   NMOD1 0,**#CHKBL                                                       
*                                                                               
         L     RC,0(R1)            RE-ESTABLISH WORKING STORAGE                 
         USING GENOLD,RC                                                        
*                                                                               
         USING T403FFD,RA          ESTABLISH TWA                                
*                                                                               
         LH    RE,=Y(CBLKEYSV-GENOLD)                                           
         AR    RE,RC                                                            
         MVC   0(L'CBLKEYSV,RE),KEY        SAVE CURRENT BUY KEY                 
*                                                                               
         L     R6,ACSHRCVC         A(CSHRCV CONTROL BLOCK)                      
         USING CSHRCVD,R6          ESTABLISH CONTROL BLOCK                      
*                                                                               
         CLC   BGROSS,GROSS        MUST BE BILLED FOR AT LEAST GROSS            
         BL    CBLBLLE1                                                         
*                                                                               
         ICM   RF,15,4(R1)         DONE IF BILL NEED NOT BE PAID                
         BNZ   CHKBLLOK                                                         
*                                                                               
         CLI   PROGPRO2+7,C'Y'     IF CASH NEEDN'T BE COLLECTED                 
         BE    *+8                                                              
         CLI   PROGPRO2+7,C'P'     IF CASH NEEDN'T BE COLLECTED                 
         BNE   CHKBLLOK               DONE                                      
*                                                                               
         L     R5,ABUYEL           POINT TO FIRST ELEMENT IN BUY RECORD         
*                                                                               
CBLLOOP  DS    0H                                                               
*                                                                               
         USING PBILELD,R5          ESTABLISH BILL ELEMENT                       
*                                                                               
         CLI   PBILELEM,0          DONE IF END OF REC REACHED                   
         BE    CBLDONE                                                          
*                                                                               
         CLI   PBILELEM,PBILELQ    LOOKING FOR BILL ELEMENTS                    
         BNE   CBLCONT                                                          
*                                                                               
         OC    PBLDATE,PBLDATE     IGNORE IF NOT BILLED                         
         BZ    CBLCONT                                                          
*                                                                               
         CLC   SAVPR,=C'ALL'       SKIP IF ALL PRODUCTS                         
         BE    *+14                                                             
         CLC   PBPRD,SAVPR         IGNORE IF NOT FOR TRANSACTION PRD            
         BNE   CBLCONT                                                          
*                                                                               
*        BUILD BILL RECORD KEY                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PBILLREC,R2         ESTABLISH BILL RECORD KEY                    
*                                                                               
         L     RE,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,RE                                                       
*                                                                               
         MVC   PBILKAGY,PBUYKAGY   AGENCY                                       
         MVC   PBILKMED,PBUYKMED   MEDIA                                        
         MVI   PBILKRCD,PBILKIDQ   RECORD ID                                    
         MVC   PBILKCLT,PBUYKCLT   CLIENT                                       
         MVC   PBILKPRD,PBUYKPRD   PRODUCT                                      
         MVC   PBILKEST,PBUYKEST   ESTIMATE                                     
         MVC   PBILKMOS,PBDBDATE   MONTH OF SERVICE                             
         MVC   PBILKBMN,PBLDATE    BILLING MONTH                                
         MVC   PBILKBNO,PBINVNO    INVOICE NUMBER                               
*                                                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 =A(HIGH),RR=RELO02  READ BILLREC                                 
*                                                                               
         CLC   PBILLKEY,KEYSAVE    MUST FIND IT                                 
         BNE   CBLBLLE2                                                         
*                                                                               
         TM    PBILLCTL+1-2,X'01'    MUST HAVE BEEN TRANSFERRED TO ACC          
         BNO   CBLBLLE3              (ADJUSTED FOR DIRECTORY CTL BYTES)         
*                                                                               
         BAS   RE,CHKBFR           CHECK BILL HAS BEEN CHECKED BEFORE           
         BE    CBLCONT             YES - ACCEPT                                 
*                                                                               
         MVC   AREC,ACLRREC        READ INTO CLEARANCE RECORD AREA              
*                                                                               
         GOTO1 =A(GETREC),RR=RELO02  READ IN RECORD                             
*                                                                               
         MVC   AREC,AIOAREA        RESTORE IOAREA PTR                           
*                                                                               
         L     R2,ACLRREC          POINT TO FOUND RECORD                        
*                                                                               
         GOTO1 VCSHRCV,DMCB,ACSHRCVC,(R2)   SEE IF CASH RECEIVED                
*                                                                               
         CLI   CRIND,CRIERRQ       TEST FOR ERRORS                              
         BE    CBLBLLER                                                         
*                                                                               
CBLCONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PBILELEM+1       BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
*                                                                               
         B     CBLLOOP                                                          
*                                                                               
CBLDONE  DS    0H                                                               
*                                                                               
         LH    RE,=Y(CBLKEYSV-GENOLD)                                           
         AR    RE,RC                                                            
         MVC   KEY,0(RE)           RESTORE CURRENT BUY KEY                      
*                                                                               
         GOTO1 =A(HIGH),RR=RELO02                                               
*                                                                               
CHKBLLOK DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
         B     CHKBLLX             ALL BILLS HAVE BEEN PAID                     
*                                                                               
CBLBLLER DS    0H                                                               
*                                                                               
         BAS   RE,CBLOVCK          CHECK FOR OVERRIDES                          
         BE    CBLDONE             OVERRIDE EXISTS                              
*                                                                               
         XC    PAYMSG,PAYMSG                                                    
         FOUT  PAYMSGH,=C'UNPAID BILL FOUND. NUMBER IS '                        
         MVC   PAYMSG+29(6),CRINV  PUT OUT INVOICE NUMBER                       
*                                                                               
         B     CBLBLLEX                                                         
*                                                                               
CBLBLLE1 DS    0H                  UNBILLED INSERTION                           
*                                                                               
         BAS   RE,CBLOVCK          CHECK FOR OVERRIDES                          
         BE    CBLDONE             OVERRIDE EXISTS                              
*                                                                               
         XC    PAYMSG,PAYMSG                                                    
         FOUT  PAYMSGH,=C'UNBILLED INSERTION - '                                
*                                                                               
         LA    R4,PAYMSG+21                                                     
*                                                                               
         L     R3,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,R3                                                       
*                                                                               
         CLI   BPUB+4,X'FF'        IF PAYING ACROSS ZONES                       
         BNE   CBLE110                                                          
*                                                                               
         UNPK  0(3,R4),PBUYKPUB+4(2)  PRINT ZONE AND EDITION                    
         MVC   2(1,R4),PBUYKPUB+5                                               
         LA    R4,2(R4)                                                         
*                                                                               
         CLI   0(R4),C' '          ADJUST FOR NO EDITION                        
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
*                                                                               
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
CBLE110  DS    0H                                                               
*                                                                               
         CLC   =C'ALL',SAVPR       IF PAYING ACROSS PRODUCTS                    
         BNE   *+18                                                             
         MVC   0(3,R4),PBUYREC+7      PRINT PRODUCT                             
         MVI   3(R4),C'/'                                                       
         LA    R4,4(R4)                                                         
*                                                                               
         EDIT  (2,19(R3)),(4,0(R4)),ALIGN=LEFT                                  
         AR    R4,R0                                                            
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,16(R3)),(4,0(R4)) INSERT DATE                    
*                                                                               
         B     CBLBLLEX                                                         
*                                                                               
CBLBLLE2 DS    0H                  BILL NOT FOUND                               
*                                                                               
         BAS   RE,CBLOVCK          CHECK FOR OVERRIDES                          
         BE    CHKBLLX             OVERRIDE EXISTS                              
*                                                                               
         XC    PAYMSG,PAYMSG                                                    
         FOUT  PAYMSGH,=C'BILL NOT FOUND - '                                    
*                                                                               
         LA    R4,PAYMSG+17                                                     
*                                                                               
         B     CBLBLLNM                                                         
*                                                                               
CBLBLLE3 DS    0H                  BILL NOT TRANSFERRED TO ACCPAK               
*                                                                               
         BAS   RE,CBLOVCK          CHECK FOR OVERRIDES                          
         BE    CHKBLLX             OVERRIDE EXISTS                              
*                                                                               
         XC    PAYMSG,PAYMSG                                                    
         FOUT  PAYMSGH,=C'BILL NOT TRANSFERRED TO ACCPAK - '                    
*                                                                               
         LA    R4,PAYMSG+33                                                     
*                                                                               
         B     CBLBLLNM                                                         
*                                                                               
CBLBLLNM DS    0H                  PRINT BILL NUMBER                            
*                                                                               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD INVOICE NUMBER                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         XC    WORK,WORK           PARAMETERS FOR GETPROF                       
*                                                                               
         MVC   WORK(4),=C'PB1X'    B1X PROFILE                                  
         NI    WORK,X'FF'-X'40'    LOWER CASE                                   
         MVC   WORK+4(2),PBUYKAGY  AGENCY                                       
         MVC   WORK+6(1),PBUYKMED  MEDIA                                        
         MVC   WORK+7(3),PBUYKCLT  CLIENT                                       
*                                                                               
         XC    BIVPROX,BIVPROX      SPECIAL BILLING PROFILE                     
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),BIVPROX,VDATAMGR                          
*                                                                               
*        BIVPROX+4   NOW HAS 'BASE' YEAR FOR INV MONTH DISPLAY                  
*        BIVPROX+5   ADD THIS NUNBER TO INVOICE MONTH                           
*                                                                               
         ZIC   R0,PBLDATE+1        BILL MONTH                                   
*                                                                               
*        IF MONTH IS TO BE COUNTED FROM A BASE YEAR                             
*          ADD ON 12*#NUMBER OF YEARS ELAPSED                                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,BIVPROX+4      INV MTH 'BASE' YEAR                          
         BZ    BIVYRX              NO BASE YEAR                                 
*                                                                               
         ZIC   RE,PBLDATE          BILLING YEAR                                 
         SR    RE,RF               NUMBER OF ELAPSED YEARS                      
         BNP   BIVYRX                                                           
*                                                                               
         MH    RE,=H'12'                                                        
         AR    R0,RE                                                            
*                                                                               
BIVYRX   DS    0H                                                               
*                                                                               
*        MAY HAVE TO ADD FIXED NUMBER TO INVOICE MONTH                          
*                                                                               
         CLI   BIVPROX+5,0          SEE IF INCREMENTING INV MONTH               
         BE    BIVMNX                NO                                         
*                                                                               
         ZIC   RE,BIVPROX+5        NUMBER OF MONTHS TO ADD                      
         AR    R0,RE                                                            
*                                                                               
         CLI   BIVPROX+4,0         SKIP IF BASE YEAR STARTING POINT             
         BNE   BIVMNX                                                           
*                                                                               
         CH    R0,=H'12'           SKIP IF STILL IN SAME YEAR                   
         BNH   BIVMNX                                                           
*                                                                               
         SH    R0,=H'12'           ADJUST TO JAN-DEC                            
*                                                                               
BIVMNX   DS    0H                                                               
*                                                                               
         CVD   R0,DUB              PRINT MONTH WITH LEADING 0'S                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
*                                                                               
         LA    R4,2(R4)            BUMP OUTPUT POINTER                          
*                                                                               
         MVI   0(R4),C'-'          SET SEPARATOR                                
         LA    R4,1(R4)            BUMP OUTPUT POINTER                          
*                                                                               
         ICM   R0,3,PBINVNO        PRINT INVOICE NUMBER                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
         UNPK  0(4,R4),DUB                                                      
*                                                                               
BLDINVX  DS    0H                                                               
*                                                                               
         B     CBLBLLEX                                                         
*                                                                               
CBLBLLEX DS    0H                                                               
*                                                                               
         TM    FXSW,X'80'          SKIP IF AGENCY NOT USING FX FEATURE          
         BNO   CBLBLLE9                                                         
*                                                                               
         XC    PAYMSG1,PAYMSG1                                                  
         FOUT  PAYMSG1H                                                         
*                                                                               
CBLBLLE9 DS    0H                                                               
*                                                                               
         LAY   RE,CBLKEYSV                                                      
         MVC   KEY,0(RE)           RESTORE CURRENT BUY KEY                      
*                                                                               
         GOTO1 =A(HIGH),RR=RELO02                                               
*                                                                               
         LTR   RB,RB               SET NE CC                                    
*                                                                               
CHKBLLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        CHECK FOR TRANSACTION OVERRIDE                                         
*                                                                               
CBLOVCK  DS    0H                                                               
*                                                                               
         CLC   =C'OVRD',PAYMD+1    IF TEST IS BEING OVERRIDDEN                  
         BE    *+10                                                             
         CLC   =C'OVRD',PAYMD+5       'MTESTOVRD'                               
         BNE   CBLOVCKX            NE CC                                        
*                                                                               
         TM    TWAAUTH-TWAD(RA),X'80'   AND USER HAS AUTHORITY                  
         BO    CBLOVCKX               (BIT MUST BE OFF)                         
*                                                                               
         CR    RB,RB                  SET EQ CC                                 
*                                                                               
CBLOVCKX DS    0H                                                               
*                                                                               
         BR    RE                   RETURN                                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R6,R3                                                            
*                                                                               
         TITLE 'T40302  CHECK BILL BUFFER - CHKBFR'                             
***********************************************************************         
*                                                                     *         
*        CHECK IF CURRENT BILL HAS ALREADY BEEN CHECKED FOR           *         
*        CASH RECEIVED. ITS KEY WILL BE IN THE BUFFER IF IT HAS       *         
*                                                                     *         
*EXIT    CC  0   IF BILL KEY IS IN BUFFER                             *         
*            <>0 OTHERWISE                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
CHKBFR   NTR1                                                                   
*                                                                               
         USING GENOLD,RC           ESTABLISH WORKING STORAGE                    
*                                                                               
         USING T403FFD,RA          ESTABLISH TWA                                
*                                                                               
         LM    R3,R5,BLBFBXLE      LOAD BXLE REGISTERS FOR BUFFER               
*                                                                               
         CR    R3,R5               IF START ON OR AFTER END                     
         BNL   CHKBFRNF               BUFFER IS EMPTY                           
*                                                                               
CHKBFRLP DS    0H                                                               
*                                                                               
         CLC   KEY,0(R3)           MATCH CURRENT BILL KEY TO BUFFER             
         BE    CHKBFRFD                                                         
*                                                                               
CHKBFRCN DS    0H                                                               
*                                                                               
         BXLE  R3,R4,CHKBFRLP                                                   
*                                                                               
CHKBFRDN DS    0H                                                               
*                                                                               
         B     CHKBFRNF            NOT IN BUFFER                                
*                                                                               
CHKBFRFD DS    0H                                                               
*                                                                               
         CR    RB,RB               SET = CONDITION CODE                         
         B     CHKBFRX                                                          
*                                                                               
CHKBFRNF DS    0H                  NOT IN BUFFER                                
*                                                                               
*        ADD BILL KEY TO BUFFER                                                 
*                                                                               
         LTR   R3,R3               IF A(BUFFER START) NOT SET                   
         BNZ   CHKBFRA1                                                         
*                                                                               
         LA    R3,BILLBUFF         SET START OF BUFFER                          
         ST    R3,BLBFSTRA                                                      
         LA    R4,L'PBILLKEY       SET BUFFER ELEMENT LENGTH                    
         ST    R4,BLBFLNTH                                                      
*                                                                               
CHKBFRA1 DS    0H                                                               
*                                                                               
         LH    RF,=Y(BILLBUFX-T403FFD) POINT TO END OF BUFFER                   
         AR    RF,RA                                                            
*                                                                               
         CR    R3,RF               SKIP IF NO ROOM LEFT IN BUFFER               
         BNL   CHKBFRA2                                                         
*                                                                               
         MVC   0(L'PBILLKEY,R3),KEY SAVE BILL KEY                               
*                                                                               
         LA    R5,0(R4,R3)         SET NEW END OF BILL BUFFER                   
         BCTR  R5,0                                                             
         ST    R5,BLBFENDA                                                      
*                                                                               
CHKBFRA2 DS    0H                                                               
*                                                                               
         LTR   RB,RB               SET NE CC                                    
*                                                                               
CHKBFRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2,R4,R5                                                         
*                                                                               
         TITLE 'T40302  BUILD CLEARANCE STATUS RECORD - BLDST'                  
*=========================================================*                     
*                                                         *                     
*        MAINTAIN CLEARANCE STATUS RECORD                 *                     
*        AND RETURN SEQUENCE NUMBER IN STATSEQ            *                     
*                                                         *                     
*NTRY    PARM0  A(TOTALS FOR CLRST REC)                   *                     
*        FXSW   X'08' - DOING FX CLRST REC                *                     
*                                                         *                     
*=========================================================*                     
*                                                                               
*         NOTE: DO NOT USE R9 IN THIS ROUTINE. IT IS NEEDED TO ADDRESS          
*               DATA MANAGER CALLS AT THE END OF THE "FIRST" CSECT              
*                                                                               
         DS    0D                                                               
BLDST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GENOLD,RC           ESTABLISH GENCON WORKING STORAGE             
*                                                                               
         USING T403FFD,RA          ESTABLISH TWA                                
         USING T40302+X'1000',R9   RE-ESTABLISH I/O ADDRESSES                   
*                                                                               
         L     R5,0(R1)            POINT TO TOTALS FOR CLRST REC                
         USING PAYTOTSD,R5         ESTABLISH TOTALS                             
*                                                                               
         MVC   ESAVKEY,KEY         SAVE KEY AND KEYSAVE                         
*                                                                               
         MVI   PSTATSQ,0           RESET CURRENT SEQUENCE NUMBER                
         XC    PSTATSQ2,PSTATSQ2   RESET CURRENT SEQUENCE NUMBER 2              
*                                                                               
         CLI   BYTE3,C'N'          SKIP IF NOT REALLY PAYING                    
         BE    BLDSTX                                                           
*                                                                               
*        BUILD ELEMENTS FOR THIS TRANSACTION                                    
*                                                                               
         LAY   R8,CLRELEMS         POINT TO ELEMENT BUILD AREA                  
         LR    R0,R8               INIT ELEMENT BUILD AREA                      
         LHI   R1,CLRXLENQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*        01 ELEMENT                                                             
*                                                                               
         USING PPCLEL01,R8         ESTABLISH MASTER CHECK ELEMENT               
*                                                                               
         MVI   PPCLEL01,X'01'      ELEMENT ID                                   
         MVI   PPCLEL01+1,PPCLELL2 ELEMENT LENGTH - NOTE NEW LENGTH             
         MVC   PPCLCLRD,BTODAY     TODAY'S DATE                                 
         MVC   PPCLZONE,BPUB+4     PUB ZONE                                     
         MVC   PPCLEDIT,BPUB+5     PUB EDITION                                  
*                                                                               
         CLC   PAYMD+1(4),=C'AUTO'                                              
         JNE   *+8                                                              
         OI    PPCLSTAT,X'08'      SCRIPT PAY UPLOAD                            
*                                                                               
         CLC   PAYMD+1(4),=C'AUTP'                                              
         JNE   *+8                                                              
         OI    PPCLSTAT,X'10'      SCRIPT AUTOPAY UPLOAD                        
*                                                                               
         CLI   PYSOURCE,C'P'       PAYING FROM PRISMA?                          
         JNE   *+8                                                              
         MVI   PPCLSRCE,PPCLSPRQ                                                
         CLI   PYSOURCE,C'R'       PAYING FROM RADIA?                           
         JNE   *+8                                                              
         MVI   PPCLSRCE,PPCLSRAQ                                                
*                                                                               
         OI    PPCLSTAT,X'04'+X'02'  NEW SQN AND 03 ELM FOLLOWS                 
*                                                                               
         LAY   RF,SVPID            POINT TO SAVED PID                           
         MVC   PPCLPID,0(RF)       SAVE PID OF CLEARER                          
         MVC   PPCLEST,BEST        SET ESTIMATE                                 
*                                                                               
         OC    PREP,PREP           SKIP IF NO PAYING REP                        
         BZ    BLDST2                                                           
*                                                                               
         MVC   PPCLPYEE,PREP       REP NUMBER                                   
         MVC   PPCLREPT,PREPTYP    AND TYPE                                     
*                                                                               
BLDST2   DS    0H                                                               
*                                                                               
         MVC   PPCLPRD,SAVPR       PRODUCT                                      
         MVC   PPCLSTDT(6),BSTART  START/END DATES                              
         MVC   PPCLCD,PTOTCD       CD                                           
*                                                                               
         CLI   GNOPT,C'G'      IF PAYING GROSS                                  
         BNE   BLDST4                                                           
*                                                                               
         MVC   PPCLGRS,PTOTG      SET GROSS                                     
*                                                                               
         B     BLDST10                                                          
*                                                                               
BLDST4   DS    0H                                                               
*                                                                               
         MVC   PPCLNET,PTOTNLCD    ELSE SET NET LESS CD                         
         OI    PPCLSTAT,X'20'      FLAG AS PAYING NET                           
*                                                                               
BLDST10  DS    0H                                                               
*                                                                               
         LA    R8,PPCLELL2(R8)     BUMP TO END OF 01 ELEMENT                    
*                                                                               
*        03/05 ELEMENTS - ONE PAIR FOR EACH STATION INVOICE                     
*                                                                               
         LA    R6,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
         LA    R2,PAYINV1H         POINT TO FIRST INVOICE                       
         LA    R3,PBAMTS           BUCKETS                                      
         USING BAMTD,R3                                                         
*                                                                               
BLDSIVLP DS    0H                                                               
*                                                                               
         OC    BAMT(BAMTL),BAMT    DONE AT EOT                                  
         JZ    BLDSIVDN                                                         
*                                                                               
         USING PPCLEL03,R8         ESTABLISH AS 03 ELEMENT                      
*                                                                               
         MVI   PPCLEL03,X'03'      ELEMENT ID                                   
         MVI   PPCLLN03,PPCL03LN   ELEMENT LENGTH                               
         MVC   PPCLINV,8(R2)       INVOICE NUMBER                               
         OC    PPCLINV,BLANKS      BLANK FILL                                   
*                                                                               
         LA    R1,L'PPCLINV        LENGTH OF INVOICE                            
         LA    RF,PPCLINV                                                       
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
*                                                                               
         LA    R8,PPCL03LN(R8)     BUMP TO END OF 03 ELEMENT                    
*                                                                               
*        BUILD 05 ELEMENT                                                       
*                                                                               
         USING PPCLEL05,R8         ESTABLISH 05 ELEMENT                         
*                                                                               
         MVI   PPCLEL05,X'05'      ELEMENT ID                                   
         MVI   PPCLLN05,PCL5ELLN   ELEMENT LENGTH                               
*                                                                               
         CLI   GNOPT,C'G'          IF PAYING GROSS                              
         BNE   BLDSIV4                                                          
*                                                                               
         OC    BTOT,BTOT           SKIP IF NO INVOICE TOTAL                     
         BZ    *+10                                                             
         CLC   BTOT,TOTG           IF NO TAXES INVOVLVED                        
         BNE   *+14                                                             
         MVC   PCL5GRS,BAMT           SET GROSS                                 
         B     BLDSIV10                                                         
*                                                                               
         L     RF,BAMT             CALC TOTG*(BAMT/BTOT)                        
         M     RE,TOTG                                                          
         D     RE,BTOT                                                          
*                                                                               
         SLL   RE,1                DOUBLE REMAINDER                             
         C     RE,BTOT                                                          
         BL    *+8                                                              
         AHI   RF,1                ROUNDUP                                      
*                                                                               
         STCM  RF,15,PCL5GRS          SAVE GROSS                                
*                                                                               
BLDSIV4  DS    0H                                                               
*                                                                               
         OC    BTOT,BTOT           SKIP IF NO INVOICE TOTAL                     
         BZ    *+10                                                             
         CLC   BTOT,TOTNLCD        IF NO TAXES INVOVLVED                        
         BNE   *+14                                                             
         MVC   PCL5NET,BAMT           SET NET LESS CD                           
         B     BLDSIV10                                                         
*                                                                               
         L     RF,BAMT             CALC TOTNLCD*(BAMT/BTOT)                     
         M     RE,TOTNLCD          NET LESS CD                                  
         D     RE,BTOT                                                          
*                                                                               
         SLL   RE,1                DOUBLE REMAINDER                             
         C     RE,BTOT                                                          
         BL    *+8                                                              
         AHI   RF,1                ROUNDUP                                      
*                                                                               
         STCM  RF,15,PCL5NET          SAVE GROSS                                
*                                                                               
BLDSIV10 DS    0H                                                               
*                                                                               
         CLI   BAMTXCD,C'X'        IF PAID W/O CD                               
         BNE   *+8                                                              
         OI    PCL5STAT,PCL5STAT_XCD    SET INDICATOR                           
*                                                                               
         CLI   BAMTTYPE,C'2'       IF CR AMOUNT                                 
         BNE   *+8                                                              
         OI    PCL5STAT,PCL5STAT_CR   SET INDICATOR                             
*                                                                               
         CLI   BAMTTYPE,C'3'       IF CK AMOUNT                                 
         BNE   *+8                                                              
         OI    PCL5STAT,PCL5STAT_CK   SET INDICATOR                             
*                                                                               
         LA    R8,PCL5ELLN(R8)     BUMP TO NEXT ELEMENT AREA                    
*                                                                               
BLDSIVCN DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT INVOICE FIELD                   
         BRAS  RE,BUMP                                                          
         BRAS  RE,BUMP                                                          
         LA    R3,BAMTL(R3)        NEXT SET OF BUCKETS                          
         BCT   R6,BLDSIVLP         NEXT INVOICE                                 
*                                                                               
BLDSIVDN DS    0H                                                               
*                                                                               
         BRAS  RE,BLDXINV#         BUILD ADDITIONAL INVOICES                    
*                                                                               
         LAY   RF,CLRELEMS         CALCULATE DATA LENGTH                        
         SR    R8,RF                                                            
         STY   R8,SVLEN            SAVE LENGTH OF DATA                          
*                                                                               
         DROP  R8                                                               
*                                                                               
*        FIND CLEARANCE STATUS RECORD TO UPDATE                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R8,KEY                                                           
         USING PPCLRST,R8          ESTABLISH CLEARANCE STATUS KEY               
*                                                                               
*        BUILD KEY                                                              
*                                                                               
         MVC   PPCLAGY,AGYALPHA    AGENCY                                       
         MVC   PPCLMED,PAYMD       MEDIA                                        
         MVI   PPCLTYPE,X'25'      RECORD ID                                    
         MVC   PPCLCLT,PAYCL       CLIENT                                       
         MVC   PPCLPUB(4),BPUB     BASE PUB                                     
*                                                                               
         DROP  R8                                                               
*                                                                               
         GOTO1 HIGH                READ FIRST CLEARANCE STATUS RECORD           
*                                                                               
         CLC   KEY(PPCLDATE-PPCLKEY),KEYSAVE SAME AGY/MED/TYPE/CLT/PUB          
         BNE   BLDSTNF             NONE - BUILD NEW RECORD                      
*                                                                               
BLDSTLP  DS    0H                                                               
*                                                                               
         MVC   KEYSAVE,KEY         SAVE PREVIOUS KEY                            
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD ON FILE                     
*                                                                               
         CLC   KEY(PPCLDATE-PPCLKEY),KEYSAVE SAME AGY/MED/TYPE/CLT/PUB          
         BNE   BLDSTDN             NO                                           
*                                                                               
BLDSTCN  DS    0H                                                               
*                                                                               
         B     BLDSTLP                                                          
*                                                                               
*        NO CLEARNCE STATUS RECORDS FOUND                                       
*        BUILD A NEW ONE                                                        
*                                                                               
BLDSTNF  DS    0H                  BUILD A NEW RECORD                           
*                                                                               
         L     R8,ACLRREC          POINT TO RECORD BUILD AREA                   
         ST    R8,AREC                                                          
*                                                                               
         XC    0(256,R8),0(R8)     INIT RECORD BUILD AREA                       
         USING PPCLRST,R8                                                       
*                                                                               
         MVC   PPCLKEY,KEYSAVE     USE THE KEY FROM READ HIGH                   
*                                                                               
         XC    KEYSAVE,KEYSAVE     THEN CLEAR SO WILL ADD LATER                 
*                                                                               
         MVC   25(2,R8),=H'33'     SET REC LEN WITH NO ELEMENTS                 
*                                                                               
         LA    R8,33(R8)           POINT TO FIRST ELEMENT POSITION              
         ST    R8,FULL             SET FOR NEXT INSERTION                       
*                                                                               
         B     BLDSELDN                                                         
*                                                                               
         DROP  R8                                                               
*                                                                               
BLDSTDN  DS    0H                                                               
*                                                                               
*        UPDATE LAST RECORD FOR THIS PUB                                        
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
*                                                                               
         L     R8,ACLRREC          POINT TO RECORD I/O AREA                     
         ST    R8,AREC                                                          
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         LA    R8,PPCLELEM-PPCLRST(R8) POINT TO FIRST ELEMENT                   
         USING PPCLEL01,R8                                                      
*                                                                               
         ST    R8,FULL             STORE JUST IN CASE                           
*                                                                               
*        FIND LAST 01 ELEMENT IN RECORD                                         
*                                                                               
         SR    RF,RF                                                            
*                                                                               
BLDSELLP DS    0H                                                               
*                                                                               
         CLI   PPCLEL01,0          DONE AT END OF RECORD                        
         BE    BLDSELDN                                                         
*                                                                               
         CLI   PPCLEL01,X'01'      SKIP IF NOT 01 ELEMENT                       
         BNE   BLDSELCN                                                         
*                                                                               
         ST    R8,FULL             SAVE ELEMENT ADDRESS                         
*                                                                               
BLDSELCN DS    0H                                                               
*                                                                               
         ICM   RF,1,PPCLEL01+1     GET ELEMENT LENGTH                           
         LA    R8,0(RF,R8)         BUMP TO NEXT ELEMENT                         
         B     BLDSELLP                                                         
*                                                                               
BLDSELDN DS    0H                                                               
*                                                                               
         LR    R0,R8               SAVE END OF RECORD POINTER                   
*                                                                               
         L     R8,FULL             GET ADDRESS OF LAST 01 ELEM                  
         USING PPCLEL01,R8         ESTABLISH 01 ELEMENT                         
*                                                                               
         SR    RE,RE               INIT SEQUENCE NUMBERS                        
         SR    RF,RF                                                            
*                                                                               
         CLI   PPCLEL01,0          SKIP IF NO 01 ELEMEENT FOUND                 
         BE    BLDSSQ10                                                         
*                                                                               
         CLC   PPCLCLRD,BTODAY     SKIP IF NEW DAY OF PAYMENTS                  
         BNE   *+12                                                             
         ICM   RE,1,PPCLCLSQ          GET PRIMARY   SEQUENCE NUMBER             
         ICM   RF,3,PPCLCLS2          GET SECONDARY SEQUENCE NUMBER             
*                                                                               
         CHI   RE,255              IF MAX FOR PRIMARY SEQ NUMBER                
         BNE   BLDSSQ10                                                         
*                                                                               
         CLC   =C'AUTP',PAYMD+1    PRINT AUTOPAY?                               
         JE    BLDSTERR            ERROR OUT FOR NOW                            
*                                                                               
         DC    H'0'                TROUBLE IF WE DO MORE THAN 255               
*                                                                               
         AHI   RF,1                   BUMP SECONDARY SEQ NUMBER                 
         B     BLDSSQ20                                                         
*                                                                               
BLDSSQ10 DS    0H                                                               
*                                                                               
         AHI   RE,1                ELSE BUMP PRIMARY SEQUENCE NUMBER            
*                                                                               
BLDSSQ20 DS    0H                                                               
*                                                                               
*        UPDATE SEQUENCE NUMBERS                                                
*                                                                               
         STC   RE,PSTATSQ          SET FOR USER                                 
         LAY   R1,CLRELEMS         POINT TO 01 ELEMENT                          
         STC   RE,PPCLCLSQ-PPCLEL01(R1)   AND SET IN NEW ELEMENT                
*                                                                               
         STCM  RF,3,PSTATSQ2       SET FOR USER                                 
         STCM  RF,3,PPCLCLS2-PPCLEL01(R1)   AND SET IN NEW ELEMENT              
*                                                                               
*        NEED TO MAKE SURE RECORD WON'T EXCEED 3000 BYTES                       
*                                                                               
         SR    RF,RF                                                            
         L     RE,AREC             POINT TO CURRENT CLRSTAT RECORD              
*                                                                               
         ICM   RF,3,25(RE)         GET CURRENT RECORD LENGTH                    
         AY    RF,SVLEN            ADD LENGTH TO BE ADDED NOW                   
*                                                                               
         TM    FXSW,X'08'          SKIP IF THIS IS AN FX BUILD                  
         BO    BLDSRC05                                                         
*                                                                               
         TM    FXSW,X'20'          IF FX PAYMENT BEING MADE                     
         BNO   *+10                                                             
         AY    RF,SVLEN              RESERVE DOUBLE THE SPACE                   
*                                                                               
BLDSRC05 DS    0H                                                               
*                                                                               
         TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    *+16                                                             
         CHI   RF,MXRLNQ-CLRXLENQ  FIT MAX ALLOWED INVOICES?                    
         JL    BLDSRC10                                                         
         J     BLDSRC06                                                         
*                                                                               
         CHI   RF,MXRLNQ-CLNORMLQ  FIT NORMAL FIVE INVOICES?                    
         JL    BLDSRC10                                                         
*                                                                               
*        START A NEW RECORD                                                     
*                                                                               
BLDSRC06 L     R8,AREC             POINT TO RECORD BUILD AREA                   
         XC    25(256,R8),25(R8)   CLEAR PART OF IT (LEAVE KEY)                 
*                                                                               
         MVC   PPCLDATE-PPCLRST(3,R8),BTODAY    LOW CLEAR DATE                  
         MVC   PPCLSEQ-PPCLRST(1,R8),PSTATSQ    LOW SEQUENCE NUMBERS            
         MVC   PPCLSEQ2-PPCLRST(2,R8),PSTATSQ2                                  
*                                                                               
         MVC   25(2,R8),=H'24'     SET LENGTH WITH NO ELEMENTS                  
         LA    R8,33(R8)           SET TO ADD FIRST ELEMENT                     
         LR    R0,R8               SAVE FIRST ELEMENT ADDRESS                   
*                                                                               
*        ADD ELEMENTS TO RECORD                                                 
*                                                                               
BLDSRC10 DS    0H                                                               
*                                                                               
         LR    R8,R0               POINT TO END OF RECORD                       
         LAY   R2,CLRELEMS         POINT TO FIRST ELM TO BE ADDED               
*                                                                               
BLDSRCLP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE AT END OF ELEMENTS                      
         BE    BLDSRCDN                                                         
*                                                                               
*        ADD ELEMENT TO RECORD                                                  
*                                                                               
         GOTO1 VRECUP,DMCB,(X'FE',AREC),0(R2),(C'R',(R8)),BLDSSYS               
         CLI   8(R1),C'R'                                                       
         BE    *+6                 NO OVERFLOW                                  
         DC    H'0'                SHOULD NOT OVERFLOW                          
*                                                                               
BLDSRCCN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(R2)            GET ELEMENT LENGTH                           
*                                                                               
         LA    R2,0(RF,R2)         BUMP TO NEXT ELEMENT                         
         LA    R8,0(RF,R8)         BUMP TO NEXT INSERT POINT                    
*                                                                               
         B     BLDSRCLP            ADD ANOTHER THE ELEMENT                      
*                                                                               
BLDSRCDN DS    0H                                                               
*                                                                               
         L     R8,AREC             POINT TO RECORD TO BE UPDATED                
*                                                                               
         MVC   KEY,0(R8)           SET NEW KEY                                  
*                                                                               
*        SAVE KEY OF UPDATED CLRST RECORD                                       
*                                                                               
         LAY   RF,ADDCDKEY                                                      
         XC    0(L'ADDCDKEY,RF),0(RF)  INIT SAVEAREA                            
         MVC   0(25,RF),KEY        SAVE KEY                                     
*                                                                               
         CLC   0(25,R8),KEYSAVE    WRITING SAME RECORD READ                     
         BE    BLDST32             YES                                          
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         LAY   RF,ADDCDKEY                                                      
         L     RE,DMCB+8           POINT TO D/A OF NEW REC                      
         MVC   27(4,RF),0(RE)      SAVE DISK ADDRESS                            
*                                                                               
         B     BLDSTX              HANDLE NEXT ELEMENT                          
*                                                                               
BLDST32  DS    0H                                                               
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE COMPLETE KEY AND D/A                 
         MVC   0(L'ADDCDKEY,RF),KEY  SAVE COMPLETE KEY                          
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
BLDSTX   DS    0H                                                               
*                                                                               
         MVC   AREC,ABUYIO         POINT TO BUY I/O AREA                        
         MVC   KEY(64),ESAVKEY      MUST RESTORE KEY AND KEYSAVE                
         CR    RB,RB               SET EQ CC                                    
         XIT1                                                                   
*                                                                               
BLDSTERR DS    0H                                                               
         LHI   R3,325              DATA EXCEEDED MAX ALLOWABLE VALUE            
         STH   R3,ADBERNUM                                                      
         LHI   RF,D#VINVNO                                                      
         STH   RF,ADBERFLD                                                      
         LTR   RB,RB               SET NEQ CC                                   
         XIT1                                                                   
*                                                                               
BLDSSYS  DC    AL2(33,25,3000)     FOR 3000 BYTE CLRST RECS                     
*                                                                               
         DROP  R8                                                               
*                                                                               
BUMP     DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BER   RE                                                 L01           
         CLI   0(R2),0                                                          
         BR    RE                                                 L01           
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BLDXINV# NTR1  BASE=*,LABEL=*      BUILD ADDITIONAL INVOICES                    
*                                                                               
         TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    BLDXI#_X                                                         
*                                                                               
         ICM   R2,15,AXLKINV#      POINT ADDITIONAL INVOICES ARRAY              
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE ADDRESS                            
*                                                                               
         USING PAYTOTSD,R5         ESTABLISH TOTALS                             
         LA    R3,PBAMTS           BUCKETS                                      
         LHI   RE,PBMXIVSQ*BAMTL                                                
         AR    R3,RE               POINT TO ADDITIONAL BUCKETS                  
         USING BAMTD,R3                                                         
*                                                                               
         CLI   AXLKINV#,LQ_TSINQ   ONE VALUE?                                   
         JNE   BLDXI#10                                                         
         LA    R0,1                ONE INVOICE TO PROCESS                       
         LA    R2,6(R2)            POINT TO START OF INVOICES                   
         J     BLDXI#20                                                         
*                                                                               
BLDXI#10 OC    6(2,R2),6(R2)       HAVE NUMBER OF INVOICES?                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,6(R2)          NUMBER OF INVOICES IN ARRAY                  
         LA    R2,8(R2)            POINT TO START OF INVOICES                   
*                                                                               
BLDXI#20 DS    0H                                                               
*                                                                               
BLDXI#30 OC    BAMT(BAMTL),BAMT    END OF AMOUNT TABLE?                         
         JZ    BLDXI#80                                                         
*                                                                               
         USING PPCLEL03,R8         ESTABLISH AND BUILD 03 ELEMENT               
         MVI   PPCLEL03,X'03'      ELEMENT ID                                   
         MVI   PPCLLN03,PPCL03LN   ELEMENT LENGTH                               
         MVC   PPCLINV(INVNUMLQ),0(R2)                                          
         OC    PPCLINV,BLANKS      BLANK FILL                                   
*                                                                               
         LA    R1,L'PPCLINV        LENGTH OF INVOICE                            
         LA    RF,PPCLINV                                                       
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
*                                                                               
         LA    R8,PPCL03LN(R8)     BUMP TO END OF 03 ELEMENT                    
*                                                                               
         USING PPCLEL05,R8         ESTABLISH AND BUILD 05 ELEMENT               
         MVI   PPCLEL05,X'05'      ELEMENT ID                                   
         MVI   PPCLLN05,PCL5ELLN   ELEMENT LENGTH                               
         CLI   GNOPT,C'G'          IF PAYING GROSS                              
         JNE   BLDXI#40                                                         
*                                                                               
         OC    BTOT,BTOT           HAVE INVOICE TOTAL?                          
         JZ    *+10                                                             
         CLC   BTOT,TOTG           TAXES?                                       
         JNE   *+14                                                             
         MVC   PCL5GRS,BAMT        SET GROSS                                    
         J     BLDXI#50                                                         
*                                                                               
         L     RF,BAMT             CALC TOTG*(BAMT/BTOT)                        
         M     RE,TOTG                                                          
         D     RE,BTOT                                                          
*                                                                               
         SLL   RE,1                DOUBLE REMAINDER                             
         C     RE,BTOT                                                          
         JL    *+8                                                              
         AHI   RF,1                ROUNDUP                                      
*                                                                               
         STCM  RF,15,PCL5GRS       SAVE GROSS                                   
*                                                                               
BLDXI#40 OC    BTOT,BTOT           INVOICE TOTAL?                               
         JZ    *+10                                                             
         CLC   BTOT,TOTNLCD        IF NO TAXES INVOVLVED                        
         JNE   *+14                                                             
         MVC   PCL5NET,BAMT        SET NET LESS CD                              
         J     BLDXI#50                                                         
*                                                                               
         L     RF,BAMT             CALC TOTNLCD*(BAMT/BTOT)                     
         M     RE,TOTNLCD          NET LESS CD                                  
         D     RE,BTOT                                                          
*                                                                               
         SLL   RE,1                DOUBLE REMAINDER                             
         C     RE,BTOT                                                          
         JL    *+8                                                              
         AHI   RF,1                ROUNDUP                                      
*                                                                               
         STCM  RF,15,PCL5NET       SAVE GROSS                                   
*                                                                               
BLDXI#50 CLI   BAMTXCD,C'X'        PAID WITHOUT CD?                             
         JNE   *+8                                                              
         OI    PCL5STAT,PCL5STAT_XCD                                            
*                                                                               
         CLI   BAMTTYPE,C'2'       CR AMOUNT?                                   
         JNE   *+8                                                              
         OI    PCL5STAT,PCL5STAT_CR                                             
*                                                                               
         CLI   BAMTTYPE,C'3'       CK AMOUNT?                                   
         JNE   *+8                                                              
         OI    PCL5STAT,PCL5STAT_CK                                             
*                                                                               
         LA    R8,PCL5ELLN(R8)     BUMP TO NEXT ELEMENT AREA                    
*                                                                               
         LA    R2,INVTOTLQ(R2)     BUMP TO NEXT INVOICE IN ARRAY                
         LA    R3,BAMTL(R3)        NEXT SET OF BUCKETS                          
         JCT   R0,BLDXI#30         NEXT INVOICE                                 
*                                                                               
BLDXI#80 DS    0H                                                               
*                                                                               
BLDXI#_X XIT1  REGS=(R8)           R8 POINTS TO ELEMENT WORK AREA               
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
*                                                                               
         TITLE 'T40302  ADD CD TO CLRST RECORD - ADDCD'                         
***********************************************************************         
*                                                                     *         
*        ADD CD TO 05 ELEMENTS IN CLRST RECORD                        *         
*              HATE TO RE-READ RECORD BUT IT IS THE                   *         
*              SAFEST WAY CONSIDERING THE SPAGHETTI CODE              *         
*                                                                     *         
***********************************************************************         
*                                                                               
*         NOTE: DO NOT USE R9 IN THIS ROUTINE. IT IS NEEDED TO ADDRESS          
*               DATA MANAGER CALLS AT THE END OF THE "FIRST" CSECT              
*                                                                               
         DS    0D                                                               
ADDCD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING T403FFD,RA          ESTABLISH TWA                                
         USING T40302+X'1000',R9   RE-ESTABLISH I/O ADDRESSES                   
*                                                                               
         L     R4,0(R1)            POINT TO TOTALS FOR CHECK                    
         USING PAYTOTSD,R4         ESTABLISH TOTALS FOR CHECK                   
*                                                                               
         MVC   ESAVKEY,KEY         SAVE KEY AND KEYSAVE                         
*                                                                               
         CLI   BYTE3,C'N'          SKIP IF NOT REALLY PAYING                    
         BE    ADDCDX                                                           
*                                                                               
*        RE-READ CLEARANCE STATUS RECORD TO UPDATE                              
*                                                                               
         LAY   RF,ADDCDKEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+27(4),27(RF)    CLRST SAVED DISK ADDRESS                     
*                                                                               
         L     R8,ACLRREC          POINT TO RECORD I/O AREA                     
         ST    R8,AREC                                                          
*                                                                               
         USING PPCLKEY,R8          ESTABLISH CLRST KEY                          
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         LAY   RF,ADDCDKEY                                                      
         CLC   PPCLKEY,0(RF)       MAKE SURE WE GOT CORRECT RECORD              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R8,PPCLELEM-PPCLRST(R8) POINT TO FIRST ELEMENT                   
         USING PPCLEL01,R8                                                      
*                                                                               
         ST    R8,FULL             STORE JUST IN CASE                           
*                                                                               
*        FIND 01 ELEMENT WITH CORRECT SEQ NUMBERS                               
*                                                                               
         SR    RF,RF                                                            
*                                                                               
ADCDELLP DS    0H                                                               
*                                                                               
         CLI   PPCLEL01,0          MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PPCLEL01,X'01'      SKIP IF NOT 01 ELEMENT                       
         BNE   ADCDELCN                                                         
*                                                                               
         CLC   PPCLCLRD,BTODAY       MATCH ON TODAY'S DATE                      
         BNE   ADCDELCN                                                         
*                                                                               
         CLC   PPCLCLSQ,PSTATSQ      MATCH ON PRIMARY SQN                       
         BNE   ADCDELCN                                                         
*                                                                               
         CLI   PPCLCLSQ,X'FF'      IF SQN IS X'FF'                              
         BNE   ADCDELFD            ELSE FOUND                                   
         CLC   PPCLCLS2,PSTATSQ2      MATCH ON SECONDARY SQN                    
         BE    ADCDELFD                                                         
*                                                                               
ADCDELCN DS    0H                                                               
*                                                                               
         ICM   RF,1,PPCLEL01+1     GET ELEMENT LENGTH                           
         LA    R8,0(RF,R8)         BUMP TO NEXT ELEMENT                         
         B     ADCDELLP                                                         
*                                                                               
ADCDELFD DS    0H                                                               
*                                                                               
         ICM   RF,1,PPCLEL01+1     GET ELEMENT LENGTH                           
         LA    R8,0(RF,R8)         BUMP TO NEXT ELEMENT                         
*                                                                               
         USING PPCLEL03,R8         ESTABLISH 03 ELEMENT                         
*                                                                               
*        03/05 ELEMENTS - ONE PAIR FOR EACH INVOICE                             
*                                                                               
         LA    R5,PBMXIVSQ         MAX # OF INVOICES ON SCREEN                  
         LA    R2,PAYINV1H         POINT TO FIRST INVOICE                       
         LA    R3,PBAMTS           BUCKETS                                      
         USING BAMTD,R3                                                         
         LAY   R6,BCDS             POINT TO CD AMOUNTS                          
*                                                                               
ADCDIVLP DS    0H                                                               
*                                                                               
         OC    BAMT(BAMTL),BAMT    DONE AT END OF TABLE                         
         BZ    ADCDIVDN                                                         
*                                                                               
         USING PPCLEL03,R8         ESTABLISH AS 03 ELEMENT                      
*                                                                               
         MVC   WORK(L'PPCLINV),8(R2)                                            
         OC    WORK(L'PPCLINV),BLANKS                                           
*                                                                               
         LA    R1,L'PPCLINV        LENGTH OF INVOICE                            
         LA    RF,WORK                                                          
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
*                                                                               
         CLC   PPCLINV,WORK        MATCH ON INVOICE NUMBER                      
         BE    *+6                                                              
         DC    H'0'                MUST MATCH                                   
*                                                                               
         LA    R8,PPCL03LN(R8)     BUMP TO END OF 03 ELEMENT                    
*                                                                               
         USING PPCLEL05,R8         ESTABLISH 05 ELEMENT                         
*                                                                               
         MVC   PCL5CD,0(R6)        SET CD                                       
*                                                                               
ADCDIVCN DS    0H                                                               
*                                                                               
         BRAS  RE,BUMP1            BUMP TO NEXT INVOICE FIELD                   
         BRAS  RE,BUMP1                                                         
         BRAS  RE,BUMP1                                                         
*                                                                               
         LA    R3,BAMTL(R3)        NEXT SET OF BUCKETS                          
         LA    R6,4(R6)            NEXT CD BUCKET                               
         LLC   RF,PPCLLN05         GET ELEMENT LENGTH                           
         LA    R8,0(RF,R8)         BUMP TO NEXT ELEMENT                         
         BCT   R5,ADCDIVLP         NEXT INVOICE                                 
*                                                                               
ADCDIVDN DS    0H                                                               
*                                                                               
         BRAS  RE,ADCXINV#         ADD CD TO ADDITIONAL INVOICES                
*                                                                               
         DROP  R8                                                               
*                                                                               
         L     R8,AREC                                                          
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
ADDCDX   DS    0H                                                               
         MVC   AREC,ABUYIO         POINT TO BUY I/O AREA                        
         MVC   KEY(64),ESAVKEY     MUST RESTORE KEY AND KEYSAVE                 
         XIT1                                                                   
*                                                                               
BUMP1    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADCXINV# NTR1  BASE=*,LABEL=*      ADD CD TO ADDITIONAL INVOICES                
*                                                                               
         TM    LKWRKSW1,LKXVIN#Q   HAVE ADDITIONAL INVOICES TO PROCESS?         
         JZ    ADCXI#_X                                                         
*                                                                               
         ICM   R2,15,AXLKINV#      POINT ADDITIONAL INVOICES ARRAY              
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE ADDRESS                            
*                                                                               
         LA    R3,PBAMTS           BUCKETS                                      
         LHI   RE,PBMXIVSQ*BAMTL                                                
         AR    R3,RE               POINT TO ADDITIONAL BUCKETS                  
         LAY   R6,BCDS             CD AMOUNTS                                   
         LHI   RE,PBMXIVSQ*4                                                    
         AR    R6,RE               POINT TO ADDITIONAL CD AMOUNTS               
*                                                                               
         CLI   AXLKINV#,LQ_TSINQ   ONE VALUE?                                   
         JNE   ADCXI#10                                                         
         LA    R5,1                ONE INVOICE TO PROCESS                       
         LA    R2,6(R2)            POINT TO START OF INVOICES                   
         J     ADCXI#20                                                         
*                                                                               
ADCXI#10 OC    6(2,R2),6(R2)       HAVE NUMBER OF INVOICES?                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
         ICM   R5,3,6(R2)          NUMBER OF INVOICES IN ARRAY                  
         LA    R2,8(R2)            POINT TO START OF INVOICES                   
*                                                                               
ADCXI#20 DS    0H                                                               
*                                                                               
         USING PPCLEL03,R8         ESTABLISH 03 ELEMENT                         
         USING BAMTD,R3                                                         
*                                                                               
ADCXI#30 OC    BAMT(BAMTL),BAMT    END OF TABLE?                                
         JZ    ADCXI#80                                                         
*                                                                               
         MVC   WORK(INVNUMLQ),0(R2)                                             
         OC    WORK(INVNUMLQ),BLANKS                                            
         LA    R1,L'PPCLINV        LENGTH OF INVOICE                            
         LA    RF,WORK                                                          
         BRAS  RE,FIXUCHAR         FIX UNPRINTABLE CHARACTERS                   
*                                                                               
         CLC   PPCLINV,WORK        MATCH ON INVOICE NUMBER                      
         JE    *+6                                                              
         DC    H'0'                MUST MATCH                                   
*                                                                               
         LA    R8,PPCL03LN(R8)     BUMP TO END OF 03 ELEMENT                    
*                                                                               
         USING PPCLEL05,R8         ESTABLISH 05 ELEMENT                         
*                                                                               
         MVC   PCL5CD,0(R6)        SET CD                                       
*                                                                               
         LA    R2,INVTOTLQ(R2)     BUMP TO NEXT INVOICE IN ARRAY                
         LA    R3,BAMTL(R3)        NEXT SET OF BUCKETS                          
         LA    R6,4(R6)            NEXT CD BUCKET                               
         LLC   RF,PPCLLN05         GET ELEMENT LENGTH                           
         LA    R8,0(RF,R8)         BUMP TO NEXT ELEMENT                         
         JCT   R5,ADCXI#30         NEXT INVOICE                                 
*                                                                               
ADCXI#80 DS    0H                                                               
*                                                                               
ADCXI#_X XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40302  BUILD CLEARANCE STATUS RECORD - CRCKGET'                
***********************************************************************         
*                                                                     *         
*        ************  USED FOR CR/CK REVERSALS  *********            *         
*    WILL SELECT AND DO CALCULATIONS FOR ALL PAYELEMS INVOLVED AND    *         
*    RETURN WITH INDICATOR IF BUY IS TO BE SKIPPED AS NOT INVOLVED    *         
*                                                                     *         
***********************************************************************         
*                                                                               
*         NOTE: DO NOT USE R9 IN THIS ROUTINE. IT IS NEEDED TO                  
*               ADDRESS THE "FIRST" CSECT ADDRESSES OFF 2ND BASE                
*                                                                               
         SPACE 1                                                                
         DS    0D                                                               
CRCKGET  NMOD1 0,*CRCKGET                                                       
*                                                                               
         L     RC,0(R1)            RE-ESTABLISH WORKING STORAGE                 
         USING GENOLD,RC                                                        
*                                                                               
         L     R9,4(R1)            RE-ESTABLISH ADDRESSES OFF 2ND BASE          
         USING T40302+X'1000',R9                                                
*                                                                               
         USING T403FFD,RA          ESTABLISH TWA                                
*                                                                               
*                                                                               
*        MUST SWITCH PAY ELEM CODES (X'25') TO X'AA' SO CAN USE GETINS          
*        TO "SELECTIVELY" RETURN PAID GST, ETC. (SEE BELOW)                     
*                                                                               
         L     R5,ABUYEL           POINT TO FIRST ELEMENT IN BUYREC             
         MVI   ELCODE,X'25'                                                     
CHGEL    BRAS  RE,NEXTEL2                                                       
         BNE   CHGELX                                                           
         MVI   0(R5),X'AA'                                                      
         B     CHGEL                                                            
*                                                                               
CHGELX   DS    0H                                                               
*                                                                               
*                                 MUST FIND THE PROPER PAY                      
*                                 ELEMENTS AND CHANGE THEIR CODES BACK          
*                                 TO 25 SO GETINS CAN DO ITS THING.             
*                                                                               
*                                                                               
         MVI   CRCKFND,C' '        CLEAR PROPER ELEM FOUND INDICATOR            
         L     R5,ABUYEL           POINT TO FIRST ELEMENT IN BUYREC             
         MVI   ELCODE,X'AA'                                                     
         BRAS  RE,NEXTEL2                                                       
         BNE   CRCKSKP             SKIP BUY - NO 'AA' ELEM FOUND                
         USING PPAYELD,R5                                                       
         B     CRCKA2              START TESTING PROCESS                        
CRCKA    BRAS  RE,NEXTEL2                                                       
         BE    CRCKA2              TEST FOR "INCLUSION"                         
         CLI   CRCKFND,C'Y'        ANY ELEMENTS FOR CHANGE FOUND ?              
         BE    CRCKAG              YES - GO SET-UP AND CALL GETINS              
         B     CRCKSKP             NO - SKIP THIS BUY                           
CRCKA2   CLC   PPDDATE,CRCKDAT     CHECK FOR CORRECT DATE                       
         BNE   CRCKA                                                            
         CLC   PPDSEQNO,CRCKSEQ    CHECK FOR CORRECT CHECK SEQUENCE             
         BNE   CRCKA                                                            
         MVC   WORK(1),PPREP                                                    
         NI    WORK,X'C0'          ONLY LEAVES X'80' OR X'40' ON                
         CLC   WORK(1),CRCKSW    SEE IF IT MATCHES TYPE I'M SWITCHING           
         BNE   CRCKA             WRONG TYPE - SKIP THIS ELEMENT                 
*                                                                               
         CLC   PPDCKDAT,CKDATE  CONTROL DATES MUST MATCH                        
         BE    CRCKA8                                                           
         OC    CKDATE,CKDATE     SEE IF I HAVE A CONTROL DATE                   
         BNZ   CRCKA6                                                           
         MVC   CKDATE,PPDCKDAT   USE DATE OF ELEM                               
         B     CRCKA8                                                           
*                                                                               
CRCKA6   LA    R2,PAYCKH        CURSOR TO CONTROL DATE                          
         XC    PAYMSG,PAYMSG                                                    
         MVC   PAYMSG(35),=C'INVALID CONTROL DATE - ORIGINAL WAS'               
         GOTO1 VDATCON,DMCB,(3,PPDCKDAT),(8,PAYMSG+36)                          
         FOUT  PAYMSGH                                                          
         NI    PAYCKH+4,X'DF'      INVALIDATE CONTROL DATE                      
*                                                                               
         TM    FXSW,X'80'          SKIP IF AGENCY NOT USING FX FEATURE          
         BNO   CRCKA7                                                           
*                                                                               
         XC    PAYMSG1,PAYMSG1                                                  
         FOUT  PAYMSG1H                                                         
*                                                                               
CRCKA7   DS    0H                                                               
*                                                                               
         B     CRCKEND5            STILL RESTORE ELEMENT CODES                  
*                                  CHANGE X'AA' BACK TO X'25' SO                
CRCKA8   MVI   0(R5),X'25'            GETINS WILL FIND THIS ELEM                
         MVI   CRCKFND,C'Y'        INDICATE PROPER ELEMENT FOUND                
         B     CRCKA               LOOK FOR MORE ELEMENTS TO CHANGE             
*                                                                               
CRCKAG   DS    0H                                                               
         MVC   WORK(3),CRCKDAT                                                  
         MVC   WORK+3(3),CRCKDAT                                                
*                                                                               
         L     RF,ABUYIO           POINT TO BUY                                 
         USING PBUYREC,RF                                                       
*                                                                               
         GOTO1 VGETINS,DMCB,(C'P',PBUYREC),PVALUES,PBUYKPRD,WORK,      X        
               =C'PST'                                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     RF,16(R1)          ADDRESS OF GST DATA                           
         LH    R1,=Y(GVALUES-GENOLD)                                            
         AR    R1,RC                                                            
         USING GVALUES,R1                                                       
*                                                                               
         MVC   GVALUES(GVALUESL),0(RF) COPY GST DATA                            
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LA    RE,PSTAREA          PST SAVEAREA                                 
         LA    RF,PSTAREA-GVALUES(RF)  PST DATA FROM GETINS                     
*                                                                               
         MVC   0(PSTAREAL,RE),0(RF)  COPY PST DATA                              
         LA    RE,PSTAREAL(RE)    BUMP POINTERS                                 
         LA    RF,PSTAREAL(RF)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         MVC   GSTTAX,GSTTAXPD    SET PREV PAID TO GROSS PAYABLE GST            
         XC    GSTTAXPD,GSTTAXPD  CLEAR PREV PAID GST                           
*                                                                               
         LA    R0,10               TEN PROVINCES                                
         LA    RF,PSTAREA          PST SAVEAREA                                 
         USING PSTAREA,RF          ESTABLISH PSTAREA                            
*                                                                               
         MVC   PSTTAX,PSTTAXPD    SET PREV PAID TO GROSS PAYABLE PST            
         XC    PSTTAXPD,PSTTAXPD  CLEAR PREV PAID PST                           
         MVC   PST$BS,PST$BSPD    SET PREV PAID $BASIS TO GROSS PYBLE           
         XC    PST$BSPD,PST$BSPD  CLEAR PREV PAID PST DOLLAR BASIS              
         LA    RF,PSTAREAL(RF)    BUMP POINTER                                  
         BCT   R0,*-28                                                          
*                                                                               
         DROP  RF,R1                                                            
*                                                                               
*                         MUST NOW CALCULATE GROSS, ETC., WHILE ONLY            
*                         "PROPER" PAYELEMS ARE SET TO ELEM CODE 25             
*                                                                               
         XC    GROSS(12),GROSS     CLEAR                                        
         LM    R6,R8,GROSS         CLEAR                                        
*                                                                               
         L     R5,ABUYEL           POINT TO FIRST ELEMENT IN BUYREC             
         MVI   ELCODE,X'25'                                                     
CRCKLUP  BRAS  RE,NEXTEL2                                                       
         BNE   CRCKEND                                                          
*                                                                               
         LM    R1,R3,PPGROSS       SET TO "PAID" GROSS,AGYCOM,CSHDSC            
         AR    R6,R1               R6 = GROSS                                   
         AR    R7,R2               R7 = AGYCOM                                  
         AR    R8,R3               R8 = CSHDSC                                  
         B     CRCKLUP                                                          
*                                                                               
CRCKEND  DS    0H                                                               
         STM   R6,R8,GROSS         SET GROSS TO PAID                            
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         S     R0,CSHDSC                                                        
         ST    R0,PYABLE                                                        
         XC    PGROSS(16),PGROSS         CLEAR PREV PAID AMTS                   
*                                                                               
***********************************************************************         
*        MUST SWITCH BACK X'AA' ELEM CODES TO X'25'                             
***********************************************************************         
*                                                                               
CRCKEND5 L     R5,ABUYEL           POINT TO FIRST ELEMENT IN BUYREC             
         MVI   ELCODE,X'AA'                                                     
CRCKENDX BRAS  RE,NEXTEL2                                                       
         BNE   CRCKXIT                                                          
         MVI   0(R5),X'25'                                                      
         B     CRCKENDX                                                         
***********************************************************************         
*                                                                               
CRCKSKP  DS    0H                                                               
         MVI   CRCKFND,C'X'        'X' MEANS SKIP THIS BUY WHEN RETURN          
CRCKXIT  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPPAY02 - BUMP TO NEXT ELMENT IN RECORD - NEXTEL2'              
***********************************************************************         
*                                                                     *         
*        BUMP TO NEXT ELEMENT IN RECORD                               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
NEXTEL2  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         BER   RE                                                               
         CLI   0(R5),0                                                          
         JNE   *-18                                                             
         LTR   R5,R5                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40302  BUILD CLEARANCE STATUS RECORD - BLDMQ'                  
***********************************************************************         
*                                                                     *         
*        BUILD MQ MESSAGE FOR IDESK                                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
BLDMQ    NTR1  BASE=*,LABEL=*      TRACK STDCOL CHANGES                         
*                                                                               
         L     R3,ABUYIO           ESTABLISH BUYREC                             
         USING PBUYREC,R3                                                       
*                                                                               
         TM    PBDSTAT2,X'20'      SKIP IF NOT BY IDESK                         
         BNO   BLDMQ_X                                                          
*                                                                               
         LAY   R2,SVIDKPRF         POINT TO IDK PROFILE                         
         OC    0(L'SVIDKPRF,R2),0(R2)   SKIP IF IDK PROFILE LOADED              
         BNZ   BLDMQPRX                                                         
*                                                                               
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'FF'-X'40'    MAKE SYSTEM LOWER CASE                       
         MVC   WORK+04(2),AGYALPHA                                              
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
*                                  GET AGENCY LEVEL PROFILE                     
         GOTO1 (RF),DMCB,(X'D0',WORK),(R2),VDATAMGR                             
*                                                                               
BLDMQPRX DS    0H                                                               
*                                                                               
         CLI   1(R2),C'Y'          SKIP IF PROFILE NOT SET                      
         BNE   BLDMQ_X                                                          
*                                                                               
*        BUILD MQ MESSAGE                                                       
*                                                                               
BLDMQST  DS    0H                                                               
*                                                                               
         LAY   R2,MQMSG            POINT TO MQ MESSAGE BUILD AREA               
*                                                                               
         CLC   4(L'QULABEL,R2),QULABEL  SKIP IF NOT 1ST TIME                    
         BE    BLDMQINX                                                         
*                                                                               
*        ADD QUEUE LABEL, AGENCY ALPHA TO MESSAGE                               
*                                                                               
         LA    R2,4(R2)            BYPASS LENGTH POSITION                       
*                                                                               
*        QUEUE LABEL                                                            
*                                                                               
         MVC   0(L'QULABEL,R2),QULABEL LABEL                                    
         LA    R2,L'QULABEL(R2)    BUMP PAST LABEL                              
*                                                                               
         MVC   0(L'CRLF,R2),CRLF   ADD CRLF                                     
         LA    R2,L'CRLF(R2)       BUMP PAST CRLF                               
*                                                                               
*        AGENCY ALPHA                                                           
*                                                                               
         MVC   0(L'QUMCAGY,R2),QUMCAGY    AGENCY ALPHA ID MAPCODE               
         MVI   L'QUMCAGY(R2),SEMICOLN     SEPARATOR                             
         LA    R2,L'QUMCAGY+1(R2)         BUMP PAST MAPCODE                     
*                                                                               
         MVC   0(L'PBUYKAGY,R2),PBUYKAGY  PASS AGY ALPHA                        
         LA    R2,L'PBUYKAGY(R2)   BUMP PAST AGENCY ALPHA                       
*                                                                               
         MVI   0(R2),TAB           SEPARATOR                                    
         LA    R2,1(R2)            BUMP PAST SEPARATOR                          
*                                                                               
*        PROGRAM SOURCE CODE                                                    
*                                                                               
         MVC   0(L'QUMCPID,R2),QUMCPGM    PROGRAM ID MAPCODE                    
         MVI   L'QUMCPID(R2),SEMICOLN     SEPARATOR                             
         LA    R2,L'QUMCPID+1(R2)         BUMP PAST MAPCODE                     
*                                                                               
         MVI   0(R2),C'P'          PASS PAY PROGRAM CODE                        
         LA    R2,1(R2)            BUMP PAST CODE                               
*                                                                               
         MVI   0(R2),TAB           SEPARATOR                                    
         LA    R2,1(R2)            BUMP PAST SEPARATOR                          
*                                                                               
*        USER'S PID AND NAME                                                    
*                                                                               
         BRAS  RE,TRNPID           TRANSLATE USER'S PID                         
*                                                                               
         MVC   0(L'QUMCPID,R2),QUMCPID    PID ALPHA ID MAPCODE                  
         MVI   L'QUMCPID(R2),SEMICOLN     SEPARATOR                             
         LA    R2,L'QUMCPID+1(R2)         BUMP PAST MAPCODE                     
*                                                                               
         LAY   RF,WKPID                                                         
         MVC   0(L'WKPID,R2),0(RF)    PASS PID                                  
         LA    R2,L'WKPID(R2)      BUMP PAST PID                                
*                                                                               
         MVI   0(R2),TAB           SEPARATOR                                    
         LA    R2,1(R2)            BUMP PAST SEPARATOR                          
*                                                                               
         MVC   0(L'QUMCPNM,R2),QUMCPNM    PID NAME ID MAPCODE                   
         MVI   L'QUMCPNM(R2),SEMICOLN     SEPARATOR                             
         LA    R2,L'QUMCPNM+1(R2)         BUMP PAST MAPCODE                     
*                                                                               
         LAY   RF,WKPIDNM                                                       
         MVC   0(L'WKPIDNM,R2),0(RF)   PASS PID NAME                            
*                                                                               
*        FIND END OF NAME                                                       
*                                                                               
         LA    R2,L'WKPIDNM-1(R2)  POINT TO END OF MAX NAME                     
         LA    R0,L'WKPIDNM        MAX LENGTH                                   
*                                                                               
         CLI   0(R2),C' '          FIND LAST CHARACTOR OF NAME                  
         BH    *+10                                                             
         BCTR  R2,0                BACK UP A POSITION                           
         BCT   R0,*-10                                                          
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
         MVC   0(L'CRLF,R2),CRLF   ADD CRLF                                     
         LA    R2,L'CRLF(R2)       BUMP PAST CRLF                               
*                                                                               
         LAY   RF,MQMSG                                                         
         SR    R2,RF               CALCULATE MSG LENGTH                         
         SHI   R2,4                DECREMENT FOR LENGTH FIELD                   
         STCM  R2,15,0(RF)         SET MESSAGE LENGTH                           
*                                                                               
BLDMQINX DS    0H                                                               
*                                                                               
*        ADD BUY SERIAL NUMBER TO MSG                                           
*        MED/CLT/SER#                                                           
*                                                                               
         LAY   R2,MQMSG            START OF MESSAGE                             
         ICM   RF,15,0(R2)         CURRENT LENGTH                               
         LA    R2,4(RF,R2)         NEXT AVAILABLE SPACE                         
*                                                                               
         MVC   0(L'QUMCSER#,R2),QUMCSER#  BUY SERIAL NUMBER MAPCODE             
         MVI   L'QUMCSER#(R2),SEMICOLN    SEPARATOR                             
         LA    R2,L'QUMCSER#+1(R2)        BUMP PAST MAPCODE                     
*                                                                               
         MVC   0(L'PBUYKMED,R2),PBUYKMED  MEDIA                                 
         MVC   L'PBUYKMED(L'PBUYKCLT,R2),PBUYKCLT  CLT                          
*                                                                               
         LA    R2,L'PBUYKMED+L'PBUYKCLT(R2)  BUMP POINTER                       
*                                                                               
         LA    R5,PBDELEM          POINT TO 1ST ELM                             
         MVI   ELCODE,X'99'                                                     
         BRAS  RE,NEXTEL           FIND BUY SERIAL ELEMENT                      
         BE    *+6                                                              
         DC    H'0'                NEED TO FIND IT                              
*                                                                               
         USING PSERELEM,R5         ESTABLISH SERIAL NUMBER ELM                  
*                                                                               
         ZAP   DUB,PSERNUM         COPY BUY SERIAL NUMBER                       
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(9,R2),DUB         CONVERT TO CH                                
*                                                                               
         LA    R2,9(R2)            BUMP POINTER                                 
*                                                                               
         MVI   0(R2),TAB           SEPARATOR                                    
         LA    R2,1(R2)            BUMP PAST SEPARATOR                          
*                                                                               
*        TOTAL PAID                                                             
*                                                                               
         GOTOR VGETINS,DMCB,PBUYREC,PVALUES,PBUYKPRD,0,0,0                      
*                                                                               
         MVC   0(L'QUMCTLPD,R2),QUMCTLPD  TOTAL PAID MAPCODE                    
         MVI   L'QUMCTLPD(R2),SEMICOLN    SEPARATOR                             
         LA    R2,L'QUMCTLPD+1(R2)        BUMP PAST MAPCODE                     
*                                                                               
         MVI   0(R2),C'$'          ALWAYS DOLLARS                               
         AHI   R2,1                BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
         ICM   RF,15,PAID          TOTAL PAID AMOUNT                            
         CVD   RF,DUB              CVD                                          
*                                                                               
         EDIT  (P8,DUB),(17,0(R2)),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT              
*                                                                               
         AR    R2,R0               BUMP TO NEXT AVAILABLE POSITION              
*                                                                               
         MVC   0(L'CRLF,R2),CRLF   ADD CRLF                                     
         LA    R2,L'CRLF(R2)       BUMP PAST CRLF                               
*                                                                               
         LAY   RF,MQMSG                                                         
         SR    R2,RF               CALCULATE MSG LENGTH                         
         SHI   R2,4                DECREMENT FOR LENGTH FIELD                   
         STCM  R2,15,0(RF)         SET MESSAGE LENGTH                           
*                                                                               
         B     BLDMQ_X             ALL DONE                                     
*                                                                               
BLDMQ_X  DS    0H                                                               
         CR    RB,RB               SET EQ CC                                    
         XIT1                                                                   
*                                                                               
SEMICOLN EQU   X'5E'               SEMI-COLON                                   
TAB      EQU   X'05'               TAB                                          
CRLF     DC    X'0D25'             CRLF                                         
*                                                                               
QULABEL  DC    CL16'IDKINV**********'     QUEUE LABEL                           
QUMCAGY  DC    CL4'0003'           AGENCY MAPCODE                               
QUMCPGM  DC    CL4'0040'           PROGRAM ID MAPCODE                           
QUMCSER# DC    CL4'0020'           BUY SERIAL NUMBER                            
QUMCPID  DC    CL4'0100'           USER'S PID                                   
QUMCPNM  DC    CL4'0110'           USER'S PID NAME                              
QUMCTLPD DC    CL4'0150'           TOTAL PAID DOLLARS                           
*                                                                               
         DROP  R5,RB                                                            
*                                                                               
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY                                                                 *         
*        SVPID    A(PID)                                              *         
*        WKPID    8CH PID                                             *         
*        WKPIDNM  USER'S NAME                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*,WORK=(R6,500)                                     
*                                                                               
         LAY   R5,SVPID            ESTABLISH WORKAREAS                          
         USING SVPID,R5                                                         
*                                                                               
         MVC   WKPID,BLANKS               INIT OUTPUT                           
         MVC   WKPIDNM(50),BLANKS         INIT OUTPUT                           
         MVC   WKPIDNM+50(10),BLANKS      INIT OUTPUT                           
*                                                                               
         OC    SVPID,SVPID         SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,BLANKS      IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,PBUYKAGY       USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,SVPID       SET PID                                      
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=CL7'DMRDHI',=CL7'CTFILE',KEY,(R6)                 
*                                                                               
         LR    R4,R6               POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
         MVC   WKPID,2(RE)         SAVE 8 CH PID                                
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,BLANKS      IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,PBUYKAGY       USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,WKPID       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=CL7'DMRDHI',=CL7'CTFILE',KEY,(R6)                 
*                                                                               
         LR    R4,R6               POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENT NOT FOUND                       
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WKPIDNM          BUILD NAME IN WORKAREA                       
         XC    WKPIDNM,WKPIDNM                                                  
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         CLC   PAYMD+1(4),=C'AUTO'                                              
         JNE   TPIDNF20                                                         
         MVC   WKPID(006),=CL6'SCRIPT'                                          
         MVC   WKPIDNM(6),=CL6'SCRIPT'                                          
         J     TPIDNF30                                                         
*                                                                               
TPIDNF20 CLC   =C'AUTP',PAYMD+1                                                 
         JNE   TPIDNF22                                                         
         MVC   WKPID(007),=CL7'AUTOPAY'                                         
         MVC   WKPIDNM(7),=CL7'AUTOPAY'                                         
         J     TPIDNF30                                                         
*                                                                               
TPIDNF22 MVC   WKPID(7),=CL7'UNKNOWN'                                           
         MVC   WKPIDNM(7),=CL7'UNKNOWN'                                         
*                                                                               
TPIDNF30 LA    R1,WKPIDNM+7        POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WKPIDNM          START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 =V(SQUASHER),DMCB,WKPIDNM,(R0),RR=RELO02 SQUASH NAME             
*                                                                               
         L     R3,ABUYIO           RESTORE BUY SEQ                              
         MVC   KEY,0(R3)                                                        
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
         DROP  R5,RB                                                            
*                                                                               
* UPDATE AUTOPAY RECORD                                                         
* READ ALL AUTOPAY RECORDS UNTIL MATCHING ONE FOUND.  START WITH                
* TODAY'S DATE AND THEN KEEP GOING BACK.                                        
*                                                                               
UPDAPY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PAPYRECD,R3                                                      
*                                                                               
         MVC   PAPYKAGY,AGYALPHA   AGENCY                                       
         MVC   PAPYKMED,PAYMD      MEDIA                                        
         MVI   PAPYKTYP,PAPYKRCD   AUTOPAY RECORD                               
         GOTO1 VDATCON,DMCB,(5,0),(2,PAPYKDAT) TODAY'S DATE                     
         XC    PAPYKDAT,=X'FFFF'   COMPLEMENT                                   
         MVC   PAPYKCLT,PAYCL      CLIENT                                       
*                                                                               
         ZIC   RF,AUTPSER+5                                                     
         SHI   RF,2                                                             
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,AUTPSER+10(0)   BUMP 8 FOR HEADER AND 2 FOR "**"             
         ZAP   PAPYKSN,DUB+3(5)    SERIAL #                                     
*                                                                               
         MVC   WORK(L'PAPYKEY),KEY SAVE AUTOPAY KEY                             
*                                                                               
UPDAPY02 GOTO1 HIGH                                                             
         CLC   KEY(PAPYKDAT-PAPYKEY),KEYSAVE   SAME AGY/MED/RECTYPE?            
         JNE   UPDAPYX                                                          
         CLC   KEY(L'PAPYKEY),KEYSAVE                                           
         JNE   *+12                                                             
         TM    KEY+25,PAPYPRCQ     ALREADY PROCESSED?                           
         JZ    UPDAPY10                                                         
*                                                                               
         XC    KEY,KEY             RESTORE ORIGINAL AUTOPAY KEY                 
         MVC   KEY(L'PAPYKEY),WORK AND TRY THE NEXT DAY                         
         SR    RF,RF                                                            
         ICM   RF,3,PAPYKDAT       DATE IS 2'S COMPLEMENTED SO ADD              
         AHI   RF,1                INSTEAD OF SUBTRACT                          
         STCM  RF,3,PAPYKDAT                                                    
         MVC   WORK(L'PAPYKEY),KEY SAVE AUTOPAY KEY W/ NEW DATE                 
         J     UPDAPY02                                                         
*                                                                               
UPDAPY10 OI    KEY+25,PAPYPRCQ     MARK AUTOPAY PROCESSED                       
         GOTO1 WRITE                                                            
*                                                                               
         MVC   AREC,ABUYIO         POINT TO BUY I/O AREA                        
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AREC                                                          
         OI    27(R3),PAPYPRCQ     MARK AUTOPAY PROCESSED                       
         AHI   R3,33                                                            
         USING PAP1D,R3                                                         
*                                                                               
         MVC   PAP1PAYE+1(L'SVREP),SVREP                                        
*                                                                               
         CLI   PAP1LEN,PAP1LNQ     OLD LENGTH?                                  
         JE    *+10                                                             
         MVC   PAP1MSG,PAYMSG      SAVE MESSAGE                                 
*                                                                               
         CLC   =C'EP',PAYMSG                                                    
         JE    UPDAPY20                                                         
         LA    RF,ERRTAB                                                        
UPDAPY17 CLI   0(RF),X'FF'         VALID ERROR?                                 
         JNE   UPDAPY18            NO - GET NEXT RECORD IN FILE                 
         CLC   0(ERRTABLN,RF),PAYMSG                                            
         JE    UPDAPY20                                                         
         AHI   RF,ERRTABLN                                                      
         J     UPDAPY17                                                         
*                                                                               
UPDAPY18 GOTO1 VDATCON,DMCB,(5,0),(3,PAP1ADTE)  AUTOPAID DATE                   
*                                                                               
UPDAPY20 GOTO1 PUTREC                                                           
UPDAPYX  XIT1                                                                   
         LTORG                                                                  
ERRTAB   DC    CL10'INVALID MI'                                                 
ERRTABLN EQU   *-ERRTAB                                                         
         DC    CL10'UNPAID BIL'                                                 
         DC    CL10'UNBILLED I'                                                 
         DC    CL10'BILL NOT F'                                                 
         DC    CL10'PLEASE ENT'                                                 
         DC    X'FF'                                                            
*                                                                               
         DROP  R3                                                               
*                                                                               
AUTPSER  DS    CL33                                                             
AUTHALF  DS    H                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SENDMQ   NTR1  BASE=*,LABEL=*      SEND MQ MESSAGE TO IDESK                     
*                                                                               
         L     R3,ABUYIO           ESTABLISH BUYREC                             
         USING PBUYREC,R3                                                       
*                                                                               
         TM    PBDSTAT2,X'20'      SKIP IF NOT BY IDESK                         
         BNO   SENDMQX                                                          
*                                                                               
         LAY   R2,SVIDKPRF                                                      
         OC    0(L'SVIDKPRF,R2),0(R2) SKIP IF IDK PROFILE LOADED                
         BNZ   SNDMQPRX                                                         
*                                                                               
         MVC   WORK+00(04),=C'PIDK'                                             
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+04(2),PBUYKAGY                                              
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
*                                  GET AGENCY LEVEL PROFILE                     
         GOTO1 (RF),DMCB,(X'D0',WORK),(R2),VDATAMGR                             
*                                                                               
SNDMQPRX DS    0H                                                               
*                                                                               
         CLI   1(R2),C'Y'          SKIP IF PROFILE NOT SET                      
         BNE   SENDMQX                                                          
*                                                                               
*        SEND MQ MESSAGE                                                        
*                                                                               
         LAY   R2,MQMSG            POINT TO MESSAGE                             
         ICM   R4,15,0(R2)         GET MESSAGE LENGTH                           
         BZ    SENDMQX             NONE TO SEND                                 
*                                                                               
         XC    DUB,DUB             INIT ERROR AREA                              
*                                                                               
         L     RF,VCOMFACS         GET DDMQIO ADDRESS                           
         L     RF,CMQIO-COMFACSD(RF)                                            
*                                                                               
         GOTO1 (RF),WORK,=CL8'PUT',4(R2),(R4),0,0,DUB,C'UNIT',0                 
*                                                                               
         CLI   0(R1),0             CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SENDMQX  DS    0H                                                               
         CR    RB,RB               SET EQ CC                                    
         XIT1                                                                   
*                                                                               
SENDMQER DS    0H                                                               
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         XIT1                                                                   
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
DTERR    EQU   20                                                               
CHKERR   EQU   190                                                              
PAYMERR  EQU   153        ATTEMPT TO PAY UNMATCHED INSERTION                    
NA0AUERR EQU   154        NO UNPAID A0A PROFILE BUYS FOUND                      
NA0ABERR EQU   155        NO A0A PROFILE BUYS FOUND                             
NOPRTADC EQU   156        NO PART PAYMENT IF BUY HAS ADDITIONAL CHARGES         
DATALOCK EQU   158        DATA IS LOCKED FOR OFFLINE PROCESSING                 
NOBUYERR EQU   200                                                              
NOUNPERR EQU   201                                                              
ICLERR   EQU   202                                                              
MIXERR   EQU   203                                                              
BADPSTPD EQU   231                 PAID SPOTS IN PERIOD-NO PST OVRD             
PAYTERR  EQU   252        ATTEMPT TO PAY INSERTION MISSING TEARSHEET            
NOPRTMLT EQU   259        NO PART PAYMENT IF MULTI-REQ OPTION REQUESTED         
NOCANBY  EQU   261        CANADIAN BUYS NOT ALLOWED IN SINGLY OPTION            
MULTERR1 EQU   262        ONLY 1 INVOICE AND AMT ALLOWED IN SINGLY OPT          
NOCDBUY  EQU   264        BUYS WITH CD NOT ALLOWED IN SINGLY OPTION             
NOCRCK   EQU   265        CR/CK NOT ALLOWED WITH SINGLY OPTION                  
MAXADBPY EQU   296        CANNOT PAY MORE THAN 200 BUYS THRU ADBUYER            
RECONERR EQU   343        MUST RECONCILE BUY/INVOICE BEFORE CLEARANCE           
PSTLIMIT EQU   626        PST MUST BE UNDER $100,000.00                         
PSTBSLIM EQU   627        PST BASIS MUST BE UNDER $21,000,000.00                
*                                                                               
CCUSAID  EQU   797                                                              
*                                                                               
         TITLE 'T40302  INCLUDED DSECTS'                                        
       ++INCLUDE PPCLRST                                                        
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
       ++INCLUDE PPPAYWRK                                                       
         EJECT                                                                  
*                                                                               
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
PPAYELD  DSECT                                                                  
       ++INCLUDE PPAYELEM                                                       
PBILELD  DSECT                     BILL ELEMENT                                 
PBILELQ  EQU   X'26'               BILL ELEMENT CODE                            
       ++INCLUDE PBILELEM                                                       
PBNIVELD DSECT                     BUY INVOICE ELEMENT (NEW STYLED)             
       ++INCLUDE PPGENPBNV                                                      
PACELEMD DSECT                     ADDITIONAL CHARGE ELEMENT                    
       ++INCLUDE PACELEM                                                        
PBYPSTD  DSECT                     PST ELEMENT                                  
PBYPSTQ  EQU   X'84'               PST ELEMENT CODE                             
       ++INCLUDE PBYPSTEL                                                       
       ++INCLUDE PPGENBYCC         BUY CUSTOM COLUMN DSECT                      
       ++INCLUDE PPGENBYDK         BUY PRISMA UPLOAD ELEMENT DSECT              
         EJECT                                                                  
*                                                                               
PBILLRCD DSECT                     BILL RECORD                                  
PBILKIDQ EQU   X'08'               BILL RECORD ID                               
       ++INCLUDE PBILLREC                                                       
         EJECT                                                                  
PUBRECD  DSECT                     PUB  RECORD                                  
PUBKIDQ  EQU   X'81'               PUB  RECORD ID                               
       ++INCLUDE PUBREC                                                         
PUBREPD  DSECT                     PUB  REP ELEMENT                             
PUBREPQ  EQU   X'14'               PUB  REP ELEMENT ID                          
       ++INCLUDE PUBREPEL                                                       
PAPYRECD DSECT                                                                  
       ++INCLUDE PPGENAPY                                                       
         PRINT ON                                                               
       ++INCLUDE PPPAYFFD                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPPAYTWA                                                       
         SPACE 2                                                                
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE PPERREQUS                                                      
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         SPACE 3                                                                
         PRINT ON                                                               
*SEACSFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         SPACE 3                                                                
         PRINT ON                                                               
*FATWA                                                                          
       ++INCLUDE FATWA                                                          
         SPACE 3                                                                
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKPUB  DS    XL4                 BASE PUB ONLY                                
         DS    XL2                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'208PPPAY02   01/20/21'                                      
         END                                                                    
