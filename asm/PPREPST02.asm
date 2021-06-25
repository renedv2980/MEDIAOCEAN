*          DATA SET PPREPST02  AT LEVEL 037 AS OF 04/23/03                      
*PHASE PPPM02D                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE PPFMTINO                                                               
         TITLE 'PPPM02 - PHILIP MORRIS INTERFACE'                               
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
****  NOTE  ************   NOTE   **************   NOTE   ************          
*  SMYE 04/22/03  AS OF 05/01/02 THIS PROGRAM IS ABSOLUTELY IDENTICAL           
*                 TO PPREPPM02 - I HAVE NO IDEA WHY THIS COPY EXISTS            
*                 PHASE CHANGED FROM PPPM02A TO PPPM02D                         
****  NOTE  ************   NOTE   **************   NOTE   ************          
*                                                                               
*  KWAN 08/00     NEW DSECT FOR PBILLREC                                        
*                                                                               
*  SMYE 06/00     CHANGES FOR LARGER PPNEWFILE (PCONREC ADDRESSING)             
*                                                                               
*  SMYE 11/98     CHANGE DTCNV CALLS TO DATCON CALLS - ADDITIONAL               
*                 CHANGES FOR Y2K                                               
*                                                                               
*  BPLA 10/95     ADD LOGIC FOR YNR TO ONLY CREATE 'C'                          
*                 RECORDS FOR ESTIMATES WITH A "B" IN THE                       
*                 SECOND FILTER POSITION                                        
*                 "B" MEANS THERE WAS NO CHANGE THE THIS ESTIMATE               
*                 BUT THERE MAY HAVE BEEN BILLING IN THE CURRENT PERIOD         
*                                                                               
*  BPLA 10/95     ADD LOGIC FOR TESTING (QOPT6 = Y)                             
*                 DOESN'T DIE AND DOESN'T PRODUCE A TAPE                        
*                 DON'T ALLOW MIX OF LIVE AND TEST REQUESTS                     
*                 BUILD TABLE OF BAD PRD/ESTS SO I WON'T                        
*                 KEEP REPEATING ERROR MESSAGES FOR THE SAME PRD/EST            
*                                                                               
*  BPLA 9/95      INCLUDE PPFMTINO AND USE IT TO FORMAT INVOICE NUMBERS         
*                 - IT NEEDS THE B1 AND B1X PROFILES THEY ARE NOW               
*                 READ AT FBUYCLI AND FBILCLI                                   
*                                                                               
* BPLA 4-5/95     COPY OF PPREPPM02O (LEVEL 178)                                
*                 CHANGES FOR Y&R                                               
*                                                                               
* BPLA 4/95       FIX OUTDOOR - IN PB120 IF OUTDOOR AND NO ACTIVITY             
*                 GO TO PB83B NOT PB82B                                         
*                                                                               
* BPLA 2/94       CHANGES FOR OUTDOOR                                           
*                                                                               
* BPLA 2/11/92    TREAT SUPPLEMENTS LIKE NEWSPAPERS - OLD WAY                   
*                                                                               
* BPLA 2/11/92    CAN'T USE PMMOSDAT TO CHECK VS PERIOD START                   
*                 IN BUFCOM+20 SINCE IT MIGHT NOT BE SET IF THERE               
*                 WAS NO CHANGE TO THE BUY                                      
* BPLA 2/11/92    FIX PMACCT FOR THOSE THAT HAVE ACCOUNT IN BUFCOM+4            
*                                                                               
* BPLA 1/7/92     FOR NEWSPAPERS - ALWAYS USE OLD WAY                           
*                                                                               
*                                                                               
* BPLA 12/17/91   FOR 1992 AND LATER ESTS USE STANDARD COMMENT CODE             
*                 FOR PMEST AND EST DESCRIPTION THEREBY COMBINING               
*                 PRINTPAK ESTS WITH SAME STANDARD COMMENT CODE.                
*                 BUCKET DATA FOR MAGS BY ON-SALE DATE INSTEAD OF               
*                 INSERTION DATE.                                               
*      FOR MAGS   PMSTART/PMEND FOR 1992 ESTIMATES NOW SET TO                   
*                 9N0101 AND 9N1231 (WHERE N IS 2 CHAR OF STANDARD              
*                 COMMENT ASSGINED TO THE ESTIMATE).                            
*        **NOTE** WHETHER OR NOT TO DO ALL THE ABOVE IS CONTROLLED              
*                 BY EST START DATE - 1992 AND LATER ESTIMATES ARE DONE         
*                 THE NEW WAY.                                                  
**********************************************************************          
*    **NOTE THAT DATE CHECKS WILL BE GOOD ONLY THROUGH 1999                     
*            CHANGES MUST BE MADE DEPENDING ON HOW YEAR 2000                    
*            IS TO BE HANDLED IN THE VARIOUS DATE FORMATS                       
*            ESPECIALLY CL6                                                     
**********************************************************************          
*                                                                               
*                                                                               
*        QOPT5 Y=LIST 'CURRENT' INVOICES                                        
*        QOPT7 Y=PDUMP RECORDS                                                  
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*                                                                               
PPPM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPPM02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'       2ND DSECT                                    
         L     R5,PPWORK2C                                                      
         USING PPWORK2D,R5                                                      
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPPMWRKD,R8                                                      
         MVC   ACONIO1,ACONIO      A(PCONREC) FROM PPG                          
         LA    R7,PPPM02+4095                                                   
         LA    R7,1(R7)                                                         
         LA    R9,1(R7)                                                         
         LA    R9,4095(R9)                                                      
         USING PPPM02+4096,R7,R9    ** NOTE USE OF R7 AND R9 AS BASES           
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,FBUYREQ                                                     
         BE    FIRSTB                                                           
****     CLI   MODE,FBUYEST                                                     
****     BE    ESTF                                                             
         CLI   MODE,FBUYCLI                                                     
         BE    FCLI                                                             
         CLI   MODE,FBILCLI                                                     
         BE    FCLI                                                             
         CLI   MODE,FBUYPRO                                                     
         BE    FPRD                                                             
         CLI   MODE,FBILPRO                                                     
         BE    FPRD                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         CLI   MODE,PROCBIL                                                     
         BE    BILL                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    PUTBUFF                                                          
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         L     R0,=V(GETUSER)                                                   
         A     R0,RELO                                                          
         ST    R0,VGETUSER                                                      
*                                                                               
         L     R0,=A(DISPTAB)                                                   
         A     R0,RELO                                                          
         ST    R0,ADISPTAB                                                      
         L     R0,=A(LENTAB)                                                    
         A     R0,RELO                                                          
         ST    R0,ALENTAB                                                       
         L     R0,=A(TITLES)                                                    
         A     R0,RELO                                                          
         ST    R0,ATITLES                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY1)                                     
         ZAP   TOTCNT,=P'0'                                                     
         L     R2,ALENTAB          ZAP ACCUMS                                   
         LA    R3,26               8 FOR NEWS + EXTRA + 8 FOR MAGS              
INIT2    ZAP   5(4,R2),=P'0'                                                    
         LA    R2,9(R2)                                                         
         BCT   R3,INIT2                                                         
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   TAPESW,0                                                         
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
FIRSTB   DS    0H                                                               
         CLI   QOPT6,C'Y'        SEE IF TEST REQUEST                            
         BE    FIRSTB0                                                          
         CLI   TAPESW,C'N'       SEE IF A PRIOR REQUEST WAS TEST                
         BE    MIXERR                                                           
         MVI   TAPESW,C'Y'       SET TAPE BEING PRODUCED                        
         OPEN  (OUTFILE,OUTPUT)                                                 
         B     FIRSTB0X                                                         
*                                                                               
FIRSTB0  CLI   TAPESW,C'Y'      SEE IF A PRIOR REQUEST WAS LIVE                 
         BE    MIXERR                                                           
         MVI   TAPESW,C'N'                                                      
         B     FIRSTB0X                                                         
*                                                                               
MIXERR   MVC   P1(37),=C'*** MIX OF TEST AND LIVE REQUESTS ***'                 
         MVC   P2(37),=C'*** THIS REQUEST HAS BENN SKIPPED ***'                 
         GOTO1 REPORT                                                           
         MVI   MODE,LBUYREQ    SKIP TO NEXT REQUEST                             
         B     EXIT                                                             
*                                                                               
FIRSTB0X DS    0H                                                               
*                             SET MYAGY AND MYCLT FROM AGYTAB                   
         LA    R1,AGYTAB              BACKER                                    
FIRSTB1  CLI   0(R1),X'FF'            END OF TABLE                              
         BNE   *+6                                                              
         DC    H'0'                   INVALID AGENCY                            
*                                                                               
         CLC   0(2,R1),QAGENCY                                                  
         BE    FIRSTB1D                                                         
         LA    R1,13(R1)                                                        
         B     FIRSTB1                                                          
*                                                                               
FIRSTB1D MVC   MYAGY,2(R1)                                                      
         MVC   MYCLT,8(R1)                                                      
         MVC   MYUSER,12(R1)         USER FIELDS IN USE                         
*                                                                               
FIRSTB2  DS    0H                                                               
         XC    ESTU1,ESTU1          CLEAR USER FIELDS                           
         XC    ESTU2,ESTU2                                                      
         XC    PRDU1,PRDU1                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   CINVGRS,=P'0'                                                    
         ZAP   CINVBIL,=P'0'                                                    
         ZAP   CINVCD,=P'0'                                                     
         ZAP   CINVRCV,=P'0'                                                    
         MVI   CINVSW,0                                                         
         XC    PESTREC(30),PESTREC                                              
         XC    ESTLLST(250),ESTLLST    CLEAR LIST OF UNLOCKED ESTS              
         XC    ESTLLST+250(250),ESTLLST+250                                     
*                                                                               
*                                  SAVE DATES FOR ACTIVITY CHECKING             
         GOTO1 DATCON,DMCB,(0,QSTART),(3,SVQSTART)                              
         MVC   SVQEND,=3X'FF'                                                   
         CLC   QEND,SPACES                                                      
         BE    FIRSTB3                                                          
         GOTO1 DATCON,DMCB,(0,QEND),(3,SVQEND)                                  
FIRSTB3  MVC   SVSTART(12),QSTART    SAVE EBCDIC DATES FOR BILL CHK             
         CLC   QEND,SPACES                                                      
         BNE   FIRSTB3C                                                         
*NOP*    MVC   SVEND(6),=C'999999'                                              
         MVC   SVEND(6),=6X'FF'    FOR 21ST CENTURY                             
*                                                                               
FIRSTB3C MVC   QSTART(12),SPACES                                                
*                                  BILLS WILL BE PASSED TO ME                   
         MVI   FCRDACTV,C'N'                                                    
         CLC   QPRODUCT,SPACES                                                  
         BNE   *+8                                                              
         MVI   FCRDACTV,C'Y'                                                    
         L     R0,=A(PPBYOWRK)                                                  
         A     R0,RELO                                                          
         ST    R0,APPBYOWK                                                      
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         B     EXIT                                                             
*                                                                               
FIRSTB4  DS    0H                                                               
FIRSTBX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
FCLI     DS    0H                  FIRST BUY OR BILL FOR CLIENT                 
*                                                                               
         XC    BADESTS(240),BADESTS   CLEAR BAD ESTIMATE TABLE                  
*                                                                               
         MVI   BADESTS,X'FF'        SET NEW END                                 
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNE   FBC1                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FPRD     DS    0H                  FIRST BUY OR BILL FOR PRODUCT                
         XC    PRDU1,PRDU1                                                      
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   FPRDX                                                            
         CLC   PPRDKPRD,=C'AAA'    NOT FOR PRD=AAA                              
         BE    FPRDX                                                            
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'P',PPRDREC),PRDU1,0              
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
         CLI   PRDU1+21,C' '    MUST FIND DATA                                  
         BNH   FPRDERR                                                          
         B     FPRDX                                                            
*                                                                               
FPRDERR  DS    0H                                                               
         MVC   P1,SPACES                                                        
         MVC   P1(36),=C'*** MISSING PRODUCT USER FIELD 1 ***'                  
         MVC   P1+40(3),PPRDKPRD                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    FPRDX                 CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
FPRDX    B     EXIT                                                             
         EJECT                                                                  
BILL     DS    0H                  OUTPUT INVOICE RECORDS                       
         TM    PBILLREC+27,X'80'                                                
         BNZ   EXIT                                                             
         OC    PBILKEST,PBILKEST       IGNORE BILLS WITH NO EST                 
         BZ    EXIT                                                             
         CLC   PBILLDAT,SVSTART                                                 
         BL    EXIT                                                             
         CLC   PBILLDAT,SVEND                                                   
         BH    EXIT                                                             
*                                                                               
         GOTO1 =V(PPFMTINO),DMCB,PBILLDAT,(2,PBILKBNO),                X        
               (PBILKMED,B1PROF),B1XPROF                                        
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)      FULL INVOICE NUMBER                          
*                                                                               
         MVI   CKESTREC,C'L'       SET FROM BILL                                
         GOTO1 =A(CKEST)           READ EST AND COMMENT                         
*                                                                               
         GOTO1 =A(INVOICE)                                                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PROCESS  DS    0H                  PROCESS BUYRECS                              
*                                  SEE IF CREATION DATE IN PERIOD               
         TM    PBUYCNTL,X'C0'      TEST CLOSED OUT                              
         BO    EXIT                YES IGNORE COMPLETELY                        
*                                                                               
         CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BE    EXIT                 IGNORE                                      
*****                                                                           
         MVI   CKESTREC,C'B'       SET FROM BUY                                 
         GOTO1 =A(CKEST)           GO READ EST AND COMMENT                      
*                                                                               
         CLI   BILLONLY,C'Y'       SEE IF BILL ONLY ESTIMATE                    
         BE    PB1B                (SPECIAL FOR YNR)                            
*                                                                               
         MVI   ADDDEL,0                                                         
         CLC   PBDBUYDT,SVQSTART                                                
         BL    PB2                                                              
         CLC   PBDBUYDT,SVQEND     SEE IF AFTER END                             
         BH    EXIT                SKIP THIS BUY                                
         MVI   TRCODE,C'A'         MUST BE ADD - EVEN IF CHANGED                
         TM    PBUYCNTL,X'80'      SEE IF DELETED                               
         BZ    PB4                                                              
*                                                                               
*              ONLY PROCESS IF BILLED IN PERIOD                                 
PB1B     LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'26'       FIND BILLING ELEMS                           
PB1D     BAS   RE,NEXTEL                                                        
         BNE   EXIT                FINALLY FINISHED WITH BUY                    
         USING PBILELEM,R2                                                      
         CLC   PBPRD,PPRDKPRD     PRODUCT MUST MATCH THE ONE I'M DOING          
         BNE   PB1D                                                             
         OC    PBLDATE,PBLDATE                                                  
         BZ    PB1D                                                             
         CLC   PBLDATE,SVQSTART    SEE IF BILLED IN PERIOD                      
         BL    PB1D                                                             
         CLC   PBLDATE,SVQEND                                                   
         BH    PB1D                                                             
*                             ELEMS                                             
*                                                                               
*        SKIP ADD AND DELETE LOGIC                                              
*        GO DIRECTLY TO CHK FOR BILLING ELEMS                                   
         B     PB300                                                            
*                                                                               
*              FOLLOWING CODE IS NO-OPED                                        
*                                                                               
**       MVI   ADDDEL,1                                                         
**       B     PB4                                                              
****                                                                            
**** NOTE                                                                       
**** IF INS ADDED,BILLED,AND DELETED IN PERIOD                                  
**** THEN ADD AND DELETE RECORDS WILL BE CREATED                                
****                                                                            
*                                                                               
PB2      CLC   PBDDATE,SVQSTART    CHK FOR CHG IN PERIOD                        
         BL    PB100               SKIP TO B12 POSTING                          
         CLC   PBDDATE,SVQEND      CHK FOR CHG PASSED PERIOD                    
         BNH   PB3                                                              
         NI    PBUYCNTL,X'7F'      UN DELETE                                    
*                                  TREAT AS CHG IN PERIOD                       
*                                                                               
*                                                                               
PB3      MVI   TRCODE,C'C'         SET TO CHANGE                                
         CLI   PBUYCNTL,X'80'      SEE IF DELETED                               
         BNE   *+8                                                              
         MVI   TRCODE,C'D'                                                      
*                                                                               
*              NOTE - IF LAST CHG DATE IN PERIOD OR LATER MUST STILL            
*                     PROCESS AS ACTIVE IN PERIOD                               
****** B10 (NEWS) OR B20 (MAGS) RECORDS                                         
*                                                                               
PB4      LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         MVC   OUTREC+200(100),SPACES                                           
         MVC   OUTREC+300(100),SPACES                                           
         MVC   OUTREC+400(100),SPACES                                           
         USING PMRECD,R6                                                        
         MVC   PMAGENCY,MYAGY AGENCY CODE                                       
         MVC   PMMEDIA,=C'PN'                                                   
         CLI   PBUYKMED,C'N'                                                    
         BE    PB4B                                                             
         MVC   PMMEDIA,=C'PM'      MAGS,SUPP,TRADE                              
         CLI   PBUYKMED,C'O'       SEE IF OUTDOOR                               
         BNE   PB4B                                                             
         MVC   PMMEDIA,=C'OD'                                                   
*                                                                               
PB4B     MVI   PMRTYPE,C'B'                                                     
         MVC   PMCLIENT,MYCLT      AGY/CLIENT                                   
*                                                                               
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   PB4C                                                             
         MVC   PMEST,ESTU1+21                                                   
         B     PB4E                                                             
*                                                                               
PB4C     DS    0H                                                               
         MVC   PMEST(1),PESTST+1   LAST DIGIT OF YEAR                           
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMEST+1(3),DUB                                                   
*                                                                               
         CLI   QMEDIA,C'N'            DO NEWSPAPERS THE OLD WAY                 
         BE    PB4E                                                             
*                                                                               
         CLI   QMEDIA,C'S'            DO SUPPLEMENTS THE OLD WAY                
         BE    PB4E                                                             
*                                                                               
         CLI   QMEDIA,C'O'            DO OUTDOOR THE OLD WAY                    
         BE    PB4E                                                             
*                                                                               
         CLC   PESTST(2),=C'92'    SEE IF BEFORE 1992                           
         BL    PB4E                IF YES - DO OLD WAY                          
         MVC   PMEST(4),PESTCOM+2  NEW WAY USE STANDARD COMMENT CODE            
*                                                                               
         CLC   PESTCOM,SPACES                                                   
         BH    PB4E                                                             
         MVC   P1,SPACES                                                        
         MVC   P1(36),=C'*** MISSING EST STANDARD COMMENT ***'                  
         MVC   P2(8),=C'PRD/EST='                                               
         MVC   P2+9(3),PESTKPRD                                                 
         MVI   P2+12,C'/'                                                       
         MVC   P2+13(3),PMEST+1                                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PB4E                  CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
PB4E     MVC   PMPRD,PPRDKPRD      USE PRODUCT I'M PROCESSING                   
*                                                                               
         CLI   MYUSER,C'Y'         SEE IF USING USER CODES                      
         BNE   *+10                                                             
         MVC   PMPRD,PRDU1+21                                                   
*                                                                               
         UNPK  WORK(11),PBUYKPUB(6)                                             
         MVC   PMPUB(8),WORK                                                    
*                                                                               
         MVC   PMNTR,TRCODE        TRANSACTION CODE                             
*                                                                               
         CLI   PBUYKMED,C'N'       MAGS DIFFERENT FROM HERE                     
         BNE   PB50                                                             
******                                                                          
******                                                                          
         CLC   QAGENCY,=C'BS'       SPECIAL CHECK FOR BACKER                    
         BNE   PB4G                                                             
******   TREAT EST 003 AS A MAGAZINE                                            
         CLC   PMEST+1(3),=C'003'                                               
         BE    PB40                                                             
*                                                                               
PB4G     DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(X'20',PMINSDAT)                        
         BAS   RE,GETLIN                                                        
         MVC   PMINSDS,DUB                                                      
         MVC   PMRCODE,=C'10'                                                   
         XC    WORK(25),WORK                                                    
         MVC   WORK(5),=X'1000000000'                                           
         MVC   WORK+5(1),PBUYKEDT                                               
         GOTO1 PUBEDIT,DMCB,(C'0',WORK),(0,WORK+10)                             
         MVC   PMEDTN,WORK+19                                                   
*                                                                               
         CLI   TRCODE,C'D'         SEE IF DELETE                                
         BNE   PB4X                                                             
*                                  IF DELETE ONLY DO 'NEW' DOLLARS              
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         ZAP   PMNGROSS,DUB                                                     
         S     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         ZAP   PMNGLCD,DUB                                                      
         MP    PMNGROSS,=P'-1'     MAKE NEGATIVE                                
         MP    PMNGLCD,=P'-1'                                                   
         B     PB30                GO WRITE RECORD                              
*                                                                               
*              ONLY DOES 2 CHARS OF EDTION                                      
*                                                                               
PB4X     ZAP   PMCOLS,=P'0'                                                     
         ZAP   PMNSPACE,=P'0'                                                   
         ZAP   PMCLE,=P'0'                                                      
         ZAP   PMCIE,=P'0'                                                      
*                                                                               
         CLI   PBDUIND,C'L'        LINES                                        
         BNE   PB5                                                              
         MVI   PMSTYPE,C'L'                                                     
         ZAP   PMNSPACE,PBDUNITS                                                
         B     PB20                                                             
*                                                                               
PB5      CLI   PBDUIND,X'89'   LOWER CASE I - INCHES 2 DECIMALS                 
         BNE   PB6                                                              
         MVI   PMSTYPE,C'C'                                                     
         ZAP   DUB,PBDUNITS        2 DECIMALS                                   
         MP    DUB,=P'10'                                                       
         CP    PBDCLMS,=P'0'       MAY NOT HAVE COLS IF XX.XXI INPUT            
         BE    PB6C                                                             
         ZAP   PMCIE,DUB           3 DECIMALS                                   
         ZAP   PMCOLS,PBDCLMS                                                   
         DP    DUB,PBDCLMS                                                      
         ZAP   PMNSPACE,DUB(6)     SO PMNSPACE X PMCOLS =PMCIE                  
         B     PB20                                                             
*                                                                               
PB6      CLI   PBDUIND,C'I'        INCHES NO DECIMALS                           
         BNE   PB8                                                              
         MVI   PMSTYPE,C'C'                                                     
         ZAP   DUB,PBDUNITS                                                     
         MP    DUB,=P'1000'        FOR 3 DECIMALS                               
PB6C     ZAP   PMNSPACE,DUB                                                     
         ZAP   PMCOLS,=P'1'                                                     
         ZAP   PMCIE,DUB                                                        
         B     PB20                                                             
*                                                                               
PB8      DS    0H                  PBDUIND MUST BE 0                            
         MVI   PMSTYPE,C'X'                                                     
PB20     MVC   PMNOTE(8),PBDSPACE    ALWAYS SAVE PBDSPACE                       
*                                  FOR SAU'S AND 10XFD ,ETC                     
         MVC   PMCOLOR,PBDCL                                                    
         ZAP   PMNRATE,=P'0'                                                    
         CLI   PBDCOSTY,C'U'       SEE IF I HAVE UNIT COST                      
         BNE   PB21                                                             
         ZAP   DUB,PBDCOS          5 DECIMALS                                   
         DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'5'      CHK REMAINDER                                
         BL    *+10                                                             
         AP    DUB(6),=P'1'                                                     
         ZAP   PMNRATE,DUB(6)      4 DECIMALS                                   
*                                                                               
PB21     ZAP   PMNCOST,=P'0'                                                    
         ZAP   PMNNET,=P'0'                                                     
         ZAP   PMNCOMM,=P'0'                                                    
         ZAP   PMNCD,=P'0'                                                      
         ZAP   PMNTAX,=P'0'                                                     
         ZAP   PMNTAXP,=P'0'                                                    
         MVI   PMNCMCD,C'1'        COMMISSION CODE SET TO 1                     
         ZAP   PMNCDP,=P'0'                                                     
*        COLOR PREMS                                                            
         ZAP   PMNCCOST,=P'0'                                                   
         ZAP   PMNCNET,=P'0'                                                    
         ZAP   PMNCCOMM,=P'0'                                                   
         ZAP   PMNCCD,=P'0'                                                     
         ZAP   PMNCTAX,=P'0'                                                    
*                                                                               
         ZAP   PMNCCOST,PBDPRCOS                                                
         ZAP   DUB,PBDPRCOS        COLOR GROSS                                  
*          DATA SET GETINS     AT LEVEL 040 AS OF 07/09/84                      
*                                                                               
         CVB   R1,DUB                                                           
         LR    RF,R1               PUT COLOR GROSS IN R1 AND RF                 
*                                  AGENCY COMMISSION                            
         CLI   PBDCOSIN,C'S'                                                    
         BE    PB24                NO AC                                        
         OC    PBDACP,PBDACP                                                    
         BZ    PB23                                                             
         CP    PBDACP,=P'-1'       = 100 PCT                                    
         BNE   PB23B                                                            
         SR    RF,RF               ZERO NET                                     
         B     PB24                                                             
PB23     DS    0H                                                               
         ZAP   PBDACP,=P'15000'                                                 
PB23B    DS    0H                                                               
         ZAP   DUB,PBDACP                                                       
         BZ    PB24                NO AC                                        
         CVB   R0,DUB                                                           
         L     RF,=F'100000'                                                    
         SR    RF,R0                                                            
         MR    RE,R1                                                            
         SLDL  RE,1                                                             
         D     RE,=F'100000'                                                    
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                RF=NET                                       
*                                                                               
PB24     DS    0H                                                               
         LR    R0,R1                                                            
         SR    R0,RF                                                            
         CVD   R0,DUB                                                           
         ZAP   PMNCCOMM,DUB        COMMISSION                                   
*                                                                               
* CASH DISCOUNT                                                                 
         ZAP   DUB,PBDCD                                                        
         CVB   RE,DUB                                                           
         MR    RE,RE                                                            
         SLDL  RE,1                                                             
         D     RE,=F'1000'                                                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         CVD   RF,DUB                                                           
         ZAP   PMNCCD,DUB          CASH DISC                                    
*                                                                               
         ZAP   PMNCNET,PMNCCOST                                                 
         SP    PMNCNET,PMNCCOMM    TO GET NET                                   
*                                                                               
*               TO GET VALUES FOR PMNCOST                                       
*              USE GETINS VALUES THEN SUBTRACT COLOR VALUES                     
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         ZAP   PMNCOST,DUB                                                      
         L     R0,AGYCOM                                                        
         CVD   R0,DUB                                                           
         ZAP   PMNCOMM,DUB                                                      
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         ZAP   PMNCD,DUB                                                        
         ZAP   PMNNET,PMNCOST                                                   
         SP    PMNNET,PMNCOMM      NET = GROSS - COMM                           
*                                                                               
*              NOW SUBTRACT COLOR VALUES                                        
         SP    PMNCOST,PMNCCOST                                                 
         SP    PMNNET,PMNCNET                                                   
         SP    PMNCOMM,PMNCCOMM                                                 
         SP    PMNCD,PMNCCD                                                     
*                                                                               
         ZAP   DUB,PBDCD           1 DECIMAL                                    
         MP    DUB,=P'10'                                                       
         ZAP   PMNCDP,DUB          2 DECIMALS                                   
*                                                                               
         CLI   PBDBFD,C'W'         CHK FOR WEEK OF                              
         BNE   *+8                                                              
         MVI   PMWOI,C'Y'                                                       
*                                                                               
         OC    PBDIDAT2,PBDIDAT2   CHK FOR SECOND INS DATE                      
         BZ    PB27                                                             
         GOTO1 DATCON,DMCB,(3,PBDIDAT2),(X'20',WORK)                            
         MVC   PMINSD2,WORK+2    ONLY MTH AND DAY                               
*                                                                               
PB27     MVC   PMNALLO(3),PPRDKPRD    USE PRODUCT I'M PROCESSING                
         ZAP   PMNALLO+3(3),=P'1000'                                            
         LA    R1,PMNALLO+9                                                     
         LA    R2,5                                                             
PB28     ZAP   0(3,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R2,PB28                                                          
*                                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZ    *+10                                                             
         MVC   PMNAD(6),PBDJOB                                                  
*                                                                               
         ZAP   PMNGROSS,PMNCOST                                                 
         AP    PMNGROSS,PMNCCOST     ADD COLOR GROSS                            
         ZAP   PMNGLCD,PMNGROSS                                                 
*                                  SUBTRACT BOTH CD'S                           
         SP    PMNGLCD,PMNCD                                                    
         SP    PMNGLCD,PMNCCD                                                   
*                                                                               
PB30     BAS   RE,WRITE                                                         
         B     PB80                                                             
*                                                                               
         EJECT                                                                  
PB40     DS    0H                  CHG NEWSPAPER BUYS FOR EST 003               
*                                  INTO MAGAZINE BUYS                           
         MVC   PBDCDATE,PBUYKDAT   SET CLOSING AND ON-SALE DATES                
         MVC   PBDSDATE,PBUYKDAT   TO INSERTION DATE                            
***                                                                             
PB50     DS    0H                  MAGAZINES,SUPP,TRADE,OUTDOOR                 
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(X'20',WORK)                            
         CLI   PBUYKMED,C'O'         SEE IF OUTDOOR                             
         BNE   PB50E                                                            
         MVC   PMOZONE(2),SPACES                                                
         CLI   PBUYKPUB+4,0       IF NO ZONE - LEAVE AS SPACES                  
         BE    PB50B                                                            
         ZIC   R0,PBUYKPUB+4       ZONE NUMBER                                  
         CVD   R0,DUB                                                           
         UNPK  PMOZONE,DUB+6(2)                                                 
         OI    PMOZONE+1,X'F0'                                                  
PB50B    MVC   PMOMTH(4),WORK+2    MTH AND DAY                                  
         BAS   RE,GETLIN                                                        
         MVC   PMOSUF,DUB                                                       
         MVC   PMRCODE,=C'30'                                                   
         B     PB50G                                                            
*                                                                               
PB50E    MVC   PMIMTH(4),WORK+2    MTH AND DAY                                  
         BAS   RE,GETLIN                                                        
         MVC   PMISUF,DUB                                                       
         MVC   PMRCODE,=C'20'                                                   
*                                                                               
PB50G    CLI   TRCODE,C'D'         SEE IF DOING DELETE                          
         BNE   PB55                                                             
         CLI   PBUYKMED,C'O'       SEE IF DOING OUTDOOR                         
         BNE   PB50M                                                            
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         ZAP   PMOGROSS,DUB                                                     
         S     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         ZAP   PMOGLCD,DUB                                                      
         MP    PMOGROSS,=P'-1'     MAKE NEGATIVE                                
         MP    PMOGLCD,=P'-1'                                                   
         B     PB75                                                             
*                                                                               
PB50M    L     R0,GROSS           MAGS (OR NEWSPAPER TREATED AS MAG)            
         CVD   R0,DUB                                                           
         ZAP   PMMGROSS,DUB                                                     
         S     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         ZAP   PMMGLCD,DUB                                                      
         MP    PMMGROSS,=P'-1'     MAKE NEGATIVE                                
         MP    PMMGLCD,=P'-1'                                                   
         CLI   PBUYKMED,C'N'       SEE IF NEWSPAPER BUY                         
         BNE   PB75                                                             
         MVI   PBUYKMED,C'M'       BEING TREATED AS A MAGAZINE                  
         B     PB75                EST = 003                                    
*                                                                               
PB55     L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         ZAP   PMMCOST,DUB                                                      
         L     R0,AGYCOM                                                        
         CVD   R0,DUB                                                           
         ZAP   PMMCOMM,DUB                                                      
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         ZAP   PMMCD,DUB                                                        
         ZAP   PMMNET,PMMCOST                                                   
         SP    PMMNET,PMMCOMM      NET = GROSS - COMM                           
*                                                                               
         ZAP   DUB,PBDCD           1 DECIMAL                                    
         MP    DUB,=P'10'                                                       
         ZAP   PMMCDP,DUB          2 DECIMALS                                   
         ZAP   PMMTAX,=P'0'                                                     
         ZAP   PMMTAXP,=P'0'                                                    
*                                                                               
         ZAP   DUB,PBDACP          3 DECIMALS                                   
         DP    DUB,=P'10'                                                       
         ZAP   PMMCOMP,DUB(6)      2 DECIMALS                                   
         CP    DUB+6(2),=P'5'                                                   
         BL    *+10                                                             
         AP    PMMCOMP,=P'1'                                                    
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(X'20',PMMIDAT)                         
*                                                                               
         CLI   PBUYKMED,C'O'          SEE IF OUTDOOR                            
         BE    PB58                   SKIP CLOSING AND ON-SALE                  
         GOTO1 (RF),(R1),(3,PBDCDATE),(X'20',PMMCDAT)                           
         GOTO1 (RF),(R1),(3,PBDSDATE),(X'20',PMMOSDAT)                          
*                                                                               
PB58     GOTO1 (RF),(R1),(3,PBDPDATE),(X'20',PMMPDAT)                           
         GOTO1 (RF),(R1),(3,PBDBDATE),(X'20',PMMBDAT)                           
         MVC   PMMBDAT+4(2),SPACES      NO DAY                                  
         CLC   PMMCDAT,ZEROS                                                    
         BNE   *+10                                                             
         MVC   PMMCDAT,SPACES                                                   
         CLC   PMMOSDAT,ZEROS                                                   
         BNE   *+10                                                             
         MVC   PMMOSDAT,SPACES                                                  
*                                                                               
         CLI   PBUYKMED,C'O'            SEE IF OUTDOOR                          
         BE    PB70                     REST IS DIFFERENT FROM MAGS             
*                                                                               
         ZAP   PMMCIRC,=P'0'                                                    
         BAS   RE,GETCIRC                                                       
         ZAP   PMMCIRC,DUB                                                      
         OC    PBDJOB,PBDJOB                                                    
         BZ    *+10                                                             
         MVC   PMMAD(6),PBDJOB                                                  
         MVC   PMMALLO(3),PPRDKPRD   USE THE PRODUCT I'M PROCESSING             
         ZAP   PMMALLO+3(3),=P'1000'                                            
         LA    R1,PMMALLO+9                                                     
         LA    R2,23                                                            
PB68     ZAP   0(3,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R2,PB68                                                          
*                                                                               
         CLI   PBUYKMED,C'N'       EST 003 NEWSPAPER INSERTION                  
         BNE   PB69                USE PPBYOUT TO GET SPACE                     
PB68C    L     R2,APPBYOWK                                                      
         USING PPBYOUTD,R2                                                      
         LA    R0,PBUYREC                                                       
         ST    R0,PBYOINPT                                                      
         MVC   PBYODTCN,DATCON                                                  
         LA    R0,GROSS                                                         
         ST    R0,PBYOVALS                                                      
         MVI   PBYOCTL,X'48'                                                    
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
***                                                                             
PB68D    MVI   PBUYKMED,C'M'       SET MEDIA TO MAGAZINES                       
***                                FOR NEWSPAPER BUY BEING TREATED              
***                                LIKE A MAG                                   
         MVC   PMMSPACE(20),PBYOSPC                                             
         CLI   PBYOSPC,C' '                                                     
         BH    *+10                                                             
         MVC   PMMSPACE(7),PBYOUNTS                                             
         CLI   PBYOPRM,C' '        CHK FOR PREMIUM                              
         BNH   PB69C                                                            
         MVC   PMMSPACE+14(11),PBYOPRM                                          
         B     PB69C                                                            
*                                                                               
         DROP  R2                                                               
*                                                                               
PB69     MVC   PMMSPACE(17),PBDSPACE                                            
PB69C    OC    PMMSPACE,SPACES                                                  
*                                                                               
         ZAP   PMMGROSS,PMMCOST                                                 
         ZAP   PMMGLCD,PMMCOST                                                  
         SP    PMMGLCD,PMMCD                                                    
         B     PB75                                                             
*                                                                               
PB70     DS    0H                   OUTDOOR FIELDS                              
         MVC   PMOALLO(3),PPRDKPRD   USE THE PRODUCT I'M PROCESSING             
         ZAP   PMOALLO+3(3),=P'1000'                                            
         LA    R1,PMOALLO+9                                                     
         LA    R2,23                                                            
PB70B    ZAP   0(3,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R2,PB70B                                                         
*                                                                               
         ZAP   PMOILL,PBDILLUM                                                  
         ZAP   PMOREG,PBDREG                                                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'66'                                                    
         BAS   RE,NEXTEL               LOOK FOR FIRST COMMENT ELEM              
         BNE   PB70F                   IF NONE FOUND PUT SHOWING                
         ZIC   R4,1(R2)                                                         
         SH    R4,=H'3'                                                         
         BM    PB70F                   NO DATA                                  
         CH    R4,=H'18'                                                        
         BM    *+8                                                              
         LH    R4,=H'18'               MAX I CAN HANDLE IS 19 BYTES             
         EX    R4,MVSPCE                                                        
         B     PB70K                                                            
*                                                                               
MVSPCE   MVC   PMOSPACE(0),2(R2)       EXECUTED                                 
*                                                                               
PB70F    CP    PBDSHOW,=P'0'           SEE IF I HAVE A SHOWING                  
         BE    PB70K                                                            
         MVC   PMOSPACE(5),=C'SHOW='                                            
         UNPK  PMOSPACE+5(3),PBDSHOW                                            
         OI    PMOSPACE+7,X'F0'                                                 
*                                                                               
PB70K    OC    PMOSPACE,SPACES                                                  
*                                                                               
         ZAP   PMOGROSS,PMMCOST                                                 
         ZAP   PMOGLCD,PMMCOST                                                  
         SP    PMOGLCD,PMMCD                                                    
         B     PB75                                                             
*                                                                               
PB75     BAS   RE,WRITE                                                         
         B     PB80                                                             
*                                                                               
         EJECT                                                                  
PB80     DS    0H                  BUFFALO FOR B12,B13 RECS                     
*                                  FOR 'ACTIVE BUYS'                            
*                                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'B'                                                     
         MVC   BUFMED,PBUYKMED                                                  
         MVC   BUFYR(4),PMEST      LAST DIGIT OF YEAR + EST                     
         MVC   BUFPUB,PMPUB                                                     
         ZAP   BUFACTS,=P'1'       SO I CAN DETECT CURRENT ACTIVITY             
PB81     CLI   PBUYKMED,C'N'                                                    
         BNE   PB82                                                             
*                                                                               
*        NOTE - NO SUFFIXES FOR NOW                                             
********                                                                        
******** MVC   BUFPSUF,PMPUBS                                                   
*******                                                                         
         ZAP   BUFCHGG,PMNGROSS    'CHANGE' TOTAL                               
         ZAP   BUFCHGCD,PMNGLCD                                                 
         ZAP   BUFTOTG,PMNGROSS     ALSO ADD TO PUB TOTAL                       
         ZAP   BUFTOTCD,PMNGLCD                                                 
*                                                                               
PB81B    DS    0H                                                               
         LA    RF,1(RC)             SO I CAN ADDRESS PUB FIELDS                 
         LA    RF,4095(RF)                                                      
         USING PPFILED+4096,RF                                                  
*                                                                               
         MVC   BUFCOM(2),PUBSTATE                                               
         MVC   BUFCOM+2(15),PUBCITY                                             
         MVC   BUFCOM+17(15),PUBNAME                                            
         OC    BUFCOM(32),SPACES                                                
         BAS   RE,GETCIRC                                                       
         ZAP   BUFCOM+32(5),DUB                                                 
*                                                                               
         B     PB85                                                             
*                                                                               
         DROP  RF                                                               
*                                                                               
PB82     DS    0H                                                               
         CLI   PBUYKMED,C'O'       SEE IF OUTDOOR                               
         BE    PB83                                                             
*                                                                               
         ZAP   BUFCHGG,PMMGROSS    'CHANGE' TOTAL                               
         ZAP   BUFCHGCD,PMMGLCD                                                 
         ZAP   BUFTOTG,PMMGROSS     ALSO ADD TO PUB TOTAL                       
         ZAP   BUFTOTCD,PMMGLCD                                                 
*                                                                               
PB82B    DS    0H                                                               
         LA    RF,1(RC)             SO I CAN ADDRESS PUB FIELDS                 
         LA    RF,4095(RF)                                                      
         USING PPFILED+4096,RF                                                  
*                                                                               
         MVC   BUFCOM(20),PUBNAME                                               
         OC    BUFCOM(20),SPACES                                                
         BAS   RE,GETCIRC                                                       
         ZAP   BUFCOM+20(4),DUB                                                 
         B     PB85                                                             
*                                                                               
         DROP  RF                                                               
*                                                                               
PB83     DS    0H                  OUTDOOR                                      
*                                                                               
*                                                                               
         ZAP   BUFCHGG,PMOGROSS    'CHANGE' TOTAL                               
         ZAP   BUFCHGCD,PMOGLCD                                                 
         ZAP   BUFTOTG,PMOGROSS     ALSO ADD TO PUB TOTAL                       
         ZAP   BUFTOTCD,PMOGLCD                                                 
         MVC   BUFPZONE,PMOZONE   ZONE NUMBER                                   
*                                                                               
PB83B    DS    0H                   GET HERE FROM PB120 IF NO ACTIVITY          
         LA    RF,1(RC)             SO I CAN ADDRESS PUB FIELDS                 
         LA    RF,4095(RF)                                                      
         USING PPFILED+4096,RF                                                  
*                                                                               
         MVC   BUFCOM(2),PUBSTATE                                               
         MVC   BUFCOM+2(15),PUBCITY                                             
         MVC   BUFCOM+17(15),PUBNAME                                            
         OC    BUFCOM(32),SPACES                                                
*                                                                               
         DROP  RF                                                               
*                                                                               
PB85     DS    0H                                                               
         CLI   ADDDEL,X'01'        SEE IF DOING ADD+DEL IN PERIOD               
         BE    PB88                YES DON'T CLEAR ORD'RD ON DELETES            
         CLI   TRCODE,C'D'                                                      
         BNE   PB88                                                             
         ZAP   BUFTOTG,=P'0'       CLEAR ORDERED ON DELETES                     
         ZAP   BUFTOTCD,=P'0'                                                   
*                                                                               
PB88     ZAP   SAVCGR,BUFCHGG   SAVE 'CHG' GROSS FOR A BUFFALO                  
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         XC    BUFPUB,BUFPUB       NOW DO BUFFALO FOR B13                       
         MVI   BUFPUB,X'FF'                                                     
         MVI   BUFPSUF,0                                                        
         XC    BUFPZONE,BUFPZONE                                                
         XC    BUFCOM,BUFCOM                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         B     PB200                                                            
*                                                                               
         EJECT                                                                  
PB100    DS    0H                  FOR BUYS WITH NO ACTIVITY                    
         TM    PBUYCNTL,X'80'      SEE IF DELETED                               
         BO    PB300               YES - GO CHK FOR CURRENT BILL ELEMS          
*                                  NO NEED TO POST TO B OR A BUFFRECS           
*                                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'B'                                                     
**                                                                              
*****    SPECIAL CODE TO MAKE NEWSPAPER BUYS                                    
*****    FOR EST 003 INTO MAGAZINE BUYS                                         
**                                                                              
         CLC   QAGENCY,=C'BS'      SPECIAL FOR BACKER                           
         BNE   PB110                                                            
*                                                                               
         CLI   PBUYKMED,C'N'                                                    
         BNE   PB110                                                            
         CLC   PBUYKEST,=X'0003'                                                
         BNE   *+8                                                              
         MVI   PBUYKMED,C'M'       CHG INTO MAGAZINE                            
*                                                                               
PB110    MVC   BUFMED,PBUYKMED                                                  
*                                                                               
         CLI   MYUSER,C'Y'          SEE IF DOING USER FIELDS                    
         BNE   PB110U                                                           
         MVC   BUFYR(4),ESTU1+21                                                
         B     PB120                                                            
*                                                                               
PB110U   DS    0H                                                               
         MVC   BUFYR,PESTST+1      LAST DIGIT OF YEAR                           
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFEST(3),DUB                                                    
*                                                                               
         CLI   QMEDIA,C'N'            DO NEWSPAPERS THE OLD WAY                 
         BE    PB120                                                            
*                                                                               
         CLI   QMEDIA,C'S'            DO SUPPLEMENTS THE OLD WAY                
         BE    PB120                                                            
*                                                                               
         CLI   QMEDIA,C'O'            DO OUTDOOR THE OLD WAY                    
         BE    PB120                                                            
*                                                                               
         CLC   PESTST(2),=C'92'     SEE IF BEFORE 1992                          
         BL    PB120                                                            
*                                                                               
         MVC   BUFYR(4),PESTCOM+2   USE STANDARD COMMENT CODE                   
         CLC   PESTCOM,SPACES                                                   
         BH    PB120                                                            
         MVC   P1,SPACES                                                        
         MVC   P1(36),=C'*** MISSING EST STANDARD COMMENT ***'                  
         MVC   P2(8),=C'PRD/EST='                                               
         MVC   P2+9(3),PESTKPRD                                                 
         MVI   P2+12,C'/'                                                       
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P2+13(3),DUB                                                     
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PB120                 CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
PB120    DS    0H                                                               
*                                  NO PRD FOR 'B' BUFFALO RECS                  
         UNPK  WORK(11),PBUYKPUB(6)                                             
         MVC   BUFPUB(8),WORK                                                   
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         ZAP   BUFTOTG,DUB                                                      
         L     R1,CSHDSC                                                        
         SR    R0,R1                                                            
         CVD   R0,DUB                                                           
         ZAP   BUFTOTCD,DUB                                                     
         ZAP   BUFCHGG,=P'0'                                                    
         ZAP   BUFCHGCD,=P'0'                                                   
         ZAP   BUFACTS,=P'0'       SET FOR NO CURRENT ACTIVITY                  
*                                                                               
         MVI   TRCODE,C'A'                                                      
*                                                                               
         CLI   PBUYKMED,C'N'                                                    
         BE    PB81B               REST SAME AS PB81B FOR NEWS                  
         CLI   PBUYKMED,C'O'       IF OUTDOOR                                   
         BNE   PB82B               NO - MUST BE MAGS                            
*                                  FOR OUTDOOR SET ZONE IN BUFPZONE             
         MVC   BUFPZONE,SPACES                                                  
         CLI   PBUYKPUB+4,0                                                     
         BE    PB83B                                                            
         ZIC   R0,PBUYKPUB+4       ZONE NUMBER                                  
         CVD   R0,DUB                                                           
         UNPK  BUFPZONE,DUB+6(2)                                                
         OI    BUFPZONE+1,X'F0'                                                 
         B     PB83B                                                            
*                                                                               
*                                  BUFFALO FOR A10 (NEWS) OR A20 (MAGS)         
         EJECT                                                                  
PB200    DS    0H                  NOW POST TO BUFF TYPE A                      
         CLI   ADDDEL,1            SEE IF DOING ADD AND DELETE                  
         BNE   PB205                                                            
*              DO NOT POST ADD + DEL TO 'A' BUFRECS                             
*                                                                               
         CLI   TRCODE,C'D'                                                      
         BE    PB300             SEE IF DOING DELETE - IF SO THEN DONE          
         MVI   TRCODE,C'D'                                                      
         B     PB4                 JUST DID ADD NOW DO DELETE                   
*                                                                               
PB205    XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'A'                                                     
         MVC   BUFMED,PBUYKMED                                                  
*                                                                               
         CLI   MYUSER,C'Y'           SEE IF USING USER FIELDS                   
         BNE   PB205U                                                           
         MVC   BUFYR(4),ESTU1+21                                                
         MVC   BUFCOM,SPACES                                                    
         MVC   BUFCOM(3),ESTU2+21                                               
         MVC   BUFCOM+20(12),PESTST      ALSO SAVE DATES                        
         CLI   QMEDIA,C'N'                                                      
         BE    PB205E                                                           
         CLI   QMEDIA,C'S'                                                      
         BE    PB205E                                                           
         CLI   QMEDIA,C'O'                                                      
         BE    PB205E                                                           
*                                                                               
*                          FOR MAGAZINES AND TRADE                              
*                          CANNOT USE ESTIMATE DATES                            
*                          SINCE RECORDS ARE TOTALED BY ON-SALE DATE            
*                                                                               
         MVC   BUFCOM+20(12),=C'900101901231'                                   
         MVC   BUFCOM+20+1(1),ESTU1+21   SET YEAR FROM USER FIELD               
         MVC   BUFCOM+20+7(1),ESTU1+21   SET YEAR FROM USER FIELD               
*                                                                               
* GO ADJUST (IF NECESSARY) HI-ORDER OF YEAR (BUFCOM+20 & BUFCOM+26)             
*                                                                               
         BAS   RE,PB2DATE                                                       
*                                                                               
         B     PB205E                                                           
*                                                                               
PB205U   DS    0H                                                               
         MVC   BUFCOM(20),PESTNAME                                              
*                                                                               
         MVC   BUFCOM+20(12),PESTST      ALSO SAVE DATES                        
*                                                                               
         MVC   BUFYR,PESTST+1      LAST DIGIT OF YEAR                           
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFEST(3),DUB                                                    
*                                                                               
         CLI   QMEDIA,C'N'            DO NEWSPAPERS THE OLD WAY                 
         BE    PB205E                                                           
*                                                                               
         CLI   QMEDIA,C'S'            DO SUPPLEMENTS THE OLD WAY                
         BE    PB205E                                                           
*                                                                               
         CLI   QMEDIA,C'O'            DO OUTDOOR THE OLD WAY                    
         BE    PB205E                                                           
*                                                                               
         CLC   PESTST(2),=C'92'   SEE IF BEFORE 1992                            
         BL    PB205E                                                           
*                                                                               
         MVC   BUFYR(4),PESTCOM+2                                               
         CLC   PESTCOM,SPACES                                                   
         BH    PB205UX                                                          
         MVC   P1,SPACES                                                        
         MVC   P1(36),=C'*** MISSING EST STANDARD COMMENT ***'                  
         MVC   P2(8),=C'PRD/EST='                                               
         MVC   P2+9(3),PESTKPRD                                                 
         MVI   P2+12,C'/'                                                       
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P2+13(3),DUB                                                     
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PB205UX               CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
PB205UX  DS    0H                                                               
*NOP*    LA    R2,PCONREC+33                                                    
         L     R2,ACONIO1          A(PCONREC)                                   
         LA    R2,33(R2)                                                        
         CLI   0(R2),X'40'                                                      
         BE    PB205B                                                           
         MVI   ELCODE1,X'40'                                                    
         BAS   RE,NEXTEL                                                        
         BE    PB205B                                                           
         DC    H'0'              BAD STANDARD COMMENT                           
*                                                                               
PB205B   DS    0H                                                               
         MVC   BUFCOM(20),2(R2)                                                 
         OC    BUFCOM(20),SPACES                                                
*                                                                               
         CLI   PESTKMED,C'N'     SEE IF NEWSPAPERS                              
         BE    PB205E                                                           
***      CLI   PESTKMED,C'O'     OR OUTDOOR    THIS WAS AN OLD CHECK            
***      BE    PB205E                                                           
         MVC   BUFCOM+20(12),=C'900101901231'                                   
         MVC   BUFCOM+20+1(1),PESTCOM+2   SET YEAR FROM COMMENT                 
         MVC   BUFCOM+20+7(1),PESTCOM+2                                         
*                                                                               
* GO ADJUST (IF NECESSARY) HI-ORDER OF YEAR (BUFCOM+20 & BUFCOM+26)             
*                                                                               
         BAS   RE,PB2DATE                                                       
*                                                                               
PB205E   DS    0H                                                               
         MVC   BUFPRD,PPRDKPRD   USE THE PRODUCT I'M PROCESSING                 
*                                                                               
         CLI   MYUSER,C'Y'          SEE IF USING USER FIELDS                    
         BNE   *+10                                                             
         MVC   BUFPRD,PRDU1+21                                                  
*                                                                               
         LA    R1,PBUYKDAT                                                      
*                                                                               
         CLI   QMEDIA,C'N'            DO NEWSPAPERS THE OLD WAY                 
         BE    PB207                                                            
*                                                                               
         CLI   QMEDIA,C'S'            DO SUPPLEMENTS THE OLD WAY                
         BE    PB207                                                            
*                                                                               
         CLI   QMEDIA,C'O'            DO OUTDOOR THE OLD WAY                    
         BE    PB207                                                            
*                                                                               
         CLC   PESTST(2),=C'92'   SEE IF BEFORE 1992                            
         BL    PB207                                                            
         CLI   PESTKMED,C'N'      SEE IF NEWAPAPERS                             
         BE    PB207                                                            
*                                                                               
         OC    PBDSDATE,PBDSDATE                                                
         BNZ   PB207X                                                           
         MVC   P1,SPACES                                                        
         MVC   P1(28),=C'*** MISSING ON-SALE DATE ***'                          
         MVC   P2(11),=C'PRD/EST/PUB'                                           
         MVC   P2+13(3),PBUYKPRD                                                
         MVI   P2+16,C'/'                                                       
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P2+17(3),DUB                                                     
*                                                                               
         MVI   P2+20,C'/'                                                       
         GOTO1 PUBEDIT,DMCB,(C'0',PBUYKPUB),(1,P2+21)                           
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(5,P2+35)                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PB207X                CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
PB207X   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,PBDSDATE),(0,WORK2)                               
         CLC   WORK2(6),BUFCOM+20                                               
         BL    PB207ERR                                                         
         CLC   WORK2(6),BUFCOM+20+6                                             
         BH    PB207ERR                                                         
         B     PB207XX                                                          
*                                                                               
PB207ERR MVC   P1,SPACES                                                        
         MVC   P1(34),=C'*** ON-SALE DATE OUT OF PERIOD ***'                    
         MVC   P2(11),=C'PRD/EST/PUB'                                           
         MVC   P2+13(3),PBUYKPRD                                                
         MVI   P2+16,C'/'                                                       
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P2+17(3),DUB                                                     
*                                                                               
         MVI   P2+20,C'/'                                                       
         GOTO1 PUBEDIT,DMCB,(C'0',PBUYKPUB),(1,P2+21)                           
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(5,P2+35)                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PB207XX               CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
PB207XX  DS    0H                                                               
         LA    R1,PBDSDATE         USE ON-SALE DATE                             
*                                                                               
PB207    DS    0H                                                               
         MVI   BUFQTR,X'01'                                                     
         CLI   1(R1),X'03'                                                      
         BNH   PB208                                                            
         MVI   BUFQTR,X'02'                                                     
         CLI   1(R1),X'06'                                                      
         BNH   PB208                                                            
         MVI   BUFQTR,X'03'                                                     
         CLI   1(R1),X'09'                                                      
         BNH   PB208                                                            
         MVI   BUFQTR,X'04'                                                     
PB208    MVC   BUFMTH,1(R1)         MONTH                                       
*                                                                               
         MVC   HALF,BUFQTR         SAVE FOR A13 REC                             
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         ZAP   BUFTOTG,DUB                                                      
         L     R1,CSHDSC                                                        
         SR    R0,R1                                                            
         CVD   R0,DUB                                                           
         ZAP   BUFTOTCD,DUB                                                     
         CLI   TRCODE,C'D'         SEE IF DELETED                               
         BNE   PB209                                                            
         ZAP   BUFTOTG,=P'0'       MAKE ZERO                                    
         ZAP   BUFTOTCD,=P'0'                                                   
*                                                                               
PB209    ZAP   BUFCHGG,SAVCGR                                                   
         ZAP   BUFCHGCD,=P'0'                                                   
         ZAP   BUFACTS,=P'0'                                                    
*                                                                               
PB210    GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFMTH,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFQTR,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         CLC   BUFPRD,SPACES                                                    
         BE    PB300               NOW DO BILLING DETAIL RECS                   
*              NOW DO BUFFALOS FOR A13(NEWS) OR A23(MAGS) RECORD                
*                                                                               
         MVC   BUFPRD,SPACES                                                    
*                                     LEAVE BUFCOM AS IT WAS                    
         MVC   BUFQTR(2),HALF         RESTORE QTR AND MONTH                     
         B     PB210                                                            
*                                                                               
*                ****************************************************           
*                *  SET HIGH-ORDER OF "TEST" YEAR FOR "USER" DATES  *           
*                ****************************************************           
PB2DATE  NTR1                                                                   
         ZIC   RE,TODAY1           HIGH-ORDER OF CURRENT YEAR                   
         CLI   TODAY1+1,C'0'       CURRENT YEAR ENDING IN 0 ?                   
         BNE   PB2D1               NO                                           
         CLI   BUFCOM+21,C'9'      USER YEAR 9 ?                                
         BE    PB2DSUB             YES - SET HIGH-ORDER OF YR "BACK" 1          
         CLI   BUFCOM+21,C'8'      USER YEAR 8 ?                                
         BE    PB2DSUB             YES                                          
         CLI   BUFCOM+21,C'7'      USER YEAR 7 ?                                
         BE    PB2DSUB             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2D1    CLI   TODAY1+1,C'1'       CURRENT YEAR ENDING IN 1 ?                   
         BNE   PB2D2               NO                                           
         CLI   BUFCOM+21,C'9'      USER YEAR 9 ?                                
         BE    PB2DSUB             YES                                          
         CLI   BUFCOM+21,C'8'      USER YEAR 8 ?                                
         BE    PB2DSUB             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2D2    CLI   TODAY1+1,C'2'       CURRENT YEAR ENDING IN 2 ?                   
         BNE   PB2D7               NO                                           
         CLI   BUFCOM+21,C'9'      USER YEAR 9 ?                                
         BE    PB2DSUB             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
*                                                                               
PB2D7    CLI   TODAY1+1,C'7'       CURRENT YEAR ENDING IN 7 ?                   
         BNE   PB2D8               NO                                           
         CLI   BUFCOM+21,C'0'      USER YEAR 0 ?                                
         BE    PB2DADD             YES - SET HIGH-ORDER OF YR "AHEAD" 1         
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2D8    CLI   TODAY1+1,C'8'       CURRENT YEAR ENDING IN 8 ?                   
         BNE   PB2D9               NO                                           
         CLI   BUFCOM+21,C'0'      USER YEAR 0 ?                                
         BE    PB2DADD             YES                                          
         CLI   BUFCOM+21,C'1'      USER YEAR 1 ?                                
         BE    PB2DADD             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2D9    CLI   TODAY1+1,C'9'       CURRENT YEAR ENDING IN 9 ?                   
         BNE   PB2DXIT             NO "ADJUSTMENT" NEEDED                       
         CLI   BUFCOM+21,C'0'      USER YEAR 0 ?                                
         BE    PB2DADD             YES                                          
         CLI   BUFCOM+21,C'1'      USER YEAR 1 ?                                
         BE    PB2DADD             YES                                          
         CLI   BUFCOM+21,C'2'      USER YEAR 2 ?                                
         BE    PB2DADD             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2DADD  LA    RE,1(RE)            SET HI-ORDER OF TEST YEAR "UP" 1             
         B     PB2DXIT             DONE                                         
PB2DSUB  BCTR  RE,0                SET HI-ORDER OF TEST YEAR "BACK" 1           
*****    B     PB2DXIT             DONE                                         
PB2DXIT  DS    0H                                                               
         STC   RE,BUFCOM+20        HI-ORDER OF FROM AND TO TEST DATES           
         STC   RE,BUFCOM+26                                                     
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
********  C10 (NEWS) OR C20 (MAGS)                                              
*                                                                               
PB300    DS    0H                  BILLING ELEMS                                
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         MVC   OUTREC+200(100),SPACES                                           
         MVC   OUTREC+300(100),SPACES                                           
         MVC   OUTREC+400(100),SPACES                                           
         USING PMRECD,R6                                                        
         MVC   PMAGENCY,MYAGY AGENCY CODE                                       
         MVC   PMMEDIA,=C'PN'                                                   
         CLI   PBUYKMED,C'N'                                                    
         BE    PB303                                                            
         MVC   PMMEDIA,=C'PM'                                                   
*                                                                               
         CLI   PBUYKMED,C'O'        SEE IF OUTDOOR                              
         BNE   PB303                                                            
         MVC   PMMEDIA,=C'OD'                                                   
*                                                                               
PB303    MVI   PMRTYPE,C'C'                                                     
         MVC   PMCLIENT,MYCLT      AGY/CLIENT                                   
*                                                                               
         MVC   PMEST(1),PESTST+1   LAST DIGIT OF YEAR                           
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMEST+1(3),DUB                                                   
*                                                                               
         CLI   MYUSER,C'Y'           SEE IF DOING USER FIELDS                   
         BNE   PB303U                                                           
         MVC   PMEST,ESTU1+21                                                   
         B     PB305                                                            
*                                                                               
PB303U   DS    0H                                                               
         CLI   QMEDIA,C'N'            DO NEWSPAPERS THE OLD WAY                 
         BE    PB305                                                            
*                                                                               
         CLI   QMEDIA,C'S'            DO SUPPLEMENTS THE OLD WAY                
         BE    PB305                                                            
*                                                                               
         CLI   QMEDIA,C'O'            DO OUTDOOR THE OLD WAY                    
         BE    PB305                                                            
*                                                                               
         CLC   PESTST(2),=C'92'    SEE IF BEFORE 1992                           
         BL    PB305               DO OLD WAY                                   
*                                                                               
         MVC   PMEST(4),PESTCOM+2  NEW WAY USES STANDARD COMMENT CODE           
         CLC   PESTCOM,SPACES                                                   
         BH    PB305                                                            
         MVC   P1,SPACES                                                        
         MVC   P1(36),=C'*** MISSING EST STANDARD COMMENT ***'                  
         MVC   P2(8),=C'PRD/EST='                                               
         MVC   P2+9(3),PESTKPRD                                                 
         MVI   P2+12,C'/'                                                       
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P2+13(3),DUB                                                     
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PB305                 CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
PB305    DS    0H                                                               
         UNPK  WORK(11),PBUYKPUB(6)                                             
         MVC   PMPUB(8),WORK                                                    
*                                                                               
         CLI   PBUYKMED,C'N'       MAGS DIFFERENT FROM HERE                     
         BNE   PB310                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(X'20',PMINSDAT)                        
         BAS   RE,GETLIN                                                        
         MVC   PMINSDS,DUB                                                      
         MVC   PMRCODE,=C'10'                                                   
         B     PB320                                                            
*                                                                               
PB310    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(X'20',WORK)                            
         CLI   PBUYKMED,C'O'         SEE IF OUTDOOR                             
         BNE   PB310E                                                           
         CLI   PBUYKPUB+4,0        CHECK FOR ZONE                               
         BE    PB310B              IF NONE - LEAVE AS SPACES                    
         ZIC   R0,PBUYKPUB+4       ZONE NUMBER                                  
         CVD   R0,DUB                                                           
         UNPK  PMOZONE,DUB+6(2)                                                 
         OI    PMOZONE+1,X'F0'                                                  
PB310B   MVC   PMOMTH(4),WORK+2    MTH AND DAY                                  
         BAS   RE,GETLIN                                                        
         MVC   PMOSUF,DUB                                                       
         MVC   PMRCODE,=C'30'                                                   
         B     PB320                                                            
*                                                                               
PB310E   MVC   PMIMTH(4),WORK+2    MTH AND DAY                                  
         BAS   RE,GETLIN                                                        
         MVC   PMISUF,DUB                                                       
         MVC   PMRCODE,=C'20'                                                   
*                                                                               
PB320    LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'26'       FIND BILLING ELEMS                           
PB325    BAS   RE,NEXTEL                                                        
         BNE   EXIT                FINALLY FINISHED WITH BUY                    
         USING PBILELEM,R2                                                      
         CLC   PBPRD,PPRDKPRD     PRODUCT MUST MATCH THE ONE I'M DOING          
         BNE   PB325                                                            
         OC    PBLDATE,PBLDATE                                                  
         BZ    PB325                                                            
         CLC   PBLDATE,SVQSTART    SEE IF BILLED IN PERIOD                      
         BL    PB325                                                            
         CLC   PBLDATE,SVQEND                                                   
         BH    PB325                                                            
         MVC   FULL,PBGROSS                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         ZAP   PMIGRS,DUB                                                       
         MVC   FULL,PBCSHDSC                                                    
         L     R1,FULL                                                          
         SR    R0,R1                                                            
         CVD   R0,DUB                                                           
         ZAP   PMIGLCD,DUB                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBLDATE),(0,WORK)                                 
         GOTO1 =V(PPFMTINO),DMCB,WORK,(2,PBINVNO),                     X        
               (QMEDIA,B1PROF),B1XPROF                                          
         L     RF,DMCB                                                          
         MVC   PMIINVNO(10),0(RF)                                               
*                                                                               
****     MVC   PMIINVNO(1),QMEDIA  ***MUST USE 'REAL' MEDIA***                  
****     MVC   PMIINVNO+1(4),=C'-  -'                                           
****     ZIC   R0,PBLDATE+1        MONTH                                        
****     CVD   R0,DUB                                                           
****     OI    DUB+7,X'0F'                                                      
****     UNPK  PMIINVNO+2(2),DUB                                                
****     MVC   HALF,PBINVNO                                                     
****     LH    R0,HALF                                                          
****     CVD   R0,DUB                                                           
****     OI    DUB+7,X'0F'                                                      
****     UNPK  PMIINVNO+5(4),DUB                                                
*                                                                               
         BAS   RE,WRITE                                                         
         B     PB325               GO DO NEXT ELEM                              
*                                                                               
         EJECT                                                                  
PUTBUFF  DS    0H      FIRST PRINT TOTAL LINE FOR CURRENT INVS                  
         TM    CINVSW,1                                                         
         BZ    PUTB2                                                            
         MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
         MVC   P1+28(7),=C'*TOTAL*'                                             
         EDIT  (P8,CINVGRS),(14,P1+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVBIL),(14,P1+53),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVRCV),(14,P1+85),2,COMMAS=YES,FLOAT=-                     
         MVI   P1+51,C'*'                                                       
         MVI   P1+67,C'*'                                                       
         MVI   P1+83,C'*'                                                       
         MVI   P1+99,C'*'                                                       
         BAS   RE,MYRPT                                                         
PUTB2    MVI   FORCEHED,C'Y'                                                    
*                                  PUT BUFFALO RECS TO TAPE                     
*                                  AT LBUYREQ                                   
*                                  FIRST DO B12 AND B13 RECORDS                 
         ZAP   GTTOTG,=P'0'        FOR GRAND TOTALS                             
         ZAP   GTTOTCD,=P'0'                                                    
         ZAP   GTCHGG,=P'0'                                                     
         ZAP   GTCHGCD,=P'0'                                                    
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         MVC   OUTREC+200(100),SPACES                                           
         MVC   OUTREC+300(100),SPACES                                           
         MVC   OUTREC+400(100),SPACES                                           
         USING PMRECD,R6                                                        
         MVC   PMAGENCY,MYAGY AGENCY CODE                                       
         MVC   PMMEDIA,=C'PN'                                                   
         CLI   QMEDIA,C'N'                                                      
         BE    PUTB4                                                            
         MVC   PMMEDIA,=C'PM'                                                   
         CLI   QMEDIA,C'O'          SEE IF OUTDOOR                              
         BNE   PUTB4                                                            
         MVC   PMMEDIA,=C'OD'                                                   
*                                                                               
PUTB4    MVI   PMRTYPE,C'B'                                                     
         MVC   PMCLIENT,MYCLT                                                   
*                                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'B'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     PUTB10                                                           
PUTB5    GOTO1 BUFFALO,DMCB,=C'SEQ',(C'B',BUFFBUFF),BUFREC,0                    
*                                                                               
PUTB10   CLI   DMCB+8,X'80'        END OF B'S                                   
         BE    PUTB100             GO DO A10 AND A13                            
         MVC   PMEST,BUFYR         LAST DIGIT OF YEAR + EST                     
         CLI   BUFPUB,X'FF'       SEE IF DOING PUB TOTAL                        
         BE    PUTB30              NO                                           
         MVC   PMPUB,BUFPUB                                                     
         CLI   BUFPSUF,0                                                        
         BE    *+10                                                             
         MVC   PMPUBS,BUFPSUF                                                   
         MVC   PMRCODE,=C'12'                                                   
         CLI   BUFMED,C'N'                                                      
         BE    PUTB12                                                           
         MVI   PMRCODE,C'2'        MAGS                                         
*                                                                               
         CLI   BUFMED,C'O'                                                      
         BNE   PUTB12                                                           
         MVI   PMRCODE,C'3'                                                     
         MVC   PMOZONE,BUFPZONE                                                 
*                                                                               
PUTB12   ZAP   PMPGROSS,BUFCHGG                                                 
         ZAP   PMPGLCD,BUFCHGCD                                                 
         CLI   BUFMED,C'N'                                                      
         BNE   PUTB20                                                           
         ZAP   PMPNTGRS,BUFTOTG                                                 
         ZAP   PMPNTGCD,BUFTOTCD                                                
         MVC   PMPNST(2),BUFCOM         STATE CODE                              
         MVC   PMPNCITY(15),BUFCOM+2     CITY                                   
         MVC   PMPNNAM(15),BUFCOM+17      PUB NAME                              
         MVC   PMPNCIRC,BUFCOM+32          CIRC                                 
         B     PUTB20X                                                          
*                                                                               
PUTB20   DS    0H                                                               
         CLI   BUFMED,C'O'             SEE IF OUTDOOR                           
         BNE   PUTB20M                                                          
         MVC   PMPOST,BUFCOM           STATE                                    
         MVC   PMPOCITY(15),BUFCOM+2   CITY                                     
         MVC   PMPONAM,BUFCOM+17       FIRST 11 CHARS OF PUB NAME               
         ZAP   PMPOTGRS,BUFTOTG                                                 
         ZAP   PMPOTGCD,BUFTOTCD                                                
         B     PUTB20X                                                          
*                                                                               
PUTB20M  MVC   PMPNAME(20),BUFCOM      PUB NAME                                 
         MVC   PMPCIRC,BUFCOM+20   CIRC                                         
         ZAP   PMPTGRS,BUFTOTG                                                  
         ZAP   PMPTGLCD,BUFTOTCD                                                
*                                                                               
PUTB20X  CP    BUFACTS,=P'0'   CHK FOR CURRENT ACTIVITY                         
         BE    PUTB22          NO  - THEN DON'T WRITE B12 OR B22 REC            
         BAS   RE,WRITE                                                         
PUTB22   BAS   RE,DISPLAY                                                       
         B     PUTB5               GO TO SEQ                                    
*                                                                               
PUTB30   DS    0H                  FOR EST TOTALS B13 OR B23                    
         MVC   PMRCODE,=C'13'                                                   
         CLI   BUFMED,C'N'                                                      
         BE    PUTB32                                                           
         MVI   PMRCODE,C'2'        MAGS                                         
*                                                                               
         CLI   BUFMED,C'O'             SEE IF OUTDOOR                           
         BNE   PUTB32                                                           
         MVI   PMRCODE,C'3'                                                     
*                                                                               
PUTB32   MVC   PMPUB(9),SPACES                                                  
         MVC   PMPGROSS(100),SPACES    CLEAR PUB NAME ,ETC                      
         ZAP   PMPGROSS,BUFCHGG                                                 
         ZAP   PMPGLCD,BUFCHGCD                                                 
         ZAP   PMETGRS,BUFTOTG                                                  
         ZAP   PMETGLCD,BUFTOTCD                                                
         BAS   RE,WRITE                                                         
         BAS   RE,DISPLAY                                                       
         B     PUTB5               GO DO SEQ READ                               
*                                                                               
         EJECT                                                                  
         DS    0F                                                               
DISPLAY  NTR1                                                                   
         MVI   RCSUBPRG,20         FOR EST/PUB RECAP                            
         MVC   P1+2(4),PMEST                                                    
         MVC   P1+8(11),=C'*EST TOTAL*'                                         
         CLI   BUFPUB,X'FF'        SEE IF EST TOTAL                             
         BE    DPLAY5                                                           
         MVC   P1+8(8),PMPUB                                                    
         MVC   P1+16(3),SPACES                                                  
         MVC   P1+20(20),PMPNAME                                                
         CLI   BUFMED,C'M'                                                      
         BE    DPLAY5                                                           
         CLI   BUFMED,C'S'                                                      
         BE    DPLAY5                                                           
         CLI   BUFMED,C'T'                                                      
         BE    DPLAY5                                                           
         CLC   PMOZONE,SPACES                                                   
         BE    DPLAY3                                                           
         MVI   P1+16,C'-'                                                       
         MVC   P1+17(2),PMOZONE       ZONE                                      
*                                                                               
DPLAY3   MVC   P1+20(20),SPACES                                                 
         MVC   P1+20(2),PMPOST     STATE                                        
         MVI   P1+22,C','                                                       
         MVC   P1+23(L'PMPOCITY),PMPOCITY                                       
         MVC   P1+40(L'PMPONAM),PMPONAM                                         
         CLI   BUFMED,C'O'          SEE IF OUTDOOR                              
         BE    DPLAY5                                                           
*                                  MUST BE NEWSPAPERS                           
         MVC   P1+16(3),SPACES                                                  
         MVC   P1+20(20),SPACES                                                 
         MVC   P1+20(2),PMPNST     STATE                                        
         MVI   P1+22,C','                                                       
         MVC   P1+23(15),PMPNCITY                                               
         MVC   P1+40(15),PMPNNAM                                                
DPLAY5   EDIT  BUFTOTG,(14,P1+56),2,COMMAS=YES,FLOAT=-                          
         EDIT  BUFTOTCD,(14,P1+73),2,COMMAS=YES,FLOAT=-                         
         EDIT  BUFCHGG,(14,P1+91),2,COMMAS=YES,FLOAT=-                          
         EDIT  BUFCHGCD,(14,P1+108),2,COMMAS=YES,FLOAT=-                        
         CLI   BUFPUB,X'FF'                                                     
         BNE   DPLAY8                                                           
         AP    GTTOTG,BUFTOTG                                                   
         AP    GTTOTCD,BUFTOTCD                                                 
         AP    GTCHGG,BUFCHGG                                                   
         AP    GTCHGCD,BUFCHGCD                                                 
         MVI   P1+70,C'*'                                                       
         MVI   P1+87,C'*'                                                       
         MVI   P1+105,C'*'                                                      
         MVI   P1+122,C'*'                                                      
         MVI   SPACING,2                                                        
DPLAY8   DS    0H                                                               
         BAS   RE,MYRPT                                                         
         XIT1                                                                   
         EJECT                                                                  
PUTB100  DS    0H                  FOR A10 AND A13 RECORDS                      
*                                                                               
         MVC   P1+2(16),=C'**REPORT TOTAL**'                                    
         EDIT  GTTOTG,(14,P1+56),2,COMMAS=YES,FLOAT=-                           
         EDIT  GTTOTCD,(14,P1+73),2,COMMAS=YES,FLOAT=-                          
         EDIT  GTCHGG,(14,P1+91),2,COMMAS=YES,FLOAT=-                           
         EDIT  GTCHGCD,(14,P1+108),2,COMMAS=YES,FLOAT=-                         
         MVI   P1+70,C'*'                                                       
         MVI   P1+87,C'*'                                                       
         MVI   P1+105,C'*'                                                      
         MVI   P1+122,C'*'                                                      
         MVI   P1+71,C'*'                                                       
         MVI   P1+88,C'*'                                                       
         MVI   P1+106,C'*'                                                      
         MVI   P1+123,C'*'                                                      
         MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
         MVC   P1+2(14),=C'UNLOCKED ESTS='                                      
         OC    ESTLLST(2),ESTLLST                                               
         BNZ   PUTB102                                                          
         MVC   P1+17(4),=C'NONE'                                                
         B     PUTB102X                                                         
*                                                                               
PUTB102  LA    R1,P1+17                                                         
         LA    R2,ESTLLST                                                       
PUTB102B OC    0(2,R2),0(R2)                                                    
         BZ    PUTB102X                                                         
         LH    R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
         OC    2(2,R2),2(R2)                                                    
         BZ    PUTB102X                                                         
         MVI   3(R1),C','                                                       
         LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         B     PUTB102B                                                         
*                                                                               
PUTB102X MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         MVC   OUTREC+200(100),SPACES                                           
         MVC   OUTREC+300(100),SPACES                                           
         MVC   OUTREC+400(100),SPACES                                           
         USING PMRECD,R6                                                        
         MVC   PMAGENCY,MYAGY AGENCY CODE                                       
         MVC   PMMEDIA,=C'PN'                                                   
         CLI   QMEDIA,C'N'                                                      
         BE    PUTB103                                                          
         MVC   PMMEDIA,=C'PM'                                                   
*                                                                               
         CLI   QMEDIA,C'O'              SEE IF OUTDOOR                          
         BNE   PUTB103                                                          
         MVC   PMMEDIA,=C'OD'                                                   
*                                                                               
PUTB103  MVI   PMRTYPE,C'A'                                                     
         MVC   PMCLIENT,MYCLT                                                   
*                                                                               
         XC    BUFREC,BUFREC                                                    
         XC    LASTREC,LASTREC                                                  
         MVI   BUFTYPE,C'A'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     PUTB110                                                          
PUTB105  GOTO1 BUFFALO,DMCB,=C'SEQ',(C'A',BUFFBUFF),BUFREC,0                    
*                                                                               
PUTB110  CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    PUTB200             GO DO A10 AND A13                            
         CLI   BUFTYPE,C'A'                                                     
         BNE   PUTB200             END OF A'S                                   
         CLI   LASTREC,0                                                        
         BNE   PUTB112                                                          
         MVC   LASTREC(8),BUFREC                                                
         B     PUTB113                                                          
PUTB112  CLC   BUFREC(8),LASTREC                                                
         BE    PUTB120                                                          
         CLI   LASTREC,X'FF'       END OF A'S                                   
         BE    *+10                                                             
         MVC   LASTREC,BUFREC                                                   
         BAS   RE,WRITE                                                         
         CLI   LASTREC,X'FF'                                                    
         BE    EXIT                                                             
*                                                                               
PUTB113  MVC   PMEST,BUFYR         LAST DIGIT OF YEAR + EST                     
         MVC   PMPRD,BUFPRD                                                     
         MVC   PMRCODE,=C'10'                                                   
         CLC   BUFPRD,SPACES                                                    
         BNE   *+8                                                              
         MVI   PMRCODE+1,C'3'      EST TOTAL                                    
         CLI   BUFMED,C'N'                                                      
         BE    PUTB114                                                          
         MVI   PMRCODE,C'2'        MAGS                                         
*                                                                               
         CLI   BUFMED,C'O'             SEE IF OUTDOOR                           
         BNE   PUTB114                                                          
         MVI   PMRCODE,C'3'                                                     
*                                                                               
PUTB114  MVC   PMACCT(4),BUFCOM       ACCOUNT NUMBER                            
*                                                                               
PUTB114D MVC   PMESTD(20),BUFCOM       EST NAME (INCLUDES ACCT NO.)             
*NOP*    MVC   PMSTART(12),BUFCOM+20   EST START AND END                        
         GOTO1 DATCON,DMCB,(0,BUFCOM+20),(X'20',PMSTART)    EST START           
         GOTO1 DATCON,DMCB,(0,BUFCOM+26),(X'20',PMEND)      EST END             
*NOP*PUTB114X MVC   PMTDATE,TODAY1                                              
PUTB114X GOTO1 DATCON,DMCB,(0,TODAY1),(X'20',PMTDATE)                           
*NOP*    MVC   PMRDATE,SVSTART          REVISION DATE - PERIOD START            
         GOTO1 DATCON,DMCB,(0,SVSTART),(X'20',PMRDATE)                          
         LA    R2,PMMONTHS                                                      
         LA    R1,51                                                            
PUTB115  ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R1,PUTB115                                                       
*                                                                               
PUTB120  L     R2,ADISPTAB         FIND DISPLACEMENT INTO PMMONTHS              
         LA    R1,17               FOR BCT                                      
PUTB125  CLC   BUFQTR(2),0(R2)                                                  
         BE    PUTB130                                                          
         LA    R2,4(R2)                                                         
         BCT   R1,PUTB125                                                       
*                                                                               
         CLI   QOPT6,C'Y'          SEE IF TESTING                               
         BE    PUTB130             THEN DON'T DIE                               
         DC    H'0'                FATAL ERROR                                  
*                                                                               
PUTB130  LA    R1,PMMONTHS                                                      
         LH    R0,2(R2)                                                         
         AR    R1,R0               ADD DISPLACEMENT                             
         CP    0(6,R1),=P'0'       NOTHING SHOULD BE HERE                       
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CP    6(6,R1),=P'0'                                                    
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CP    12(6,R1),=P'0'                                                   
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         ZAP   0(6,R1),BUFTOTG                                                  
         ZAP   12(6,R1),BUFTOTCD                                                
         ZAP   6(6,R1),BUFCHGG                                                  
         B     PUTB105                                                          
*                                                                               
PUTB200  CLI   LASTREC,0           SEE IF I DID ANY RECORDSS                    
         BE    EXIT                NO - THEN DONE                               
         MVI   LASTREC,X'FF'       GO FINISH LAST REC                           
         B     PUTB112                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
TOTALS   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         L     R4,ATITLES                                                       
         L     R3,ALENTAB                                                       
         LA    R6,26               FOR BCT                                      
TOT2     MVC   P1+7(17),0(R4)                                                   
         EDIT  (P4,5(R3)),(9,P1+26),0,COMMAS=YES                                
         BAS   RE,MYRPT                                                         
         LA    R4,17(R4)                                                        
         LA    R3,9(R3)                                                         
         BCT   R6,TOT2                                                          
         BAS   RE,MYRPT            SKIP A LINE                                  
         MVC   P1+7(13),=C'TOTAL RECORDS'                                       
         EDIT  TOTCNT,(9,P1+26),0,COMMAS=YES                                    
         MVI   P1+35,C'*'                                                       
         BAS   RE,MYRPT                                                         
*                                                                               
         CLI   TAPESW,C'Y'          SEE IF PRODUCING A TAPE                     
         BNE   EXIT                                                             
*                                                                               
         CLOSE (OUTFILE,)                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                  GET LATEST CIRCULATION                       
         DC    F'0'                                                             
GETCIRC  ST    RE,GETCIRC-4                                                     
         LA    RF,1(RC)             SO I CAN ADDRESS PUB FIELDS                 
         LA    RF,4095(RF)                                                      
         USING PPFILED+4096,RF                                                  
*                                                                               
         LA    R2,PUBREC+33                                                     
         USING PUBCIREL,R2                                                      
         MVI   ELCODE1,X'30'                                                    
         XC    CIRCDAT,CIRCDAT     USED TO SAVE DATE                            
         ZAP   DUB,=P'0'                                                        
GETC5    BAS   RE,NEXTEL                                                        
         BNE   GETCX                                                            
         CLC   CIRCDAT,0                                                        
         BE    GETC10                                                           
         CLC   CIRCDAT,PUBCDAT                                                  
         BH    GETC5                                                            
GETC10   MVC   CIRCDAT,PUBCDAT                                                  
         ZAP   DUB,PUBCIR1                                                      
         B     GETC5                                                            
*                                                                               
GETCX    L     RE,GETCIRC-4                                                     
         BR    RE                                                               
*                                                                               
         DROP  RF                                                               
         DROP  R2                                                               
         EJECT                                                                  
         DC    F'0'                                                             
GETLIN   ST    RE,GETLIN-4                                                      
         LA    R1,PBUYKLIN    IF LINE EXCEEDS 36                                
*                             THEN USE BINARY VALUE                             
         CLI   PBUYKLIN,36                                                      
         BH    GETLIN5                                                          
         ZIC   R0,PBUYKLIN    ELSE USE ALPHA-NUMERIC                            
         LA    R1,LINTAB      FROM LINTAB                                       
         AR    R1,R0                                                            
GETLIN5  MVC   DUB(1),0(R1)                                                     
*                                                                               
         L     RE,GETLIN-4                                                      
         BR    RE                                                               
*                                                                               
LINTAB   DC    C'01234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ'                         
*                                                                               
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE1,0(R2)                                                    
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
         DC    F'0'                                                             
WRITE    ST    RE,WRITE-4          FIND RECORD LENGHT IN LENTAB                 
*                                                                               
         MVC   SAVMED,PMMEDIA      SAVE 'REAL' MEDIA                            
*                                                                               
*                                                                               
*                                  MUST MAKE THIS EST INTO MAGAZINE             
         CLC   QAGENCY,=C'BS'       SPECIAL CHECK FOR BACKER                    
         BNE   WRITE2                                                           
*                                                                               
         CLC   PMEST+1(3),=C'003'   SPECIAL CODE FOR EST 003                    
         BNE   WRITE2                                                           
         CLI   QMEDIA,C'N'         AND NEWSPAPERS                               
         BNE   WRITE2                                                           
         MVC   PMMEDIA,=C'PM'      CALL MAGAZINE FOR PHILIP MORRIS              
         MVI   PMRCODE,C'2'        USE MAGAZINE REC CODE                        
*                                                                               
*                                                                               
WRITE2   MVC   WORK(1),PMRTYPE                                                  
         MVC   WORK+1(2),PMRCODE                                                
         L     R1,ALENTAB                                                       
WRITE4   CLC   0(3,R1),WORK                                                     
         BE    WRITE5                                                           
         LA    R1,9(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   WRITE4                                                           
         DC    H'0'                UNKNOWN TYPE                                 
*                                                                               
WRITE5   MVC   HALF,3(R1)                                                       
         AP    5(4,R1),=P'1'                                                    
         LH    R3,HALF                                                          
         LA    R3,4(R3)                                                         
         STH   R3,OUTREC-4                                                      
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
         MVC   P1(125),OUTREC                                                   
         MVC   P2(125),OUTREC+125                                               
         MVC   P3(125),OUTREC+250                                               
         OI    P2,X'01'                                                         
         OI    P3,X'01'                                                         
         GOTO1 HEXOUT,DMCB,OUTREC-4,P4+10,54,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+50,P5+18,50,=C'N'                               
         GOTO1 (RF),(R1),OUTREC+100,P6+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+150,P7+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+200,P8+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+250,P9+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+300,P10+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+350,P11+18,50,=C'N'                             
         MVC   P4+1(7),=C'001-050'                                              
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC-4                                                      
         PUT   (1),(0)                                                          
WRIT3    AP    TOTCNT,=P'1'                                                     
         MVC   PMMEDIA,SAVMED      MUST RESTORE 'REAL' MEDIA                    
         L     RE,WRITE-4                                                       
         BR    RE                                                               
*                                                                               
         DS    0F                                                               
MYRPT    NTR1                                                                   
         MVC   QSTART(12),SVSTART      RESTORE FOR HEADLINES                    
         CLC   SVEND,=6X'FF'       FOR 21ST CENTURY                             
         BNE   MYRPT5                                                           
         MVC   QEND,SPACES                                                      
MYRPT5   GOTO1 REPORT                                                           
         MVC   QSTART(12),SPACES                                                
MYRPTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        AGENCY TABLE                                                           
*                                                                               
AGYTAB   DC    C'BS',C'310774',C'BSPM',C'N'                                     
         DC    C'YN',C'331543',C'YNPM',C'Y'                                     
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=VB,LRECL=408,             X        
               BLKSIZE=4084,MACRF=PM                                            
         EJECT                                                                  
INVOICE  CSECT                                                                  
         NMOD1 0,INVOICE                                                        
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R5, R7, R8, R9, RA                         
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
BILL2    DS    0H                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         MVC   OUTREC+200(100),SPACES                                           
         MVC   OUTREC+300(100),SPACES                                           
         MVC   OUTREC+400(100),SPACES                                           
         USING PMRECD,R6                                                        
         MVC   PMAGENCY,MYAGY AGENCY CODE                                       
         MVC   PMMEDIA,=C'PN'                                                   
         CLI   QMEDIA,C'N'                                                      
         BE    BILL2B                                                           
         MVC   PMMEDIA,=C'PM'                                                   
         CLI   QMEDIA,C'O'          SEE IF OUTDOOR                              
         BNE   BILL2B                                                           
         MVC   PMMEDIA,=C'OD'                                                   
*                                                                               
BILL2B   MVI   PMRTYPE,C'C'                                                     
         MVC   PMCLIENT,MYCLT                                                   
         MVC   PMEST(1),PESTST+1   LAST DIGIT OF YEAR                           
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMEST+1(3),DUB                                                   
*                                                                               
         CLI   MYUSER,C'Y'          SEE IF USING USER FIELDS                    
         BNE   BILL2C                                                           
         MVC   PMEST,ESTU1+21                                                   
         B     BILL2D                                                           
*                                                                               
BILL2C   DS    0H                                                               
         CLI   QMEDIA,C'N'            DO NEWSPAPERS THE OLD WAY                 
         BE    BILL2D                                                           
*                                                                               
         CLI   QMEDIA,C'S'            DO SUPPLEMENTS THE OLD WAY                
         BE    BILL2D                                                           
*                                                                               
         CLI   QMEDIA,C'O'            DO OUTDOOR THE OLD WAY                    
         BE    BILL2D                                                           
*                                                                               
         CLC   PESTST(2),=C'92'       SEE IF 1992 OR LATER                      
         BL    BILL2D                                                           
         MVC   PMEST(4),PESTCOM+2     USE STANDARD COMMENT CODE                 
*                                                                               
         CLC   PESTCOM,SPACES                                                   
         BH    *+6                                                              
         DC    H'0'                   MISSING STANDARD COMMENT                  
*                                                                               
BILL2D   MVC   PMPRD+1(3),PBILKPRD    ONLY FOR C12 OR C22 REC                   
*                                                                               
         CLI   MYUSER,C'Y'         IF USING USER FIELDS                         
         BNE   *+10                                                             
         MVC   PMPRD+1(3),PRDU1+21                                              
*                                     PUT PRD IN COL 27 NOT 26                  
         MVC   PMRCODE,=C'12'      FIRST DO PRD BILL                            
         CLI   PBILKMED,C'N'                                                    
         BE    BILL2D5                                                          
         MVI   PMRCODE,C'2'                                                     
         CLI   PBILKMED,C'O'      SEE IF OUTDOOR                                
         BNE   BILL2D5                                                          
         MVI   PMRCODE,C'3'        3 FOR OUTDOOR                                
*                                                                               
BILL2D5  ZAP   PMIGRS,PBILLGRS     GROSS                                        
         ZAP   PMIGLCD,PBILLBIL    GROSS LESS CD                                
*                                                                               
         MVC   PMIINVNO(10),DINVFULL    FULL INV NUMBER FROM PPFMTINO           
*                                                                               
****     MVC   PMIINVNO(1),PBILKMED                                             
****     MVC   PMIINVNO+1(4),=C'-  -'                                           
****     ZIC   R0,PBILKBMN+1       MONTH                                        
****     CVD   R0,DUB                                                           
****     OI    DUB+7,X'0F'                                                      
****     UNPK  PMIINVNO+2(2),DUB                                                
****     MVC   HALF,PBILKBNO                                                    
****     LH    R0,HALF                                                          
****     CVD   R0,DUB                                                           
****     OI    DUB+7,X'0F'                                                      
****     UNPK  PMIINVNO+5(4),DUB                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBILINVD),(X'20',PMIDATE)                         
         GOTO1 DATCON,DMCB,(3,PBILDUED),(X'20',PMIDDATE)                        
*                                                                               
         CLI   QOPT5,C'Y'          SEE IF LISTING CUR INVOICES                  
         BNE   POSTB8                                                           
         MVI   RCSUBPRG,10                                                      
         MVC   P1+3(3),PBILKPRD                                                 
         MVC   P1+8(3),=C'000'                                                  
         OC    PBILKEST,PBILKEST   SEE IF EST=000                               
         BZ    INVR10                                                           
         ZIC   R0,PBILKEST                                                      
         SLL   R0,8                                                             
         IC    R0,PBILKEST+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+8(3),DUB                                                      
*                                                                               
INVR10   DS    0H                                                               
         CLI   PBILKMOS+1,12       FUNNY BILLING PERIODS                        
         BNH   INVR15                                                           
         ZIC   R0,PBILKMOS+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+14(2),DUB                                                     
         MVI   P1+16,C'/'                                                       
         ZIC   R0,PBILKMOS                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+17(2),DUB                                                     
         B     INVR17                                                           
*                                                                               
INVR15   GOTO1 DATCON,DMCB,(3,PBILKMOS),(6,P1+14)                               
INVR17   DS    0H                                                               
         CLI   PBILLTYP,C'0'                                                    
         BH    INVR18                                                           
         MVC   P1+23(2),PBILLTYP                                                
         B     INVR20                                                           
*                                                                               
INVR18   MVC   P1+23(2),=C'S '                                                  
         CLI   PBILLTYP,C'4'                                                    
         BNE   INVR20                                                           
         MVI   P1+23,C'D'                                                       
INVR20   DS    0H                                                               
         MVC   P1+28(10),DINVFULL                                               
*                                                                               
****     ZIC   R0,PBILKBMN+1       MONTH                                        
****     CVD   R0,DUB                                                           
****     OI    DUB+7,X'0F'                                                      
****     UNPK  P1+28(2),DUB                                                     
****     MVI   P1+30,C'-'                                                       
****     MVC   HALF,PBILKBNO                                                    
****     LH    R0,HALF                                                          
****     CVD   R0,DUB                                                           
****     OI    DUB+7,X'0F'                                                      
****     UNPK  P1+31(4),DUB                                                     
*                                                                               
         ZAP   MYBILLCD,PBILLGRS                                                
         SP    MYBILLCD,PBILLBIL                                                
         EDIT  (P6,PBILLGRS),(14,P1+37),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P6,PBILLBIL),(14,P1+53),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P8,MYBILLCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P6,PBILLRCV),(14,P1+85),2,COMMAS=YES,FLOAT=-                    
         CLI   PBILLTYP,C'B'                                                    
         BE    INVR25                                                           
         CLI   PBILLCAN,0                                                       
         BE    INVR25                                                           
         CLC   PBILLCAN,=6C'0'                                                  
         BE    INVR25                                                           
         MVC   P1+101(12),=C'(REVERSED BY'                                      
         MVC   P1+114(2),PBILLCAN                                               
         MVI   P1+116,C'-'                                                      
         MVC   P1+117(4),PBILLCAN+2                                             
         MVI   P1+121,C')'                                                      
         B     INVR28                                                           
*                                                                               
INVR25   GOTO1 DATCON,DMCB,(3,PBILINVD),(5,P1+101)                              
         GOTO1 DATCON,DMCB,(3,PBILDUED),(5,P1+111)                              
INVR28   BAS   RE,MYRPT                                                         
*                                                                               
*                                  ROLL TO CURRENT INV TOTALS                   
INVR30   AP    CINVGRS,PBILLGRS    NET OR NET/NET                               
         AP    CINVBIL,PBILLBIL    GROSS OR GROSS-CD                            
         AP    CINVCD,MYBILLCD                                                  
         AP    CINVRCV,PBILLRCV                                                 
         OI    CINVSW,1                                                         
*                                                                               
POSTB8   DS    0H                                                               
         EJECT                                                                  
BILL4    DS    0H                                                               
         BAS   RE,WRITE                                                         
*                                                                               
*              NOW DO C13 RECORD                                                
*                                                                               
         MVC   PMPRD+1(3),SPACES       SAME AS C12 OR C22 BUT NO PRD            
*                                      PRODUCT WAS IN COL 27 NOT 26             
         MVC   PMRCODE,=C'13'                                                   
         CLI   PBILKMED,C'N'                                                    
         BE    BILL7                                                            
         MVI   PMRCODE,C'2'         MAGS,SUPP,TRADE                             
         CLI   PBILKMED,C'O'        SEE IF OUTDOOR                              
         BNE   BILL7                                                            
         MVI   PMRCODE,C'3'                                                     
*                                                                               
BILL7    BAS   RE,WRITE                                                         
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
         L     RC,PPFILEC                                                       
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R5, R7, R8, R9, RA                         
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         MVI   BILLONLY,C'N'                                                    
*                                                                               
         DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
         MVC   PPGAREC,AREC                                                     
         MVI   CKESTSW,0    WILL BE SET TO X'01' IF I READ SOMETHING            
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBUYREC                                                   
         MVI   KEY+3,X'07'         MUST READ EST FOR START YEAR                 
         MVC   KEY+4(6),PBUYREC+4   CLT AND PRD                                 
         MVC   KEY+10(2),PBUYREC+19   EST                                       
         XC    KEY+12(19),KEY+12                                                
         CLI   CKESTREC,C'B'        FROM BUY                                    
         BE    CKEST3                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(3),PBILLREC                                                  
         MVI   KEY+3,X'07'         MUST READ EST FOR START YEAR                 
         MVC   KEY+4(6),PBILLREC+4  CLT AND PRD                                 
         MVC   KEY+10(2),PBILKEST     EST                                       
         XC    KEY+12(19),KEY+12                                                
         CLI   CKESTREC,C'L'        FROM BILL                                   
         BE    CKEST3                                                           
         DC    H'0'                 ERROR                                       
*                                                                               
CKEST3   CLC   PESTREC(12),KEY      SEE IF I ALREADY HAVE EST                   
         BE    CKEST5                                                           
         MVI   CKESTSW,1                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                EST MUST BE ON FILE                          
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         BAS   RE,ESTF            ADD TO ESTTABLE                               
*                                                                               
CKEST5   DS    0H                                                               
         CLC   QAGENCY,=C'YN'     SPECIAL CHECK FOR YNR                         
         BNE   CKEST5C                                                          
         CLI   PESTGRPS+1,C'B'    BILL ONLY ESTIMATE                            
         BNE   CKEST5C            (NO CHANGES)                                  
         MVI   BILLONLY,C'Y'                                                    
*                                                                               
CKEST5C  DS    0H                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
*                                                                               
         CLI   MYUSER,C'Y'         SEE IF USER FIELDS IN USE                    
         BNE   CKEST50                                                          
*                                                                               
         CLC   PESTKPRD,=C'AAA'     NOT FOR PRD AAA                             
         BE    CKEST50                                                          
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',PCLTREC),(C'E',PESTREC),ESTU1,ESTU2          
         CLI   DMCB,X'FF'                                                       
         BE    UESTERR                                                          
         CLI   ESTU1+21,C' '    MUST FIND DATA                                  
         BNH   UESTERR                                                          
         CLI   ESTU2+21,C' '    MUST FIND DATA                                  
         BNH   UESTERR                                                          
         B     CKEST50                                                          
*                                                                               
UESTERR  DS    0H                                                               
         LA    R1,BADESTS                                                       
         MVC   WORK(3),PPRDKPRD                                                 
         MVC   WORK+3(2),PESTKEST                                               
UESTERR2 CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    UESTERR3                                                         
         CLC   WORK(5),0(R1)                                                    
         BE    CKEST50                                                          
         LA    R1,6(R1)                                                         
         B     UESTERR2                                                         
*                                                                               
UESTERR3 MVC   0(5,R1),WORK                                                     
         MVI   5(R1),0                                                          
         MVI   6(R1),X'FF'        SET NEW END OF TABLE                          
*                                                                               
         MVC   P1(35),=C'*** MISSING ESTIMATE USER FIELD ***'                   
         MVC   P1+40(3),PPRDKPRD                                                
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+45(3),DUB                                                     
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT6,C'Y'           SEE IF TEST RUN                             
         BE    CKEST50                                                          
         MVC   P2(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
*                                                                               
CKEST50  DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
         XC    PCONREC(200),PCONREC                                             
         CLC   PESTCOM,SPACES                                                   
         BNH   CKEST80                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   WORK2+6(6),PESTCOM                                               
         MVC   WORK2(6),SPACES                                                  
         LA    R2,WORK2+6                                                       
         CLI   5(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVC   KEY+4(6),0(R2)                                                   
         CLC   PCONREC(10),KEY       SEE IF ALREADY THERE                       
         BE    CKEST80                                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   CKESTSW,1                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MISSING STANDARD COMMENT                     
*                                                                               
*NOP*    LA    R0,PCONREC          READ INTO CONTRACT AREA                      
*NOP*    ST    R0,AREC                                                          
         MVC   AREC,ACONIO1        ACONIO1=A(PCONREC)                           
         GOTO1 GETPRT                                                           
*                                                                               
CKEST80  MVC   KEY,PPGKEY                                                       
         MVC   AREC,PPGAREC                                                     
         CLI   CKESTSW,1           SEE IF I READ SOMETHING                      
         BNE   CKESTX              NO - SKIP READ HIGH                          
         GOTO1 HIGH                                                             
CKESTX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
ESTF     NTR1                                                                   
         CLI   PESTSTAT,C'1'                                                    
         BE    ESTFX                                                            
         LA    R1,250                                                           
         LA    R2,ESTLLST                                                       
ESTF5    CLC   0(2,R2),PESTKEST                                                 
         BE    ESTFX                                                            
         OC    0(2,R2),0(R2)                                                    
         BNZ   ESTF10                                                           
         MVC   0(2,R2),PESTKEST                                                 
         B     ESTFX                                                            
*                                                                               
ESTF10   LA    R2,2(R2)                                                         
         BCT   R1,ESTF5                                                         
         DC    H'0'                TOO MANY UNLOCKED ESTS                       
*                                                                               
ESTFX    XIT                                                                    
         LTORG                                                                  
CKESTSW  DC    X'00'                                                            
*****                                                                           
TITLES   CSECT                                                                  
         DS    0C                                                               
         DC    CL17'NEWSPAPERS A10'                                             
         DC    CL17'           A13'                                             
         DC    CL17'           B10'                                             
         DC    CL17'           B12'                                             
         DC    CL17'           B13'                                             
         DC    CL17'           C10'                                             
         DC    CL17'           C12'                                             
         DC    CL17'           C13'                                             
         DC    CL17'              '    TO SKIP A LINE                           
         DC    CL17'MAGAZINES  A20'                                             
         DC    CL17'           A23'                                             
         DC    CL17'           B20'                                             
         DC    CL17'           B22'                                             
         DC    CL17'           B23'                                             
         DC    CL17'           C20'                                             
         DC    CL17'           C22'                                             
         DC    CL17'           C23'                                             
*                                                                               
         DC    CL17'              '    TO SKIP A LINE                           
         DC    CL17'OUTDOOR    A30'                                             
         DC    CL17'           A33'                                             
         DC    CL17'           B30'                                             
         DC    CL17'           B32'                                             
         DC    CL17'           B33'                                             
         DC    CL17'           C30'                                             
         DC    CL17'           C32'                                             
         DC    CL17'           C33'                                             
*                                                                               
         EJECT                                                                  
*                                                                               
DISPTAB  CSECT                                                                  
         DC    X'0101',H'0'        JAN                                          
         DC    X'0102',H'18'                                                    
         DC    X'0103',H'36'                                                    
         DC    X'01FF',H'54'       1ST QUARTER                                  
         DC    X'0204',H'72'                                                    
         DC    X'0205',H'90'                                                    
         DC    X'0206',H'108'                                                   
         DC    X'02FF',H'126'      2ND QUARTER                                  
         DC    X'0307',H'144'                                                   
         DC    X'0308',H'162'                                                   
         DC    X'0309',H'180'                                                   
         DC    X'03FF',H'198'      3RD QUARTER TOTAL                            
         DC    X'040A',H'216'                                                   
         DC    X'040B',H'234'                                                   
         DC    X'040C',H'252'                                                   
         DC    X'04FF',H'270'      4TH QUARTER                                  
         DC    X'FFFF',H'288'      GRAND TOTAL                                  
*                                                                               
         EJECT                                                                  
*                                  TABLE OF RECORD LENGHTS                      
LENTAB   CSECT                                                                  
         DC    CL3'A10',AL2(PMA10X-PMKEY),PL4'0'                                
         DC    CL3'A13',AL2(PMA13X-PMKEY),PL4'0'                                
         DC    CL3'B10',AL2(PMB10X-PMKEY),PL4'0'                                
         DC    CL3'B12',AL2(PMB12X-PMKEY),PL4'0'                                
         DC    CL3'B13',AL2(PMB13X-PMKEY),PL4'0'                                
         DC    CL3'C10',AL2(PMC10X-PMKEY),PL4'0'                                
         DC    CL3'C12',AL2(PMC12X-PMKEY),PL4'0'                                
         DC    CL3'C13',AL2(PMC13X-PMKEY),PL4'0'                                
         DC    XL5'00',PL4'0'      EXTRA LINE                                   
* MAGS                                                                          
         DC    CL3'A20',AL2(PMA20X-PMKEY),PL4'0'                                
         DC    CL3'A23',AL2(PMA23X-PMKEY),PL4'0'                                
         DC    CL3'B20',AL2(PMB20X-PMKEY),PL4'0'                                
         DC    CL3'B22',AL2(PMB22X-PMKEY),PL4'0'                                
         DC    CL3'B23',AL2(PMB23X-PMKEY),PL4'0'                                
         DC    CL3'C20',AL2(PMC20X-PMKEY),PL4'0'                                
         DC    CL3'C22',AL2(PMC22X-PMKEY),PL4'0'                                
         DC    CL3'C23',AL2(PMC23X-PMKEY),PL4'0'                                
         DC    XL5'00',PL4'0'      EXTRA LINE                                   
* OUTDOOR                                                                       
         DC    CL3'A30',AL2(PMA30X-PMKEY),PL4'0'                                
         DC    CL3'A33',AL2(PMA33X-PMKEY),PL4'0'                                
         DC    CL3'B30',AL2(PMB30X-PMKEY),PL4'0'                                
         DC    CL3'B32',AL2(PMB32X-PMKEY),PL4'0'                                
         DC    CL3'B33',AL2(PMB33X-PMKEY),PL4'0'                                
         DC    CL3'C30',AL2(PMC30X-PMKEY),PL4'0'                                
         DC    CL3'C32',AL2(PMC32X-PMKEY),PL4'0'                                
         DC    CL3'C33',AL2(PMC33X-PMKEY),PL4'0'                                
         DC    X'FFFF'                                                          
PPPMWRKD DSECT                                                                  
TOTCNT   DS    PL4'0'                                                           
*                                                                               
B1PROF   DS    CL16                                                             
B1XPROF  DS    CL16                                                             
*                                                                               
DINVFULL DS    CL10                                                             
*                                                                               
WORK2    DS    CL64                                                             
ADISPTAB DS    A                                                                
ALENTAB  DS    A                                                                
ATITLES  DS    A                                                                
VGETUSER DS    A                                                                
ACONIO1  DS    A                                                                
*                                                                               
MYAGY    DS    CL6       SET FROM AGYTAB AT FBUYREQ                             
MYCLT    DS    CL4       SET FROM AGYTAB AT FBUYREQ                             
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
*                                                                               
ESTU1    DS    CL54      USER FIELDS                                            
ESTU2    DS    CL38                                                             
PRDU1    DS    CL54                                                             
*                                                                               
MYBILLCD DS    PL8                                                              
*                                                                               
CINVGRS  DS    PL8        CURRENT INVOICE TOTALS                                
CINVBIL  DS    PL8                                                              
CINVCD   DS    PL8                                                              
CINVRCV  DS    PL8                                                              
CINVSW   DS    CL1                                                              
*                                                                               
ADDDEL   DS    CL1                 X'01' IF ADDED + DELETED IN PERIOD           
BILLONLY DS    CL1                 SET IN CKEST Y= BILL ONLY ESTIMATE           
*                                  (NO CHANGES)                                 
SAVMED   DS    CL2                 USED IN WRITE TO SAVE 'REAL' MEDIA           
CKESTREC DS    CL1                                                              
*                                                                               
GTTOTG   DS    PL8                 REPORT TOTALS                                
GTTOTCD  DS    PL8                                                              
GTCHGG   DS    PL8                                                              
GTCHGCD  DS    PL8                                                              
*                                                                               
TODAY1   DS    CL6                                                              
ELCODE1  DS    CL1                                                              
SAVCGR   DS    PL8                                                              
*                                                                               
TAPESW   DS    CL1         STARTS AS 0 CHANGED TO N OR Y AT FBUYCLI             
*                          MIX NOT ALLOWED                                      
*                                                                               
SVQSTART DS    CL3                                                              
SVQEND   DS    CL3                                                              
SVSTART  DS    CL6                                                              
SVEND    DS    CL6                                                              
BQS      DS    XL3                                                              
BQE      DS    XL3                                                              
CIRCDAT  DS    CL3                                                              
TRCODE   DS    CL1                                                              
PPGKEY   DS    CL32                                                             
PPGAREC  DS    CL4                                                              
ZEROS    DS    CL30                                                             
LASTREC  DS    CL8                                                              
APPBYOWK DS    A                                                                
BUFFIO   DS    A                                                                
BUFFBUFF DS    A                                                                
         DS    F                                                                
*                                                                               
BUFREC   DS    0CL103                                                           
BUFKEY   DS    0CL23                                                            
BUFTYPE  DS    CL1                 A OR B                                       
BUFMED   DS    CL1                 MEDIA                                        
BUFYR    DS    CL1                 LAST DIGIT OF YEAR                           
BUFEST   DS    CL3                 EST                                          
BUFPRD   DS    CL3                 PRODUCT                                      
BUFPUB   DS    CL8                 PUB (FOR B TYPE ONLY)                        
BUFPSUF  DS    CL1                 SUFFIX (FOR NEWS - NOT USED YET)             
BUFPZONE DS    CL2                 OUTDOOR ZONE                                 
BUFQTR   DS    CL1                 QUARTER NO.   (FOR A TYPE ONLY)              
*              BUFQTR=X'FF' FOR GRAND TOTAL                                     
*              ELSE BINARY 01-04                                                
BUFMTH   DS    CL1                 MONTH (FOR A TYPE ONLY)                      
*              BUFYM=X'FF'  FOR QUARTER TOTAL                                   
         DS    CL1                 SPARE                                        
*                                                                               
BUFCOM   DS    CL40                COMMENT                                      
*                                                                               
*                                                                               
*              THESE FIELDS FOR  TYPE B BUFFALO RECS                            
*                       AND FOR  TYPE A BUFFALO RECS                            
*                                                                               
BUFCHGG  DS    PL8                 TOTAL OF B10'S OR B20'S  GROSS               
BUFCHGCD DS    PL8                 TOTAL OF B10'S OR B20'S  GROSS-CD            
BUFTOTG  DS    PL8                 FILE TOTAL - GROSS                           
BUFTOTCD DS    PL8                 FILE TOTAL - GROSS - CD                      
BUFACTS  DS    PL8                                                              
         ORG                                                                    
         DS    F                                                                
OUTREC   DS    CL500                                                            
*                                                                               
*                                                                               
ESTLLST  DS    CL500               LIST OF UNLOCKED ESTS (2 BYTES PER)          
*                                                                               
BADESTS  DS    CL240            ROOM FOR 40 BAD PRD/ESTS                        
*                                PRD(3)/EST(2)/+ ONE BYTE                       
*                                TO BE USED FOR ERRORS                          
*                                                                               
         DS    CL50             SPARE                                           
*                                                                               
         BUFF  LINES=4000,ROWS=1,COLUMNS=5,FLAVOR=PACKED,COMMENT=40,KEYX        
               LIST=(23,A)                                                      
*                                                                               
PPBYOWRK CSECT                                                                  
         DS    CL600                                                            
         DC    X'0000'                                                          
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PNNEWFILE         HAVE NEW DSECT FOR PBILLREC                  
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PMINTFCO                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037PPREPST02 04/23/03'                                      
         END                                                                    
