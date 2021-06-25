*          DATA SET REINV20    AT LEVEL 143 AS OF 05/01/02                      
*PHASE T80320A,+0                                                               
         TITLE 'T80320 - REPPAK MULTIPLE INVOICE DISPLAY MODULE'                
*                                                                               
*- REINV20 -- PHASE T80320                                                      
**********************************************************************          
*  HISTORY OF CHANGES                                                *          
**********************************************************************          
*  08/24/89  PJS  CHANGED PHASE TO 'A' LEVEL                                    
*                                                                               
*  10/25/89  PJS  IF REP PROFILE 3 IS 'Y', DEFAULT TO DIS-ADV ACTION            
*                                                                               
*  10/30/89  PJS  ADDED CONTRACT TYPE FILTER                                    
*                                                                               
*  13FEB90   EFJ  FIX TO PRINT PROD NAME IF PROD CODE USED IN CONHEAD           
*                                                                               
*  JUN01/92  BU   CHANGE EDIT PATTERN IF $ > 1MEG                               
*                                                                               
*  JUL25/94  SKU  SKIP DISPLAY OF FORECAST CONTRACTS                            
*                                                                               
*  AUG16/94  SKU  REP PROFILE CONTROLLED DOUBLE DISPLAY LINE                    
*                                                                               
* FEB16/95  BU   'TOTALS ONLY' REQUEST                                          
*                                                                               
* AUG 15 1995     J RYAN INVOICE                                                
*              REINVFF   SCREEN CHANGED TO ADD TP OR TYPE IN HEADER             
*                                                                               
*  24MAY96   SMP  NEW DIFFERENCE COLUMN                                         
*                                                                               
* 17JUL96   BU   EXPAND 'ORDERED' COLUMN                                        
*                                                                               
* 18JUN97   BU   FIX PROBLEM WITH BAD D/A WHEN PAGE > 40                        
*                                                                               
* 31JUL97  RHV   TEMP FIX TO RELEASE LOCKED RECS IN TOTALS SEQ LOOP             
*                                                                               
* 19MAY00  BU    ADD TRADE PROCESSING                                           
*                                                                               
* 10APR02  SKU   FIX INVOICE # DISPLAY BUG                                      
*                                                                               
*                ***  END TOMBSTONE  ***                                        
**********************************************************************          
T80320   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80320,R7                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T803FFD,RA                                                       
         USING LINED,R2                                                         
         EJECT                                                                  
* CLEAR OUT ACCUMULATORS                                                        
         XC    ORDTOT,ORDTOT                                                    
         XC    INVTOT,INVTOT                                                    
         SPACE 1                                                                
* DISPLAY EXPANSION HEADER NAME                                                 
         FOUT  INVNAMEH,=CL13'PRODUCT NAME'                                     
**                                         CL18 WAS CL20 BEFORE TP              
**                                           TP WAS ADDED TO SCREEN             
         CLC   INVACT+4(3),=C'PRO'         EXPLICIT PRODUCT DISPLAY             
         BE    D10                                                              
         CLC   INVACT+4(3),=C'AGE'                                              
         BNE   *+14                                                             
         MVC   INVNAME,=CL13'AGENCY NAME'                                       
         B     D10                                                              
         CLC   INVACT+4(3),=C'ADV'                                              
         BNE   *+14                                                             
D05      MVC   INVNAME,=CL13'ADVERTISER NM'                                     
         B     D10                                                              
         CLC   INVACT+4(3),=C'OFF'                                              
         BNE   *+14                                                             
         MVC   INVNAME,=CL13'OFFICE NAME'                                       
         B     D10                                                              
         CLC   INVACT+4(3),=C'SAL'                                              
         BNE   *+14                                                             
         MVC   INVNAME,=CL13'SALESPERSON'                                       
         B     D10                                                              
*                                                                               
*- IF REP PROFILE 3 IS 'Y', DEFAULT TO DIS-ADV                                  
         CLI   SVREPROF+2,C'Y'                                                  
         BNE   D07                                                              
         MVC   INVACT+3(4),=C'-ADV'                                             
         FOUT  INVACTH                                                          
         B     D05                 DISPLAY ADV LITERAL                          
*                                                                               
* PRODUCT IS DEFAULT                                                            
D07      MVC   INVACT+3(4),=C'-PRO'                                             
         FOUT  INVACTH                                                          
*                                                                               
* CLEAR AND FOUT NON-ZERO FIELDS                                                
D10      LA    R2,INVLN01H         FIRST LINE HEADER                            
         SR    R4,R4                                                            
D15      IC    R4,0(R2)            LEN                                          
         SH    R4,=H'9'                                                         
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BZ    D20                                                              
         FOUT  (R2)                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
D20      IC    R4,0(R2)            FIELD LEN                                    
         AR    R2,R4               NEXT FIELD                                   
         LA    RF,INVLNXH                                                       
         CR    R2,RF               LAST?                                        
         BNH   D15                                                              
         MVI   TWADISP,0                                                        
*                                                                               
         MVI   LOCKCNT,0           INITALIZE                                    
*                                                                               
         CLI   TWAPFKEY,PFPGUP     PAGE UP?                                     
         BNE   D25                                                              
         L     RF,=AL4(TWAPDA-T803FFD)  RESOLVE A(TWAPDA)                       
         AR    RF,RA                                                            
         ZIC   R1,TWAPAGE                                                       
         BCTR  R1,0                                                             
         MH    R1,=H'4'                                                         
         AR    RF,R1                                                            
         MVC   KEY+28(4),0(RF)       D/A FROM TABLE                             
         BAS   RE,GETREC                                                        
         MVC   TWAKEY,IOAREA                                                    
         MVC   KEY,TWAKEY                                                       
         B     D28                                                              
*                                                                               
D25      DS    0H                                                               
         MVC   KEY,TWAKEY                                                       
         SR    R5,R5                                                            
         IC    R5,KEY+26                                                        
         LA    R5,1(R5)                                                         
         STC   R5,KEY+26                                                        
D28      DS    0H                                                               
         BAS   RE,HIGH                                                          
         B     *+8                                                              
D30      BAS   RE,SEQ              ANY CONTRACTS?                               
         CLC   KEY(11),KEYSAVE     SAME STATION?                                
         BNE   LAST10                                                           
         OC    INVOFF(2),INVOFF    IF OFFICE FILTER                             
         BZ    D32                                                              
         CLC   KEY(13),KEYSAVE     SAME OFFICE?                                 
         BNE   LAST10                                                           
D32      OC    TWAAGYF,TWAAGYF       AGY FILTER?                                
         BZ    D35                   NO - PROCEED                               
         CLC   TWAAGYF(4),KEY+13     MATCH AGY?                                 
         BNE   D30                   NO - NEXT REC                              
         CLC   TWAAGYF+4(2),SPACES   HAVE AGY OFC FILTER?                       
         BE    D35                   NO - PROCEED                               
         CLC   KEY+17(2),TWAAGYF+4   MATCH AGY OFC?                             
         BNE   D30                   NO - NEXT REC                              
*                                                                               
* GET CONTRACT                                                                  
D35      BAS   RE,GETREC                                                        
         TM    IOAREA+29,X'80'     DELETE?                                      
         BO    D30                                                              
*                                                                               
*- CONTRACT TYPE FILTER (OPTIONAL)                                              
         CLI   INVTYPEH+5,0                                                     
         BE    D39                 NO TYPE FILTER ENTERED                       
*                                                                               
         LA    RF,RCONTYPE-RCONREC                                              
         A     RF,AIOAREA                                                       
*                                                                               
         CLI   INVTYPE,C'*'        EXCLUDE?                                     
         BE    D37                                                              
*                                                                               
         CLC   0(1,RF),INVTYPE  INCLUDE ONLY MATCHING TYPE                      
         BNE   D30                                                              
         B     D39                                                              
*                                                                               
D37      CLC   0(1,RF),INVTYPE+1   EXCLUDE MATCHING TYPE                        
         BE    D30                                                              
                                                                                
* IF CONTRACT IS A FORECAST, SKIP AND GET NEXT                                  
D39      EQU   *                                                                
         BAS   RE,IS4CAST          IS THIS A FORECAST CONTRACT?                 
         BZ    D30                 YES, SKIP IT                                 
                                                                                
* CHECK FOR ORDERED OR INVOICE DOLLARS IN REQUESTED MONTH                       
         GOTO1 BUCKETS,DMCB,RCONREC,DOLLARS                                     
         CLI   DMCB,X'FF'          ACCOUNTING                                   
         BE    D40                                                              
         OC    DOLLARS,DOLLARS     DOLLARS?                                     
         BZ    D30                                                              
         EJECT                                                                  
* REQUESTED PAGE - DISPLAY CONTRACTS                                            
D40      LA    R8,WORK2            KEY LIST                                     
         LA    R9,TWACLIST         DISK ADDR LIST                               
         LA    R2,INVLN01H         FIRST OUTPUT LINE                            
*                                                                               
         L     RF,=AL4(TWAPDA-T803FFD)  RESOLVE A(TWAPDA)                       
         AR    RF,RA                                                            
         ZIC   R1,TWAPAGE                                                       
         BCTR  R1,0                                                             
         MH    R1,=H'4'                                                         
         AR    RF,R1                                                            
         MVC   0(4,RF),KEY+28      D/A INTO THIS PG SPOT IN TABLE               
*                                                                               
         XC    TWACLIST,TWACLIST                                                
         XC    WORK2(250),WORK2                                                 
         XC    WORK2+250(100),WORK2+250                                         
* KEYLIST HAS KEY FIELDS FOR EXPANSIONS                                         
*                        ADV =4                                                 
*                        AGY =6                                                 
*                        OFF =2                                                 
*                        SAL =3                                                 
*                     TOTAL  =15 PER CONTRACT (UP TO 20 CONTRACTS)              
* DISPLAY CONTRACT                                                              
D50      EQU   *                                                                
         CLI   TOTREQ,C'Y'  ***FIX TO RELEASE LOCKED RECORDS IN ***             
         BNE   D50A         ***TOTALS SEQ LOOP                  ***             
         ZIC   RF,LOCKCNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,LOCKCNT                                                       
         CH    RF,=H'200'                                                       
         BL    D50A                                                             
         GOTO1 VDATAMGR,DMCB,=C'DMUNLK'                                         
         MVI   LOCKCNT,0                                                        
D50A     DS    0H                                                               
*                                                                               
         CLI   TOTREQ,C'Y'         TOTALS ONLY REQUEST?                         
         BE    D71                                                              
         MVC   LADV,RCONKADV                                                    
         MVC   0(4,R8),RCONKADV                                                 
         MVC   LTYP1,RCONTYPE                                                   
*                                  MOVE TYPE TO PRINT                           
*                                          JR0895                               
* AGENCY                                                                        
         MVC   LAGY(4),RCONKAGY                                                 
         MVC   4(6,R8),RCONKAGY                                                 
* CHECK AGY OFFICE                                                              
         CLC   RCONKAOF,SPACES                                                  
         BE    D55                                                              
         MVI   LAGY+4,C'-'                                                      
         MVC   LAGY+5(2),RCONKAOF                                               
         CLI   RCONKAGY+2,C' '                                                  
         BNE   D52                                                              
         MVC   LAGY+2(3),LAGY+4                                                 
         MVC   LAGY+5(2),SPACES                                                 
         B     D55                                                              
*                                                                               
D52      CLI   RCONKAGY+3,C' '                                                  
         BNE   D55                                                              
         MVC   LAGY+3(3),LAGY+4                                                 
         MVI   LAGY+6,C' '                                                      
*                                                                               
* OFFICE                                                                        
D55      MVC   LOFF,RCONKOFF                                                    
         MVC   10(2,R8),RCONKOFF                                                
* SALESMAN                                                                      
         MVC   LSAL,RCONSAL                                                     
         MVC   12(3,R8),RCONSAL                                                 
*                                                                               
* GET PRODUCT                                                                   
         SR    R4,R4                                                            
         LA    R3,RCONELEM                                                      
*                                                                               
D60      CLI   0(R3),5             PRODUCT ELEMENT?                             
         BE    D65                                                              
*                                                                               
* 13FEB90 *** START ***                                                         
         CLI   0(R3),1             IS THIS AN X'01' ELEM?                       
         BNE   D64                                                              
         CLC   =C'   ',22(R3)      RCONPRD=SPACES?                              
         BE    D64                                                              
         LA    R1,IOAREA2          SET DATAMGR TO USE IOAREA2                   
         ST    R1,AIOAREA                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'09'           BUILD PRODUCT CODE KEY                       
         MVC   KEY+18(4),RCONKADV                                               
         MVC   KEY+22(3),22(R3)                                                 
         MVC   KEY+25(2),RCONKREP                                               
         CLC   KEY(27),IOAREA2     SAME PRODUCT CODE AS PREV?                   
         BE    D63                 IF YES, SAVE I/O                             
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    D62                                                              
         MVC   LEXP1(13),=C'** MISSING **'                                      
         B     D63A                                                             
*                                                                               
D62      DS    0H                                                               
         BAS   RE,GETREC                                                        
D63      DS    0H                                                               
         LA    R1,IOAREA2+34       RPRDELEM                                     
*                                                                               
*                                  MOVE OF LEXP CHANGED TO LEXP1                
*                                                 JR 0895                       
         MVC   LEXP1,2(R1)         EXPANDED NAME                                
*                                                                               
D63A     DS    0H                                                               
         LA    R1,IOAREA           RESTORE DATAMGR TO USE IOAREA                
         ST    R1,AIOAREA                                                       
         MVC   KEY(27),IOAREA      RESTORE PREV READ                            
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                WHOA!THE INCREDIBLE DISAPPEARING REC         
         B     D70                                                              
D64      DS    0H                                                               
* 13FEB90 *** END ***                                                           
*                                                                               
         IC    R4,1(R3)            ELEM LEN                                     
         AR    R3,R4               NEXT ELEM                                    
         CLI   0(R3),0             LAST?                                        
         BE    D70                                                              
         B     D60                                                              
* MOVE PRODUCT EXPANSION                                                        
D65      MVC   LEXP1,2(R3)                                                      
*                                   MOVE LEXP CHANGED TO LEXP1                  
*                                             JR0895                            
* EDIT K NUMBER                                                                 
D70      ZAP   DUB,=P'0'                                                        
         MVO   DUB,RCONKCON                                                     
         EDIT  (P8,DUB),(8,LCON-1)                                              
         CLC   RCONBUYR(3),=C'ACC'                                              
         BNE   *+8                                                              
         MVI   LCON+7,C'*'                                                      
         SPACE 1                                                                
D71      EQU   *                                                                
* ADD ORDERED AND INVOICE AMOUNTS INTO PAGE ACCUMULATORS                        
         L     RE,ORDTOT                                                        
         A     RE,DOLLARS                                                       
         ST    RE,ORDTOT                                                        
         L     RE,DOLLARS                                                       
         OC    TWAORDGP,TWAORDGP   SET FOR PACKED?                              
         BNZ   D71A                                                             
         ZAP   TWAORDGP(8),=P'0'                                                
D71A     EQU   *                                                                
         CVD   RE,WORKX+4          CONVERT TO PACKED                            
         AP    TWAORDGP(8),WORKX+4(8)                                           
*                                                                               
         L     RE,INVTOT                                                        
         A     RE,DOLLARS+4                                                     
         ST    RE,INVTOT                                                        
         L     RE,DOLLARS+4                                                     
         OC    TWAINVGP,TWAINVGP   SET FOR PACKED?                              
         BNZ   D71B                                                             
         ZAP   TWAINVGP(8),=P'0'                                                
D71B     EQU   *                                                                
         CVD   RE,WORKX+4          CONVERT TO PACKED                            
         AP    TWAINVGP(8),WORKX+4(8)                                           
*                                                                               
         CLI   TOTREQ,C'Y'         TOTALS ONLY REQUEST?                         
         BE    D100                YES - SKIP DISPLAYING ANYTHING               
         SPACE 1                                                                
* EDIT ORD                                                                      
         L     R0,DOLLARS                                                       
         LPR   R0,R0                                                            
         C     R0,=F'100000000'        ARE $ 1MEG(WITH PENNIES) OR >?           
         BL    D72                       NO  - USE REGULAR EDIT                 
         EDIT  (4,DOLLARS),(11,LORD-1),2,MINUS=YES                              
         B     D73                 THIS EDIT DROPS COMMAS                       
D72      EQU   *                                                                
         EDIT  (4,DOLLARS),(11,LORD-1),2,MINUS=YES,COMMAS=YES                   
D73      EQU   *                                                                
*                                                                               
         FOUT  (R2)                                                             
         MVC   TWAKEY,KEY                                                       
         MVC   0(4,R9),KEY+28      SAVE DISK ADDR                               
         MVC   4(4,R9),DOLLARS+4   INVOICE AMOUNT                               
         OC    4(4,R9),4(R9)                                                    
         BNZ   D75                                                              
         CLI   BYTE4,0                                                          
         BE    D75                                                              
         MVC   4(4,R9),=C'ZERO'                                                 
D75      LA    R9,8(R9)                                                         
         LA    R8,15(R8)           KEY LIST                                     
*                                                                               
* GET TO INVOICE FIELD                                                          
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R2,R4               NEXT FIELD                                   
                                                                                
* EDIT INVOICE AMOUNT                                                           
         L     RE,DOLLARS+4        INVOICE AMOUNT                               
         CLI   BYTE4,0             INVOICE BUCKET FOR THIS MONTH?               
         BE    D80                                                              
         LPR   R0,RE                                                            
         C     R0,=F'100000000'    ARE $ 1MEG(WITH PENNIES) OR >?               
         BL    D76                 NO  - USE REGULAR EDIT                       
         EDIT  (RE),(11,8(R2)),2,MINUS=YES                                      
         B     D77                 THIS EDIT DROPS COMMAS                       
D76      EQU   *                                                                
         EDIT  (RE),(11,8(R2)),2,COMMAS=YES,MINUS=YES                           
D77      EQU   *                                                                
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         VALID BIT                                    
*                                                                               
D80      DS    0H                                                               
* GET TO DIFFERENCE FIELD                                                       
         ZIC   R4,0(R2)                                                         
         AR    R2,R4                                                            
         L     R4,DOLLARS                                                       
         S     R4,DOLLARS+4                                                     
         LPR   R0,R4                                                            
         C     R0,=F'10000000'     ARE $ 100,000.00 OR >?                       
         BL    D83                 NO  - USE REGULAR EDIT                       
         EDIT  (R4),(10,8(R2)),2,MINUS=YES                                      
         B     D85                 THIS EDIT DROPS COMMAS                       
D83      EQU   *                                                                
         EDIT  (R4),(10,8(R2)),2,COMMAS=YES,MINUS=YES                           
D85      EQU   *                                                                
         FOUT  (R2)                                                             
                                                                                
* CHECK IF REP PROFILE DOUBLE DISPLAY/INVOICE# SPECIFIED                        
         CLI   SVREPROF+7,C'Y'     DOUBLE DISPLAY LINE?                         
         BNE   D90                                                              
         ZIC   R4,0(R2)                                                         
         AR    R2,R4                                                            
         MVC   LORD+2(8),=C'INVOICE#'                                           
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R4,0(R2)                                                         
         AR    R2,R4                                                            
         GOTO1 GETINV#,DMCB,RCONREC,(R2)                                        
         OI    4(R2),X'20'         SET VALIDATED                                
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R4,0(R2)                                                         
         AR    R2,R4                                                            
                                                                                
D90      DS    0H                                                               
         ZIC   R4,0(R2)                                                         
         AR    R2,R4               NEXT FIELD                                   
         LA    RF,INVLN18H                                                      
         CR    R2,RF               LAST?                                        
         BH    D200                                                             
*                                                                               
* GET NEXT CONTRACT                                                             
D100     BAS   RE,SEQ                                                           
         CLC   KEY(11),TWAKEY      SAME STATION?                                
         BNE   D200                                                             
         OC    INVOFF(2),INVOFF    IS THERE OFFICE FILTER                       
         BZ    D105                                                             
         CLC   KEY(13),TWAKEY      SAME OFFICE?                                 
         BNE   D200                                                             
D105     OC    TWAAGYF,TWAAGYF       AGY FILTER?                                
         BZ    D110                  NO - PROCEED                               
         CLC   TWAAGYF(4),KEY+13     MATCH AGY?                                 
         BNE   D100                  NO - NEXT REC                              
         CLC   TWAAGYF+4(2),SPACES   HAVE AGY OFC FILTER?                       
         BE    D110                  NO - PROCEED                               
         CLC   KEY+17(2),TWAAGYF+4   MATCH AGY OFC?                             
         BNE   D100                  NO - NEXT REC                              
*                                                                               
*                                                                               
D110     BAS   RE,GETREC                                                        
         TM    IOAREA+29,X'80'     DELETE?                                      
         BO    D100                                                             
*                                                                               
*- CONTRACT TYPE FILTER (OPTIONAL)                                              
         CLI   INVTYPEH+5,0                                                     
         BE    D150                NO TYPE FILTER ENTERED                       
*                                                                               
         LA    RF,RCONTYPE-RCONREC                                              
         A     RF,AIOAREA                                                       
*                                                                               
         CLI   INVTYPE,C'*'        EXCLUDE?                                     
         BE    D120                                                             
*                                                                               
         CLC   0(1,RF),INVTYPE  INCLUDE ONLY MATCHING TYPE                      
         BNE   D100                                                             
         B     D150                                                             
*                                                                               
D120     CLC   0(1,RF),INVTYPE+1   EXCLUDE MATCHING TYPE                        
         BE    D100                                                             
                                                                                
* IF CONTRACT IS A FORECAST, SKIP AND GET NEXT                                  
D150     EQU   *                                                                
         BAS   RE,IS4CAST          IS THIS A FORECAST CONTRACT?                 
         BZ    D100                YES, SKIP IT                                 
                                                                                
*                                                                               
* CHECK FOR ORD OR INV DOLLARS IN THIS MONTH                                    
         GOTO1 BUCKETS,DMCB,RCONREC,DOLLARS                                     
         CLI   DMCB,X'FF'          ACCOUNTING                                   
         BE    D50                                                              
         OC    DOLLARS(8),DOLLARS                                               
         BZ    D100                                                             
         B     D50                 DISPLAY CONTRACT                             
         EJECT                                                                  
* DISPLAY CONTRACT EXPANSIONS                                                   
D200     CLC   INVACT+4(3),=C'PRO' PRODUCT?                                     
         BE    LAST                                                             
*                                                                               
         LA    R8,WORK2            KEY LIST                                     
         LA    R2,INVLN01H         FIRST OUTPUT LINE                            
         XC    IOAREA(27),IOAREA                                                
         CLC   INVACT+4(3),=C'ADV' ADVERTISER                                   
         BNE   D300                                                             
* PUT ADVERTISER EXPANSIONS                                                     
D202     MVI   RADVKTYP,8          ADV REC CODE                                 
         MVC   RADVKADV,0(R8)                                                   
         MVC   RADVKREP,REPALPHA                                                
         XC    WORK(20),WORK                                                    
         MVC   KEY(27),RADVKEY                                                  
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   D215                                                             
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    D212                                                             
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    D212                                                             
         MVC   KEY+25(2),=C'ZZ'                                                 
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   D215                                                             
* ADV FOUND                                                                     
D212     BAS   RE,GETREC                                                        
         MVC   WORK(20),RADVNAME                                                
D215     LA    R4,0                KEY ENTRY DISPL.                             
         LA    R5,3                LEN-1                                        
* PUT EXPANSION AND CHECK FOR SAME IN NEXT LINE                                 
         BAS   RE,PUTEXP                                                        
         B     D202                                                             
         EJECT                                                                  
* CHECK FOR AGENCY EXP                                                          
D300     CLC   INVACT+4(3),=C'AGE'                                              
         BNE   D400                                                             
* PUT AGENCY EXPANSIONS                                                         
* BUILD KEY                                                                     
D302     MVI   RAGYKTYP,10                                                      
         MVC   RAGYKAGY(6),4(R8)                                                
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   KEY(27),RAGYKEY                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   D315                                                             
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    D312                                                             
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    D312                                                             
         MVC   KEY+25(2),=C'ZZ'                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   D315                                                             
* AGENCY FOUND                                                                  
D312     BAS   RE,GETREC                                                        
         MVC   WORK(20),RAGYNAM1                                                
*                                                                               
D315     LA    R4,4                KEY ENTRY DISPL                              
         LA    R5,5                LEN-1                                        
* PUT AGENCY EXPANSION AND CHECK FOR SAME IN NEXT LINE                          
         BAS   RE,PUTEXP                                                        
         B     D302                                                             
         EJECT                                                                  
* CHECK FOR OFFICE EXP                                                          
D400     CLC   INVACT+4(3),=C'OFF'                                              
         BNE   D500                                                             
* PUT OFFICE EXPANSION                                                          
* BUILD OFFICE KEY                                                              
D402     MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,REPALPHA                                                
         MVC   ROFFKOFF(2),10(R8)                                               
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   KEY(27),ROFFKEY                                                  
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   D415                                                             
* OFFICE FOUND                                                                  
         BAS   RE,GETREC                                                        
         MVC   WORK(20),ROFFNAME                                                
*                                                                               
D415     LA    R4,10               KEY ENTRY DISPL.                             
         LA    R5,1                LEN-1                                        
* PUT OFFICE EXPANSION AND CHECK FOR SAME IN NEXT LINE                          
         BAS   RE,PUTEXP                                                        
         B     D402                                                             
         EJECT                                                                  
D500     CLC   INVACT+4(3),=C'SAL'                                              
         BNE   LAST                                                             
* SALESMAN EXPANSION                                                            
* BUILD KEY                                                                     
D502     MVI   RSALKTYP,6                                                       
         MVC   RSALKREP,REPALPHA                                                
         MVC   RSALKSAL,12(R8)                                                  
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   KEY,RSALKEY                                                      
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   D515                                                             
* SALESMAN FOUND                                                                
         BAS   RE,GETREC                                                        
         MVC   WORK(20),RSALNAME                                                
*                                                                               
D515     LA    R4,12               KEY ENTRY DISPL.                             
         LA    R5,2                LEN-1                                        
* PUT SALESMAN EXPANSION AND CHECK FOR SAME IN NEXT LINE                        
         BAS   RE,PUTEXP                                                        
         B     D502                                                             
         EJECT                                                                  
LAST     MVI   TWADISP,X'FF'       CONTRACTS DISPLAYED                          
*                                                                               
LAST10   MVC   INVMSG(L'INVMSG),SPACES                                          
         FOUT  INVMSGH                                                          
         MVC   INVMSG(33),=C'CONTRACTS DISPLAYED - MORE TO SEE'                 
         LA    R2,INVACTH                                                       
*                                                                               
         CLI   SVREPROF+7,C'Y'     DOUBLE DISPLAY LINE?                         
         BNE   LAST15                                                           
         CLI   TWAPAGE,250         IF MORE THAN 250 PAGES                       
         BH    LAST20              SKIP TOTALS                                  
                                                                                
LAST15   DS    0H                                                               
         CLI   TWAPAGE,125         IF MORE THAN 125 PAGES                       
         BH    LAST20              SKIP TOTALS                                  
*                                                                               
LAST18   DS    0H                                                               
         ZIC   RF,TWAPAGE          PUT PAGE TOTAL IN PROPER SLOT                
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         LA    R3,TWAORDT(RF)      ORDERED AMOUNT                               
         L     RE,ORDTOT                                                        
         ST    RE,0(R3)                                                         
*                                                                               
         LA    R3,TWAINVT(RF)      INVOICE AMOUNT                               
         L     RE,INVTOT                                                        
         ST    RE,0(R3)                                                         
*                                                                               
LAST20   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO PUT EXPANSIONS -ALSO BUMPS KEY PTR AND CHECKS FOR SAME             
*              R2=A(OUTPUT LINE HEADER)                                         
*              R8=A(KEY LIST PTR)                                               
*              R4=  KEY ENTRY DISPLACEMENT IN LIST                              
*              R5=  LEN-1 OF KEY                                                
*              RE=RETURN ADDR                                                   
*              WORK(20) = EXPANSION                                             
PUTEXP   MVC   LEXP1,WORK                                                       
*                                  MOVE OF LEXP CHANGED TO LEXP1                
*                                          JR0895                               
         LR    R1,R8                                                            
         LA    R8,15(R8)           NEXT KEY LIST                                
*                                                                               
         OC    0(4,R8),0(R8)                                                    
         BE    LAST                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)            NEXT OUTPUT LINE                             
         AR    R2,R0                                                            
         IC    R0,0(R2)            SKIP INVOICE FIELD                           
         AR    R2,R0                                                            
         IC    R0,0(R2)            SKIP DIFFERENCE FIELD                        
         AR    R2,R0                                                            
*                                                                               
         CLI   SVREPROF+7,C'Y'     DOUBLE DISPLAY LINE?                         
         BNE   PUTEXP10            NO                                           
         SR    R0,R0               YES - SKIP ANOTHER LINE                      
         IC    R0,0(R2)            NEXT OUTPUT LINE                             
         AR    R2,R0                                                            
         IC    R0,0(R2)            SKIP INVOICE FIELD                           
         AR    R2,R0                                                            
         IC    R0,0(R2)            SKIP DIFFERENCE FIELD                        
         AR    R2,R0                                                            
PUTEXP10 DS    0H                                                               
         LA    RF,INVLN18H                                                      
         CR    R2,RF               LAST?                                        
         BH    LAST                                                             
*                                                                               
         AR    R1,R4               OLD ENTRY                                    
         LR    RF,R8                                                            
         AR    RF,R4               NEW KEY ENTRY                                
         EX    R5,*+10             TEST IF NEXT KEY SAME                        
         BE    PUTEXP                                                           
         BR    RE                                                               
         CLC   0(0,R1),0(RF)                                                    
         EJECT                                                                  
***********************************************************************         
* IS CONTRACT REQUESTED A FORECAST CONTRACT?                                    
* RETURNS CC                                                                    
***********************************************************************         
IS4CAST  NTR1                                                                   
         LA    R6,IOAREA                                                        
         USING RCONREC,R6                                                       
         LA    R3,RCONELEM                                                      
         B     IS4CST20                                                         
         DROP  R6                                                               
                                                                                
IS4CST10 DS    0H                                                               
         ZIC   R0,1(R3)            NEXT ELEM                                    
         AR    R3,R0                                                            
                                                                                
IS4CST20 DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    NOT4CAST            NOT FOUND                                    
                                                                                
         CLI   0(R3),3             FORECAST CONTRACT HAS NO                     
         BE    NOT4CAST            BUCKETS                                      
         CLI   0(R3),4                                                          
         BE    NOT4CAST            INVOICE BUCKETS                              
         CLI   0(R3),X'54'                                                      
         BE    NOT4CAST            ALTERNATE INVOICE BUCKETS                    
         CLI   0(R3),X'63'                                                      
         BE    NOT4CAST            TRADE ESTIMATE BUCKETS                       
         CLI   0(R3),X'64'                                                      
         BE    NOT4CAST            TRADE INVOICE BUCKETS                        
         CLI   0(R3),X'83'                                                      
         BE    NOT4CAST            TRADE/ALTERNATE ESTIMATE BUCKETS             
         CLI   0(R3),X'84'                                                      
         BE    NOT4CAST            TRADE/ALTERNATE INVOICE BUCKETS              
         CLI   0(R3),6                                                          
         BE    NOT4CAST            EPL/SPL DATA                                 
                                                                                
         CLI   0(R3),X'12'                                                      
         BNE   IS4CST10                                                         
                                                                                
                                                                                
         USING RSARXEL,R3          FOUND. IS THERE A FORECAST FLAG?             
         CLI   RSARXLEN,RSARXLTH   NO ELEMENT IS OLD SAR ELEMENT                
         BL    NOT4CAST                                                         
         TM    RSARXFLG,X'18'                                                   
         BNZ   YES4CAST                                                         
         DROP  R3                                                               
                                                                                
NOT4CAST LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES4CAST SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO FIND DOLLARS FOR TWAMON                                            
*              P1=A(CONREC)                                                     
*              P2=A(8-BYTE OUTPUT - FIRST WORD = ORDERED                        
*                                   SECOND     = INVOICE                        
BUCKETS  NTR1                                                                   
         MVI   BYTE4,0                                                          
         LM    R2,R3,0(R1)                                                      
         SR    R4,R4                                                            
*********************************************************************           
***      LA    R7,RCONBUYR-RCONREC(R2)                                          
***      CLC   0(3,R7),=C'ACC'     ACCOUNTING OPTION?                           
***      BNE   BUCK0020                                                         
*********************************************************************           
* GET BROADCAST MONTHS                                                          
         MVC   DMWORK(6),RCONDATE-RCONREC(R2)                                   
*                                  PULL FLIGHT DATES                            
*********************************************************************           
***      LA    R7,RCONDATE-RCONREC(R2)                                          
***      GOTO1 VDATCON,DMCB,(3,(R7)),WORK                                       
***      GOTO1 (RF),(R1),(3,3(R7)),WORK+6                                       
*********************************************************************           
         GOTO1 VDATCON,DMCB,(3,DMWORK),WORK                                     
         GOTO1 (RF),(R1),(3,DMWORK+3),WORK+6                                    
         GOTO1 VGTBROAD,(R1),(1,WORK),WORK+12,VGETDAY,VADDAY                    
         GOTO1 (RF),(R1),(1,WORK+6),WORK+24                                     
         GOTO1 VDATCON,(R1),WORK+18,(3,DUB)                                     
         GOTO1 (RF),(R1),WORK+30,(3,DUB+3)                                      
         CLC   =C'DIS',INVACT      REGULAR INVOICE DISPLAY?                     
         BE    BUCK0010            YES - SHOW ACCOUNTING FLAG                   
         CLC   =C'DIT',INVACT      TRADE INVOICE DISPLAY?                       
         BE    BUCK0010            YES - SHOW ACCOUNTING FLAG                   
         B     BUCK0020            NO  - SKIP ACCOUNTING FLAG FOR               
*                                     ALT CAL DISPLAYS                          
BUCK0010 EQU   *                                                                
         CLC   TWAMON,DUB                                                       
         BL    BUCK0020                                                         
         CLC   TWAMON,DUB+3                                                     
         BH    BUCK0020                                                         
         MVI   0(R1),X'FF'         INDICATE ACCOUNTING CONTRACT                 
BUCK0020 LA    R2,RCONELEM-RCONREC(R2)                                          
         XC    0(8,R3),0(R3)       OUTPUT                                       
BUCK0040 LR    R5,R3                                                            
         CLC   =C'DIS',INVACT      REGULAR INVOICE DISPLAY?                     
         BNE   BUCK0060            NO                                           
         CLI   0(R2),3             YES - ORD BUCKET?                            
         BE    BUCK0120            YES - PROCESS                                
         CLI   0(R2),4             NO  - INV BUCKET?                            
         BE    BUCK0080            YES - PROCESS                                
         B     BUCK0100            NO  - BUMP TO NEXT BUCKET                    
BUCK0060 EQU   *                                                                
         CLC   =C'DIN',INVACT      ALT CAL INVOICE DISPLAY?                     
         BNE   BUCK0064            NO                                           
         CLI   0(R2),X'53'         YES - ALT ORD BUCKET?                        
         BE    BUCK0120            YES - PROCESS                                
         CLI   0(R2),X'54'         NO  - ALT INV BUCKET?                        
         BE    BUCK0080            YES - PROCESS                                
         B     BUCK0100            NO  - BUMP TO NEXT BUCKET                    
BUCK0064 EQU   *                                                                
         CLC   =C'DIT',INVACT      TRADE INVOICE DISPLAY?                       
         BNE   BUCK0068            NO                                           
         CLI   0(R2),X'63'         YES - ORD BUCKET?                            
         BE    BUCK0120            YES - PROCESS                                
         CLI   0(R2),X'64'         NO  - INV BUCKET?                            
         BE    BUCK0080            YES - PROCESS                                
         B     BUCK0100            NO  - BUMP TO NEXT BUCKET                    
BUCK0068 EQU   *                                                                
         CLC   =C'DTA',INVACT      TRADE/ALTERNATE INVOICE DISPLAY?             
         BNE   BUCK0072            NO                                           
         CLI   0(R2),X'83'         YES - ORD BUCKET?                            
         BE    BUCK0120            YES - PROCESS                                
         CLI   0(R2),X'84'         NO  - INV BUCKET?                            
         BE    BUCK0080            YES - PROCESS                                
         B     BUCK0100            NO  - BUMP TO NEXT BUCKET                    
BUCK0072 EQU   *                                                                
         DC    H'0'                HOW'D WE GET HERE?                           
BUCK0080 EQU   *                                                                
         LA    R5,4(R5)            BUMP TO NEXT ACCUMULATOR?                    
         B     BUCK0120                                                         
****>>>> B     BUCK0100                                                         
BUCK0100 IC    R4,1(R2)                                                         
         AR    R2,R4               NEXT ELEM                                    
         CLI   0(R2),0             LAST?                                        
         BNE   BUCK0040                                                         
* LAST                                                                          
         XIT1                                                                   
* CHECK BUCKET MONTH                                                            
BUCK0120 CLC   2(2,R2),TWAMON                                                   
         BNE   BUCK0100                                                         
* SAME MONTH                                                                    
         MVC   FULL,6(R2)          DOLLARS                                      
         L     R6,FULL                                                          
         A     R6,0(R5)                                                         
         ST    R6,0(R5)                                                         
         CLC   =C'DIS',INVACT      REGULAR INVOICE CHANGE?                      
         BNE   BUCK0140            NO                                           
         CLI   0(R2),4             REGULAR INVOICE?                             
         BE    BUCK0160                                                         
         B     BUCK0100                                                         
BUCK0140 EQU   *                                                                
         CLC   =C'DIN',INVACT      ALTERNATE INVOICE CHANGE?                    
         BNE   BUCK0144            NO                                           
         CLI   0(R2),X'54'         ALTERNATE INVOICE?                           
         BE    BUCK0160                                                         
         B     BUCK0100                                                         
BUCK0144 EQU   *                                                                
         CLC   =C'DIT',INVACT      TRADE INVOICE CHANGE?                        
         BNE   BUCK0148            NO                                           
         CLI   0(R2),X'64'         TRADE INVOICE?                               
         BE    BUCK0160                                                         
         B     BUCK0100                                                         
BUCK0148 EQU   *                                                                
         CLC   =C'DTA',INVACT      TRADE/ALTERNATE INVOICE CHANGE?              
         BNE   BUCK0152            NO                                           
         CLI   0(R2),X'54'         TRADE/ALTERNATE INVOICE?                     
         BE    BUCK0160                                                         
         B     BUCK0100                                                         
BUCK0152 EQU   *                                                                
         DC    H'0'                HOW'D WE GET HERE?                           
BUCK0160 EQU   *                                                                
         MVI   BYTE4,1             INDICATE INVOICE BUCKET EXISTS FOR           
*                                  THIS MONTH                                   
         B     BUCK0100                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET INVOICE NUMBER WITH APPROPRIATE MONTH                          
*              P1=A(CONREC)                                                     
*              P2=A(FIELD OUTPUT)                                               
***********************************************************************         
GETINV#  NTR1                                                                   
         L     R6,0(R1)                                                         
         L     R2,4(R1)                                                         
                                                                                
         MVI   ELCODE,X'18'        INVOICE# ELEMENT                             
         USING RCONDVEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   GETINVX                                                          
         CLI   RCONDVI#,0                                                       
         BE    GETINVX                                                          
         ZIC   R3,RCONDVI#                                                      
         LA    R6,RCONDVIL                                                      
                                                                                
GETINV10 DS    0H                                                               
         CLC   TWAMON,1(R6)        GET INVOICE# FOR MATCHING YEAR/MONTH         
         BE    GETINV20                                                         
         ZIC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         BCT   R3,GETINV10                                                      
         B     GETINVX                                                          
                                                                                
GETINV20 DS    0H                                                               
         CLI   0(R6),3                                                          
         BNH   GETINVX                                                          
         ZIC   RF,0(R6)                                                         
         SH    RF,=H'4'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),3(R6)                                                    
                                                                                
GETINVX  DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
*                                                                               
* LOCAL VARIABLES                                                               
*                                                                               
ELCODE   DS    X                                                                
LOCKCNT  DS    X                   LOCKED RECORD COUNT (IN TOTAL LOOP)          
         EJECT                                                                  
       ++INCLUDE REINVGEN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'143REINV20   05/01/02'                                      
         END                                                                    
