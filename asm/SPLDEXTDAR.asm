*          DATA SET SPLDEXTDAR AT LEVEL 211 AS OF 05/01/02                      
*PHASE SPEXTHSD                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CLUNPK                                                                 
         TITLE 'DMLDEXTHSD - GET TOTAL NUMBER OF BUYS OFF DARE ORDERS'          
*                            FOR A PARTICULAR MONTH                             
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
*                                                                               
         L     R2,VLDDEFN                                                       
         USING LDDEFND,R2                                                       
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  LA    RE,*+6              KEEP RECORD EXIT                             
         BSM   0,RE                AND GET OUT OF 31 BIT MODE                   
         L     R1,APARM                                                         
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
EXIT     XIT1                                                                   
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         MVC   VDATAMGR,LDATAMGR   SET DMGR ADDRESS IN COMFACS                  
         MVC   ADATCON,LDATCON     A(DATCON)                                    
         MVC   ADATVAL,LDATVAL     A(DATVAL)                                    
*                                                                               
         GOTO1 LLOADER,DMCB,=CL8'T00A7A'   STAPACK                              
         ICM   R0,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R0,VSTAPACK                                                      
*                                                                               
         L     RE,=V(BINSRCH)                                                   
         ST    RE,VBINSRCH                                                      
         L     RE,=V(CLUNPK)                                                    
         ST    RE,VCLUNPK                                                       
*                                                                               
         GOTO1 ADATVAL,DMCB,STRTDATE,STARTDAY                                   
         OC    DMCB,DMCB           VALID DATE?                                  
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 ADATVAL,DMCB,ENDDATE,ENDDAY                                      
         OC    DMCB,DMCB           VALID DATE?                                  
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 ADATCON,DMCB,(0,STARTDAY),(19,STARTJUL)                          
         GOTO1 ADATCON,DMCB,(0,ENDDAY),(19,ENDJUL)                              
*                                                                               
         MVC   TITLE+10(18),=C'DARE ACTIVITY FOR '                              
         MVC   TITLE+10+18(L'STRTDATE),STRTDATE                                 
         MVI   TITLE+10+18+L'STRTDATE,C'-'                                      
         MVC   TITLE+10+18+L'STRTDATE+1(L'ENDDATE),ENDDATE                      
*                                                                               
         XC    COUNT,COUNT                                                      
         XC    BINCOUNT,BINCOUNT                                                
*                                                                               
* GET MAIN HERE                                                                 
*                                                                               
         L     R0,GETSIZE          R0 = SIZE OF STORAGE TO GET (2 MIL)          
         XC    ATABLE,ATABLE                                                    
*                                                                               
* CHANGE GETSIZE IF YOU CHANGE AMOUNT OF STORAGE NEEDED                         
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF               ANY ERRORS?                                  
         BZ    *+6                 NO - CAN ALLOCATE SPACE                      
         DC    H'00'               ERROR                                        
*                                                                               
         ST    R1,ATABLE           ADDR OF TABLE IN CORETAB                     
*                                                                               
DMXINITX B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         TM    MYFLAG,DAREDONE     FINISHED ALL DARE RECORDS?                   
         BO    DMXR60              YES                                          
*                                                                               
         L     R3,AREC             PROCESS DARE ORDER RECORDS                   
         USING DAREORDD,R3         X'0D34' RECORDS                              
*                                                                               
         CLC   0(2,R3),=X'0D34'    STILL DARE ORDER RECORD?                     
         BL    DMXKEEP             NO - TRY NEXT ONE                            
         BH    DMXR50              NO MORE DARE ORDER RECORDS                   
*                                                                               
         CLI   DOKCMT,0            COMMENT RECORD?                              
         BNE   DMXKEEP             YES - TRY NEXT ONE                           
         DROP  R3                                                               
*                                                                               
DMXR5    DS    0H                                                               
         L     R3,AREC                                                          
         MVI   ELCODE,X'11'        GET DARE TRANSMISSION ELEMENT                
         LA    R5,24               DISPLACEMENT TO FIRST ELEMENT                
         STH   R5,DATADISP                                                      
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
*                                                                               
         USING DOXMTELD,R3         JAN1/97 < STATUS DATE < JAN31/97?            
*                                                                               
         CLC   DOXMTSTD,STARTJUL                                                
         BL    DMXKEEP             NO                                           
         CLC   DOXMTSTD,ENDJUL                                                  
         BH    DMXKEEP             NO                                           
*                                                                               
         CLI   DOXMTSTA,QCFMD      CONFIRMED?                                   
         BE    DMXR10              YES - PROCESS THIS RECORD                    
         CLI   DOXMTSTA,QNODARE    NOT DARE ANYMORE?                            
         BE    DMXR10              YES - PROCESS THIS RECORD                    
         CLI   DOXMTSTA,QUNDARE    UNDARED?                                     
         BNE   DMXKEEP                                                          
         DROP  R3                                                               
*                                                                               
DMXR10   DS    0H                  YES - FOUND AN X'0D34' RECORD                
         L     R3,AREC                                                          
         USING DAREORDD,R3                                                      
*                                                                               
         XC    REC,REC                                                          
         LA    R4,REC                                                           
         USING RECOUTD,R4                                                       
         MVC   ROAGYMED,DOKAGMD    AGENCY/MEDIA                                 
         DROP  R3                                                               
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL            GO GET X'01' ELEMENT                         
         BNE   DMXKEEP             NO X'01' ELEM - GET NEXT RECORD              
*                                                                               
         USING DOIDELD,R3          DARE ID ELEMENT                              
         MVC   ROCLT,DOIDCLT       CLIENT                                       
         MVC   ROSTA,DOISTA        STATION                                      
         MVC   ROEST,DOIDEST       ESTIMATE                                     
         MVC   ROPRD,DOIDPRD       PRODUCT                                      
         MVC   ROPRD2,DOIDPRD2     PRODUCT 2                                    
         DROP  R3                                                               
*                                                                               
         L     R3,AREC                                                          
         MVI   ELCODE,X'22'        DARE BUYLINE ELEMENT                         
         BAS   RE,GETEL            GO GET X'22' ELEMENT                         
         BNE   DMXKEEP             NO X'22' ELEM - GET NEXT RECORD              
*                                                                               
         USING DOBUYELD,R3                                                      
*                                                                               
DMXR20   DS    0H                                                               
         LA    R5,ROLINES          LINE NUMS FOR WORKER FILE                    
         XC    ONBIT,ONBIT         BIT TO TURN ON                               
*                                                                               
         SR    R6,R6                                                            
         ZIC   R7,DOBUYSPT         SPOTPAK BUYLINE                              
         D     R6,=F'8'            R6 = REMAINDER, R7 = QUOTIENT                
*                                                                               
         CH    R6,=H'0'            ANY REMAINDER?                               
         BNE   *+12                YES - R7 = DISPLACEMENT INTO WFLINES         
         SH    R7,=H'1'            NO - MUST DECREMENT R7                       
         BM    *+8                 QUOTIENT = 0, DON'T BUMP                     
         LA    R5,0(R7,R5)         BUMP TO QUOTIENTH POS. IN REC                
*                                                                               
         LA    R7,8                                                             
         SR    R7,R6               R7 = 8 - REMAINDER = BIT TO TURN ON          
*                                                                               
         LA    RF,1                                                             
         CH    R7,=H'8'            ANY REMAINDER?                               
         BE    DMXR27              NO                                           
DMXR25   SLL   RF,1                SHIFT 1 BY # OF BITS IN R7                   
         BCT   R7,DMXR25                                                        
*                                                                               
DMXR27   STC   RF,ONBIT            BIT TO TURN ON IN WFLINES                    
         OC    0(1,R5),ONBIT       TURN ON THIS BIT                             
*                                                                               
         BAS   RE,NEXTEL           GET NEXT X'22' ELEMENT                       
         BE    DMXR20                                                           
         DROP  R3                                                               
*                                                                               
DMXR30   DS    0H                                                               
         L     R3,AREC                                                          
         MVC   RECKEY,0(R3)                                                     
*&&DO                                                                           
         GOTO1 LHEXOUT,DMCB,REC,P,9,=C'TOG'          PPPPPPPPPPPP               
         GOTO1 LHEXOUT,DMCB,RECKEY,P+22,13,=C'TOG'   PPPPPPPPPPPP               
         EDIT  (4,BINCOUNT),(8,P+50),ZERO=NOBLANK    PPPPPPPPPPPP               
         GOTO1 VPRINTER                                                         
*&&                                                                             
         L     R5,BINCOUNT                                                      
         LA    R5,1(R5)                                                         
         ST    R5,BINCOUNT                                                      
*                                                                               
* ADD REC TO TABLE                                                              
*                                                                               
         LA    RE,*+10             GET INTO 31 BIT MODE                         
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         L     R5,ATABLE                                                        
         MVI   BINPARAM+12,X'01'   ADD REC TO TABLE                             
         GOTO1 VBINSRCH,BINPARAM,REC,(R5)                                       
         OC    DMCB(4),DMCB        TABLE FULL?                                  
         BNZ   DMXKEEP             NO - GET NEXT DARE RECORD                    
         DC    H'00'                                                            
*                                                                               
DMXR50   DS    0H                  FINISHED WITH ALL '0D34' RECORDS             
*&&DO                                                                           
* PRINT TOTAL # OF DARE RECORDS                                                 
         MVC   P(26),=C'TOTAL # OF DARE RECORDS : '  PPPPPPPPPPPP               
         EDIT  (4,BINCOUNT),(8,P+28),ZERO=NOBLANK    PPPPPPPPPPPP               
         GOTO1 VPRINTER                              PPPPPPPPPPPP               
         GOTO1 VPRINTER                              PPPPPPPPPPPP               
*&&                                                                             
         OI    MYFLAG,DAREDONE     FINISHED ALL DARE RECS - GET BUYS            
         XC    REC,REC                                                          
         XC    SAVEKEY,SAVEKEY                                                  
         XC    TOTAL,TOTAL                                                      
         XC    SUBTOTAL,SUBTOTAL                                                
         XC    SVAGYCLT,SVAGYCLT                                                
         XC    SUBAGY,SUBAGY                                                    
         XC    SUBAGCLT,SUBAGCLT                                                
*                                                                               
         LA    R5,24                                                            
         STH   R5,DATADISP         DISPLACEMENT TO FIRST ELEMENT                
         GOTO1 VPRINTER                                                         
*                                                                               
DMXR60   DS    0H                  PROCESS BUY RECORDS                          
         XC    COUNT,COUNT                                                      
         MVI   AREC,0                                                           
         L     R3,AREC                                                          
         USING BUYRECD,R3                                                       
*                                                                               
         CLI   BUYKAM,X'11'        RANGE OF BUY RECORDS                         
         BL    DMXKEEP                                                          
         CLI   BUYKAM,X'FE'                                                     
         BH    DMXRX               FINISHED BUY RECORDS - EXIT                  
*                                                                               
         CLI   BUYKPRD,X'FF'       POOL PRODUCTS                                
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   SAVEKEY,BUYKEY      SAME 10 BYTES AS LAST REC?                   
         BE    DMXR65              YES - SAME RULES AS LAST BUY RECORD          
*                                                                               
* BUILD KEY TO FIND IN BINSRCH TABLE                                            
*                                                                               
DMXR62   DS    0H                                                               
         LA    R4,REC                                                           
         USING RECOUTD,R4                                                       
*                                                                               
         MVC   ROAGYMED,BUYKAM     AGENCY/MEDIA                                 
         MVC   ROCLT,BUYKCLT       CLIENT                                       
         MVC   ROSTA,BUYMSTA+2     STATION                                      
         MVC   ROEST,BUYKEST       ESTIMATE NUMBER                              
*                                                                               
DMXR65   LA    R4,REC                                                           
         L     R3,AREC                                                          
         LA    R3,24(R3)           A(1ST ELEMENT)                               
*                                                                               
DMXR70   DS    0H                                                               
         CLI   0(R3),X'0B'         DID WE GET '0B' ELEMENTS YET?                
         BE    DMXR75              YES                                          
         CLI   0(R3),X'0C'         DID WE GET '0C' ELEMENTS YET?                
         BE    DMXR75              YES                                          
         CLI   0(R3),0             NO MORE ELEMENTS - DONE                      
         BE    DMXR100                                                          
*                                                                               
DMXR72   DS    0H                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                GET OUT OF 31 BIT MODE                       
*                                                                               
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     DMXR70                                                           
*                                                                               
DMXR75   DS    0H                  FOUND AN ELEMENT - CHECK THE                 
         USING REGELEM,R3          PRODUCT NUMBERS FOR THE BUYS                 
         XC    TMPPRODS,TMPPRODS                                                
*                                                                               
         CLI   RLEN,X'0E'          ONLY 1 PRODUCT HERE?                         
         BNE   DMXR80                                                           
         MVC   TMPPRODS(1),RPPRD   YES - MOVE IN 1 PRODUCT CODE                 
         B     DMXR85                                                           
*                                                                               
DMXR80   DS    0H                  NO - ELEMENT HAS 2 PRODUCTS                  
         MVC   TMPPRODS(1),RPPRD       MOVE IN 1ST PRODUCT CODE                 
         MVC   TMPPRODS+1(1),RPPRD+4   MOVE IN 2ND PRODUCT CODE                 
*                                                                               
* FIND REC IN TABLE                                                             
*                                                                               
DMXR85   DS    0H                                                               
         LA    RE,*+10             GET INTO 31 BIT MODE                         
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         MVC   ROPRD(2),TMPPRODS   MOVE IN PRODUCT CODES                        
*                                                                               
         LA    R4,REC                                                           
         ST    R4,BINPARAM         A(REC)                                       
         L     R5,ATABLE                                                        
         ST    R5,BINPARAM+4       A(TABLE)                                     
         L     RE,BINCOUNT         # OF RECS IN TABLE                           
         ST    RE,BINPARAM+8                                                    
         MVI   BINPARAM+12,X'00'   FIND MATCH ON KEY                            
*                                                                               
         GOTO1 VBINSRCH,BINPARAM                                                
         CLI   0(R1),X'80'         RECORD FOUND?                                
         BE    DMXR72              !!!! NO -  GO TRY NEXT ELEMENT               
*                                                                               
         L     R4,0(R1)            A(RECORD IN TABLE)                           
         MVC   TBLREC,0(R4)        SAVE AWAY REC                                
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                GET OUT OF 31 BIT MODE                       
         DROP  R3                                                               
*                                                                               
DMXR95   DS    0H                  BOTH PRODUCTS MATCH!                         
         L     R5,COUNT            # OF BUYS FOR THIS RECORD                    
         LA    R5,1(R5)                                                         
         ST    R5,COUNT                                                         
         B     DMXR72              !!!! GO GET NEXT ELEMENT                     
*                                                                               
DMXR100  DS    0H                  CHECK ALL ELEMENTS IN THIS RECORD            
         OC    COUNT,COUNT         FOUND ANY BUY MATCHES?                       
         BZ    DMXKEEP             NO - GO GET NEXT BUY RECORD                  
*                                                                               
         L     R6,TOTAL            TOTAL # OF BUYS                              
         A     R6,COUNT            + # OF BUYS FOR THIS RECORD                  
         ST    R6,TOTAL                                                         
* !!!!!                                                                         
         L     R3,AREC                                                          
         USING BUYRECD,R3                                                       
*&&DO                                                                           
         EDIT  (4,COUNT),(8,P+30),ZERO=NOBLANK    PPPPPPPPPPPP                  
         GOTO1 LHEXOUT,DMCB,BUYKEY,P,13,=C'TOG'   PPPPPPPPPPPP                  
         GOTO1 VPRINTER            PPPPPPPPPPPPPPPP                             
*&&                                                                             
         LA    R4,TBLREC                                                        
         LA    R5,P                                                             
         USING PLINED,R5                                                        
*                                                                               
         OC    SAVEKEY,SAVEKEY     1ST TIME THROUGH?                            
         BNZ   *+10                                                             
         MVC   SAVEKEY,BUYKEY                                                   
*                                                                               
         CLC   SAVEKEY(9),BUYKEY      SAME AS PREV BUY REC?                     
         BE    DMXR120                (NOT COUNTING ESTIMATE #)                 
*                                                                               
DMXR110  DS    0H                                                               
         LA    R3,SAVEKEY                                                       
         CLC   BUYKEY(3),SVAGYCLT  SAME AGENCY OR CLIENT AS BEFORE              
         BE    DMXR115                                                          
         ZAP   LINE,=P'99'                                                      
         MVC   SVAGYCLT,BUYKEY     SAVE OFF AGY/CLT                             
         BAS   RE,PRNTHEAD         PRINT HEADER TO NEW LINE                     
*                                                                               
DMXR115  DS    0H                                                               
         BAS   RE,PRNTREC                                                       
         L     R3,AREC                                                          
*                                                                               
         CLC   BUYKEY(1),SAVEKEY   SAME AGY AS BEFORE?                          
         BE    *+12                                                             
         BAS   RE,PRNTASUB         NO - CALC SUBTOTAL BY AGY                    
         B     DMXR117                                                          
*                                                                               
         CLC   BUYKEY+1(2),SAVEKEY+1    SAME CLT AS BEFORE?                     
         BE    *+8                                                              
         BAS   RE,PRNTCSUB         PRINT SUBTOTAL BY AGY/CLT                    
*                                                                               
DMXR117  XC    SUBTOTAL,SUBTOTAL                                                
*                                                                               
DMXR120  DS    0H                                                               
         L     RF,SUBTOTAL         COUNTER FOR SUBTOTAL BY AGY/CLT/STA          
         A     RF,COUNT                                                         
         ST    RF,SUBTOTAL                                                      
*                                                                               
         L     RF,SUBAGY           COUNTER FOR SUBTOTAL BY AGY                  
         A     RF,COUNT                                                         
         ST    RF,SUBAGY                                                        
*                                                                               
         L     RF,SUBAGCLT         COUNTER FOR SUBTOTAL BY AGY/CLT              
         A     RF,COUNT                                                         
         ST    RF,SUBAGCLT                                                      
*                                                                               
         L     R3,AREC                                                          
         MVC   SAVEKEY,BUYKEY      SAVE BUY RECORD KEY                          
         MVC   SVAGY,BUYALPHA      SAVE ALPHA AGENCY                            
         GOTO1 VCLUNPK,DMCB,BUYKCLT,SVCLT     SAVE ALPHA CLIENT                 
         XC    COUNT,COUNT                                                      
         B     DMXKEEP                                                          
         DROP  R5                                                               
*                                                                               
DMXRX    DS    0H                  DONE - EXIT                                  
         L     R0,GETSIZE                                                       
         L     R1,ATABLE                                                        
         FREEMAIN RC,A=(1),LV=(0)                                               
*                                                                               
DMXEOF   DS    0H                                                               
         OC    TOTAL,TOTAL         ANY SPOTS AT ALL?                            
         BZ    DMXEOFX             NO - EXIT                                    
*                                                                               
         CLC   SAVEKEY(3),SVAGYCLT  SAME AGENCY OR CLIENT AS BEFORE?            
         BE    *+14                                                             
         ZAP   LINE,=P'99'         NO - PRINT ON NEW PAGE                       
         BAS   RE,PRNTHEAD         PRINT HEADER TO NEW LINE                     
*                                                                               
         BAS   RE,PRNTREC          PRINT LAST RECORD                            
         BAS   RE,PRNTASUB         PRINT SUBTOTAL BY AGY                        
*                                                                               
         MVC   P+2(18),=C'TOTAL # OF SPOTS : '                                  
         EDIT  (4,TOTAL),(8,P+21),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
DMXEOFX  B     DMXIT                                                            
*-------------------------------------------------------------------            
*        PRINT SUBTOTAL BY AGENCY                                               
*-------------------------------------------------------------------            
PRNTASUB NTR1                                                                   
         LA    R5,P                                                             
         USING PLINED,R5                                                        
*                                                                               
         CLC   BUYKEY+1(2),SAVEKEY+1    SAME CLT AS BEFORE?                     
         BE    *+8                                                              
         BAS   RE,PRNTCSUB         PRINT SUBTOTAL BY AGY/CLT                    
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+2(2),SVAGY        ALPHA AGENCY                                 
         MVC   P+7(8),=C'TOTALS :'                                              
         EDIT  (4,SUBAGY),(8,P+21),ZERO=NOBLANK                                 
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XC    SUBAGY,SUBAGY                                                    
*                                                                               
PRNTASBX XIT1                                                                   
*-------------------------------------------------------------------            
*        PRINT SUBTOTAL BY AGENCY/CLIENT                                        
*-------------------------------------------------------------------            
PRNTCSUB NTR1                                                                   
         LA    R5,P                                                             
         USING PLINED,R5                                                        
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+2(2),SVAGY        ALPHA AGENCY                                 
         MVC   P+7(3),SVCLT        ALPHA CLIENT                                 
         MVC   P+13(8),=C'TOTALS :'                                             
         EDIT  (4,SUBAGCLT),(8,P+21),ZERO=NOBLANK                               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XC    SUBAGCLT,SUBAGCLT                                                
*                                                                               
PRNTCSBX XIT1                                                                   
*-------------------------------------------------------------------            
*        PRINT HEADERS ON NEW PAGE                                              
*-------------------------------------------------------------------            
PRNTHEAD NTR1                                                                   
         LA    R5,P                                                             
         USING PLINED,R5                                                        
*&&DO                                                                           
         MVC   P(18),=C'DARE ACTIVITY FOR '                                     
         MVC   P+18(L'STRTDATE),STRTDATE                                        
         MVI   P+18+L'STRTDATE,C'-'                                             
         MVC   P+18+L'STRTDATE+1(L'ENDDATE),ENDDATE                             
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*&&                                                                             
         MVC   PAGY(3),=C'AGY'                                                  
         MVC   PCLT(3),=C'CLT'                                                  
         MVC   PSTA(3),=C'STA'                                                  
         MVC   PSPOTS+2(7),=C'# SPOTS'                                          
         GOTO1 VPRINTER                                                         
         MVC   PAGY(3),=C'---'                                                  
         MVC   PCLT(3),=C'---'                                                  
         MVC   PSTA(3),=C'---'                                                  
         MVC   PSPOTS+2(7),=C'-------'                                          
         GOTO1 VPRINTER                                                         
PRNTHX   XIT1                                                                   
*-------------------------------------------------------------------            
*        PRINT OUT REPORT                                                       
*-------------------------------------------------------------------            
PRNTREC  NTR1                                                                   
         LA    R3,SAVEKEY                                                       
         USING BUYRECD,R3                                                       
         LA    R5,P                                                             
         USING PLINED,R5                                                        
*                                                                               
         MVC   PAGY,SVAGY                     ALPHA AGENCY                      
         GOTO1 VCLUNPK,DMCB,BUYKCLT,PCLT      CLIENT                            
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'        UNPACK                                       
         MVC   STAPAGY,BUYALPHA    ALPHA AGENCY                                 
         MVI   STAPMED,C'T'        TELEVISION                                   
         MVC   STAPACOM,=A(COMFACS)                                             
         MVC   STAPMKST,BUYMSTA    MKT/STA                                      
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PSTA,STAPQSTA       STATION                                      
*                                                                               
         EDIT  (4,SUBTOTAL),(8,PSPOTS),ZERO=NOBLANK    # OF SPOTS               
         GOTO1 VPRINTER                                                         
*                                                                               
PRNTRECX XIT1                                                                   
*-------------------------------------------------------------------            
         EJECT                                                                  
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
*                                                                               
         DS    0D                                                               
         DS    0A                                                               
STAPARMS DS    XL32                                                             
*                                                                               
VCLUNPK  DS    A                                                                
VSTAPACK DS    A                                                                
VBINSRCH DS    A                                                                
*                                                                               
* COMFACS BELOW IS FOR CALL TO STAPACK                                          
*                                                                               
COMFACS  DS    0F                                                               
VDATAMGR DS    A                                                                
         DS    0D                                                               
*                                                                               
         DC    CL8'COUNTERS'                                                    
BYTE     DS    X                                                                
COUNT    DS    F                   # OF BUYS FOR THAT RECORD                    
SUBTOTAL DS    F                   # OF BUYS FOR THAT AGY/CLT/STA               
TOTAL    DS    F                   TOTAL # OF BUYS                              
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    XL1                                                              
*                                                                               
STAWORK  DS    XL31                STAPACK INPUT/OUTPUT BLOCK                   
*                                                                               
         DC    CL8'BINPARAM'                                                    
         DS    0D                                                               
BINPARAM DC    A(0)                A(RECORD TO BE FOUND OR ADDED)               
         DC    A(0)                A(TABLE)                                     
BINPCNT  DC    F'0'                NUMBER OF RECS IN TABLE                      
         DS    XL1                                                              
         DS    XL1                                                              
         DC    H'42'               LENGTH OF REC IN TABLE                       
         DC    AL1(0),AL3(9)       DISPLACEMENT OF KEY                          
BINPMAX  DC    A(2000000/L'REC)                                                 
*                                                                               
BINCOUNT DS    F                                                                
TBLMATCH DS    F                   REC MATCH IN TABLE                           
TBLREC   DS    XL9                 REC MATCH IN TABLE                           
*                                                                               
         DC    CL7'MYSTUFF'                                                     
ONBIT    DS    XL1                 BIT TO TURN ON                               
ELEMLEN  DS    XL1                 LENGTH TO COMPARE IN '0B','0C' ELEMS         
*                                                                               
STRTDATE DC    CL8'DEC01/97'                                                    
ENDDATE  DC    CL8'DEC31/97'                                                    
STARTDAY DS    CL6                                                              
ENDDAY   DS    CL6                                                              
STARTJUL DS    XL3                                                              
ENDJUL   DS    XL3                                                              
*                                                                               
*                                                                               
STARTCPR DS    XL2                 COMPRESSED START OF BROADCAST MONTH          
ENDCPR   DS    XL2                 COMPRESSED END OF BROADCAST MONTH            
*                                                                               
SVAGYCLT DS    CL3                 SAVE PREVIOUS AGENCY/CLIENT                  
SVAGY    DS    CL2                 ALPHA AGENCY                                 
SVCLT    DS    CL3                 ALPHA CLIENT                                 
SUBAGY   DS    F                   SUBTOTAL BY AGENCY                           
SUBAGCLT DS    F                   SUBTOTAL BY AGENCY/CLIENT                    
TMPPRODS DS    XL2                 SAVE PRODUCTS TO MATCH ON                    
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
ONEPRD   EQU   X'01'               ONLY ONE PRODUCT NUMBER                      
DAREDONE EQU   X'02'               FINISHED PROCCESSING DARE RECS               
*                                                                               
         DC    CL3'REC'                                                         
REC      DS    XL42                USED TO BUILD WORKER FILE                    
RECKEY   DS    XL13                                                             
SAVEKEY  DS    XL10                CHECK IF SAME RULES APPLY AS LAST -          
*                                  - BUY RECORD                                 
         DC    CL7'GETMAIN'                                                     
         DS    0F                                                               
GETSIZE  DC    XL4'001E8480'       GET 2,000,000 BYTES                          
ATABLE   DS    F                                                                
*                                                                               
         DC    CL9'ADDRESSES'                                                   
ADATCON  DS    A                                                                
ADATVAL  DS    A                                                                
         DS    0F                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*SPGENDRORD                                                                     
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
*DDCOREQUS                                                                      
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*SPGENBUY                                                                       
BUYRECD   DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
RECOUTD  DSECT                     USED TO BUILD WORKER FILE                    
ROAGYMED DS    XL1                 AGENCY/MEDIA                                 
ROCLT    DS    XL2                 CLIENT                                       
ROSTA    DS    XL3                 STATION                                      
ROEST    DS    XL1                 ESTIMATE                                     
ROPRD    DS    XL1                 PRODUCT                                      
ROPRD2   DS    XL1                 PRODUCT 2                                    
ROLINES  DS    XL(L'REC-9)         REMAINING SPACE FOR LINE NUMS                
*                                                                               
PLINED   DSECT PRINT LINE FOR OUTPUT                                            
PLINE    DS   0CL27                                                             
         DS    CL2                                                              
PAGY     DS    CL2                 AGENCY                                       
         DS    CL3                                                              
PCLT     DS    CL3                 CLIENT                                       
         DS    CL3                                                              
PSTA     DS    CL5                 STATION                                      
         DS    CL3                                                              
PSPOTS   DS    CL3                 # OF SPOTS                                   
         DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'211SPLDEXTDAR05/01/02'                                      
         END                                                                    
