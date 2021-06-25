*          DATA SET SPCSO08    AT LEVEL 008 AS OF 11/07/03                      
*PHASE T21808A,*                                                                
         TITLE 'T21808 - CHILD SPOT BUY TRANSFER'                               
T21808   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYIOL,T21808,R8,RR=RE                                            
         ST    RE,RELO                                                          
         LR    R2,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,MYIO                                                          
         USING SESTLSTD,R7         R7 RESERVED FOR SUBEST TABLE                 
         B     AROUND                                                           
RELO     DS    A                                                                
*                                                                               
AROUND   CLI   MYOVNUM,X'08'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    BUYMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
*                                                                               
GOAHEAD  MVI   MYOVNUM,X'08'       STORE OVERLAY NUMBER                         
*                                                                               
         MVI   SKIPWRT,C'N'        SET FLAG OF WHETHER TO SKIP WRITES           
         CLI   OFFLINE,C'Y'                                                     
         BNE   GOMODE                                                           
         CLI   TWAWRITE,C'N'                                                    
         BNE   GOMODE                                                           
         MVI   SKIPWRT,C'Y'                                                     
*                                                                               
GOMODE   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT (DONE FOR OVERNITE)             
         BE    PR                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
* CLOSE DCB ON RUNLAST !                                                        
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T218FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         ST    RE,AREQDSN                SET CORE RES ADDRESS                   
*                                                                               
         L     R2,AREQDSN                                                       
         CLOSE ((2))              CLOSE VIRTUAL REQUEST FILE                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AREQDSN                                                       
         FREEPOOL ((2))                                                         
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,BUYMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    BUYCLTH+4,X'DF'                                                  
         NI    BUYSTAH+4,X'DF'                                                  
         NI    BUYESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,BUYCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    BUYSTAH+4,X'DF'                                                  
         NI    BUYESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,BUYSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    BUYESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         CLI   CONREC,C'B'         IF OVERNITE TRANSFER                         
         BE    VKSTA2                                                           
         CLC   8(3,R2),=C'ALL'     THEN 'ALL' FOR ALL MARKETS                   
         BE    VKALL                                                            
         TM    4(R2),X'08'         NUMERIC MEANS MARKET NUMBER                  
         BO    VKMKT                                                            
VKSTA2   GOTO1 VALISTA             OTHERWISE VALIDATE STATION                   
         B     VKSTAX                                                           
*                                                                               
VKMKT    GOTO1 VALIMKT             VALIDATE MARKET NUMBER                       
         B     VKSTAX                                                           
*                                                                               
VKALL    XC    BMKTSTA,BMKTSTA     ALL MARKETS/STATIONS                         
*                                                                               
VKSTAX   MVC   SVMKTSTA,BMKTSTA    SAVE REQUESTED MKT/STA                       
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,BUYESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
VKX      BRAS  RE,TSTLOCK          TEST DATA LOCKED                             
         BNE   ERRLOCK                                                          
         MVI   PTAPRD,0            IF CLIENT 'PTA'                              
*        CLC   =C'PTA',BUYCLT                                                   
*        BNE   VKX10                                                            
*        GOTO1 =A(GETPTAPR),RR=RELO    GET PRODUCT FOR THIS BRD POL EST         
*                                                                               
VKX10    CLI   CONREC,C'B'         IF BUY TRANSFER                              
         BNE   VKX50                                                            
         XC    TRNSTART,TRNSTART   INITIALIZE START DATE                        
         BAS   RE,INITSEST         INITIALIZE SUBEST LIST                       
*                                                                               
         BAS   RE,TESTBUY          IF BUYS ALREADY EXIST                        
         BNE   VKX20                                                            
         BAS   RE,FRCRETR          THEN FORCE RETRANSFER 'YES' OF 'OK'          
         BAS   RE,VALSTART         VALIDATE START DATE                          
         BAS   RE,RETRANS          PERFORM RETRANSFER                           
         BAS   RE,MAIN             PERFORM REST OF TRANSFER                     
         B     VKX30                                                            
*                                                                               
VKX20    BAS   RE,MAIN             ELSE PERFORM FIRST TIME TRANSFER             
*                                                                               
VKX30    B     EXIT                BUY TRANSFERS USE VALKEY ONLY                
*                                                                               
*                                  OVERNIGHT TRANSFER...                        
VKX50    BAS   RE,VALSTART         VALIDATE START DATE                          
         MVI   TRACEOPT,C'N'       VALIDATE TRACE OPTION                        
         CLI   BUYOPT,C'T'                                                      
         BNE   *+8                                                              
         MVI   TRACEOPT,C'Y'                                                    
         B     XIT                 GO BACK TO GENCON - NEXT PRINTREP            
         EJECT                                                                  
INITSEST NTR1                                                                   
         LA    R7,SVSUBEST         POINT R7 TO SUB ESTIMATE TABLE               
*                                                                               
IS10     CLI   SNUM,0              AND SET NEXT BUYLINE TO 1 FOR ALL            
         BE    ISX                     SUB ESTIMATES                            
         MVI   SBUYLINE,1                                                       
         LA    R7,SESTLSTL(R7)                                                  
         B     IS10                                                             
*                                                                               
ISX      B     XIT                                                              
         SPACE 2                                                                
TESTBUY  NTR1                                                                   
         LA    R7,SVSUBEST         POINT R7 TO SUB ESTIMATE LIST                
*                                                                               
TB10     BAS   RE,BLDBKEY          TEST IF BUY ALREADY EXISTS                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    YES                                                              
*                                                                               
         LA    R7,SESTLSTL(R7)     BUMP TO NEXT ESTIMATE                        
         CLI   0(R7),0                                                          
         BNE   TB10                                                             
         B     NO                                                               
         SPACE 2                                                                
FRCRETR  NTR1                                                                   
         CLC   BUYRETR(2),=C'OK'   CHECK FOR OK TO GO AHEAD WITH                
         BE    FRX                     RETRANSFER                               
         CLC   BUYRETR(3),=C'YES'                                               
         BNE   RETRERR             MESSAGE ASKING FOR OK TO RETRANSFER          
FRX      B     XIT                                                              
         SPACE 2                                                                
VALSTART NTR1                                                                   
         LA    R2,BUYDATEH         VALIDATE START DATE FIELD                    
         GOTO1 DATVAL,DMCB,(0,8(R2)),THISDATE                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRDATE                                                          
         CLC   THISDATE,QSTART     DATE MUST BE INSIDE MASTER ESTIMATE          
         BL    ERROUT                  START AND END DATES                      
         CLC   THISDATE,QEND                                                    
         BH    ERROUT                                                           
         GOTO1 GETDAY,DMCB,THISDATE,WORK                                        
         CLI   0(R1),1             DATE MUST BE MONDAY DATE                     
         BNE   ERRMON                                                           
*                                  CONVERT TO 2-BYTE COMPRESSED DATE            
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,TRNSTART)                            
VSX      B     XIT                                                              
         EJECT                                                                  
PR       L     R4,ASPOOLD                                                       
         USING SPOOLD,R4                                                        
* COPY DCB TO CORE-RESIDENT STORAGE                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   PR05                                                             
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T218FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
         ST    RE,AREQDSN                SET CORE RES ADDRESS                   
         OC    0(100,RE),0(RE)           TEST FIRST TIME (NMOD PRESENT)         
         BNZ   PR05                                                             
         L     RF,=A(REQDSN)                                                    
         MVC   0(128,RE),0(RF)     MOVE REQDSN DCB                              
* OPEN THE CORE RES DCB                                                         
         L     R2,AREQDSN                                                       
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PR05     XC    BMKTSTA,BMKTSTA     INITIALIZE MKT/STA TO ZEROS                  
*                                                                               
PR10     BAS   RE,NXMKTSTA         GET NEXT MKT/STA                             
         OC    BMKTSTA,BMKTSTA                                                  
         BZ    PRX                 DONE IF NO MORE                              
*                                                                               
         MVC   P(5),QSTA           OUTPUT STATION TO REPORT                     
         GOTO1 SPOOL,DMCB,(R4)                                                  
*                                                                               
         BAS   RE,INITSEST         INITIALIZE SUBEST LIST                       
*                                                                               
         BAS   RE,TESTPAID         IF PAID BUYLINES EXIST                       
         BNE   PR20                                                             
         MVC   P(44),=C'    ** THIS STATION SKIPPED DUE TO PAID BUYS'           
         GOTO1 SPOOL,DMCB,(R4)                                                  
         B     PR30                                                             
*                                                                               
PR20     BAS   RE,RETRANS          PERFORM RETRANSFER                           
         BAS   RE,MAIN             PERFORM REST OF TRANSFER                     
*                                                                               
PR30     CLI   TRACEOPT,C'Y'       IF TRACING THEN FORCE PAGE EJECT             
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     PR10                                                             
*                                                                               
PRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* GET NEXT MKT/STA TO PROCESS                                                   
*                                                                               
NXMKTSTA NTR1                                                                   
         LA    R6,KEY              BUILD KEY UP TO CLIENT                       
         USING CSOKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
*                                                                               
         OC    BMKTSTA,BMKTSTA     IF FIRST TIME                                
         BNZ   NM10                                                             
         MVC   CSOKMKT(5),SVMKTSTA    SET MKTSTA TO START                       
         B     NM15                                                             
*                                                                               
NM10     OC    SVSTA,SVSTA         ELSE IF MKTSTA SPECIFIC                      
         BZ    NM12                                                             
         XC    BMKTSTA,BMKTSTA     THEN RETURN END OF STATIONS                  
         B     NMX                                                              
*                                                                               
NM12     MVC   CSOKMKT(5),BMKTSTA  ELSE SET MKTSTA TO CURRENT                   
         MVC   CSOKEST,X'FF'       SET ESTIMATE TO HIGH TO GET NEXT STA         
*                                                                               
NM15     MVI   RDUPDATE,C'N'       START KEY SEARCH                             
         GOTO1 HIGH                                                             
*                                                                               
NM20     CLC   KEY(5),KEYSAVE      IF CLIENT CHANGES                            
         BE    NM30                                                             
         XC    BMKTSTA,BMKTSTA     THEN RETURN END OF STATIONS                  
         B     NMX                                                              
*                                                                               
NM30     CLC   CSOKMKT(5),BMKTSTA  IF MKTSTA CHANGED                            
         BE    NM90                                                             
         OC    CSOKSTA,CSOKSTA     AND NOT STATION PERC REC                     
         BE    NM90                                                             
*                                                                               
NM40     OC    SVMKT,SVMKT         AND MKT MATCHES                              
         BZ    NM50                                                             
         CLC   CSOKMKT,SVMKT                                                    
         BNE   NM90                                                             
*                                                                               
NM50     OC    SVSTA,SVSTA         AND STA MATCHES                              
         BZ    NM60                                                             
         CLC   CSOKSTA,SVSTA                                                    
         BNE   NM90                                                             
*                                                                               
NM60     CLC   CSOKEST,BMEST       AND ESTIMATE MATCHES                         
         BE    NM100               THEN RETURN MKTSTA                           
*                                                                               
NM90     MVI   RDUPDATE,C'N'       ELSE READ NEXT KEY                           
         GOTO1 SEQ                                                              
         B     NM20                AND LOOP BACK                                
*                                                                               
NM100    MVC   BMKTSTA,CSOKMKT     RETURN MKTSTA                                
*                                                                               
         LA    R2,FAKEFLD          FAKE UP STATION FIELD AND CALL               
         MVI   0(R2),L'FAKEFLD         VALISTA TO READ MASTER RECS              
         GOTO1 MSUNPK,DMCB,CSOKMKT,FULL,8(R2)                                   
         MVI   5(R2),4                                                          
         CLI   11(R2),C' '                                                      
         BNE   *+8                                                              
         MVI   5(R2),3                                                          
         GOTO1 VALISTA                                                          
*                                                                               
NMX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LOOP THROUGH BUY RECORDS FOR THIS STATION AND LOOK FOR BUY ELEMS THAT         
* COME AFTER START DATE AND HAVE A PAID DATE.  RETURN TRUE IF ONE FOUND         
*                                                                               
TESTPAID NTR1                                                                   
         LA    R7,SVSUBEST         POINT R7 TO SUB ESTIMATE LIST                
*                                                                               
TP5      BAS   RE,BLDBKEY          BUILD BUY START KEY FOR THIS SUBEST          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
TP10     CLC   KEY(10),KEYSAVE     IF NO MORE LINES THEN NEXT SUBEST            
         BNE   TP100                                                            
*                                                                               
         MVI   RDUPDATE,C'N'       READ RECORD                                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT R6 TO FIRST BUY ELEMENT                
         BAS   RE,GETBYEL                                                       
         BNE   TP90                                                             
         USING REGELEM,R6                                                       
*                                                                               
TP20     CLC   RDATE,TRNSTART      IF BUY ELEM IS AFTER START DATE              
         BL    TP30                                                             
         OC    RPAY,RPAY           AND BUY IS PAID FOR THEN RET 'YES'           
         BNZ   YES                                                              
*                                                                               
TP30     BAS   RE,NEXTBYEL         ELSE GET NEXT BUY ELEMENT AND LOOP           
         BE    TP20                                                             
*                                                                               
TP90     MVI   RDUPDATE,C'N'       GET NEXT BUY KEY AND LOOP BACK               
         GOTO1 SEQ                                                              
         B     TP10                                                             
*                                                                               
TP100    LA    R7,SESTLSTL(R7)     BUMP TO NEXT ESTIMATE                        
         CLI   0(R7),0                                                          
         BNE   TP5                                                              
         B     NO                  RETURN 'NO' PAID BUYLIES                     
         EJECT                                                                  
* LOOP THROUGH BUY RECORDS FOR THIS STATION AND KEEP BUYLINES THAT COME         
* BEFORE TRANSFER START DATE.                                                   
*                                                                               
RETRANS  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         LA    R7,SVSUBEST         POINT R7 TO ESTIMATE TO START WITH           
*                                                                               
RT50     CLI   0(R7),0             IF END OF LIST THEN DIE                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SEND,TRNSTART       ELSE IF START DATE IN EST THEN FOUND         
         BNL   RT60                                                             
         LA    R7,SESTLSTL(R7)     ELSE BUMP TO NEXT AND TRY AGAIN              
         B     RT50                                                             
*                                                                               
RT60     LA    R5,1                R5 = INPUT LINE NUMBER                       
*                                                                               
RT70     BAS   RE,BLDBKEY          BUILD BUY KEY AND INSERT INPUT LINE#         
         STC   R5,KEY+11                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(10),KEYSAVE     IF NO MORE LINES THEN DONE                   
         BNE   RTX                                                              
         IC    R5,KEY+11           ELSE SAVE LINE THAT WAS FOUND                
*                                                                               
         GOTO1 GETREC              READ RECORD AND POINT TO DESC ELEM           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BDELEM,R6                                                        
*                                                                               
*                                  WRITE BACK RECORDS THAT HAVE BUYS            
*                                      BEFORE TRANSFER START DATE               
         GOTO1 DATCON,DMCB,(3,BDSTART),(2,THISDATE)                             
         CLC   THISDATE(2),TRNSTART                                             
         BNL   RT160                                                            
*                                                                               
         L     R6,AIO              MAKE SURE FIRST BUY ELEMENT IS               
         BAS   RE,GETBYEL              REALLY BEFORE TRANSFER START             
         BNE   RT160                   DATE                                     
         USING REGELEM,R6                                                       
         CLC   RDATE,TRNSTART                                                   
         BNL   RT160                                                            
*                                                                               
         L     R6,AIO              POINT BACK TO DESC ELEMENT                   
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BDELEM,R6                                                        
*                                  REMOVE BUY ELEMENTS THAT COME AFTER          
*                                      TRANSFER START DATE                      
         GOTO1 DATCON,DMCB,(3,BDEND),(2,THISDATE)                               
         CLC   THISDATE(2),TRNSTART                                             
         BL    RT150                                                            
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              POINT R6 TO FIRST BUY ELEMENT                
         BAS   RE,GETBYEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REGELEM,R6                                                       
*                                                                               
RT100    CLC   RDATE,TRNSTART      LOOP UNTIL ELEMENT HAS DATE ON OR            
         BNL   RT110                   AFTER TRANSFER START DATE                
         MVC   LASTSPLT,RDATE      SAVE LAST MONDAY IN RECORD                   
         BAS   RE,NEXTBYEL                                                      
         BE    RT100                                                            
*                                                                               
RT110    BAS   RE,CHKBYEL          WHILE R6 STILL POINTS TO BUY ELEM            
         BNE   RT120                                                            
*                                                                               
RT115    OC    RPAY,RPAY           IF BUY IS PAID FOR THEN DIE                  
         BZ    *+6                                                              
         DC    H'0'                ELSE REMOVE BUY ELEMENT                      
         GOTO1 RECUP,DMCB,(0,AIO),(R6)                                          
         B     RT110                                                            
         DROP  R6                                                               
*                                                                               
RT120    L     R6,AIO              UPDATE END DATE IN DESCRIPTION ELEM          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BDELEM,R6                                                        
*                                  GET DAY OF WEEK OF START DATE                
         GOTO1 DATCON,DMCB,(3,BDSTART),(0,THISDATE)                             
         GOTO1 GETDAY,DMCB,THISDATE,WORK                                        
         ZIC   R2,0(R1)                                                         
*                                  GET DAY OF WEEK OF OLD END DATE              
         GOTO1 DATCON,DMCB,(3,BDEND),(0,THISDATE)                               
         GOTO1 GETDAY,DMCB,THISDATE,WORK                                        
         ZIC   R3,0(R1)                                                         
*                                  CALCULATE DIFFERENCE                         
         SR    R3,R2                                                            
*                                  ADD DIFFERENCE TO LAST BUY DATE              
         GOTO1 DATCON,DMCB,(2,LASTSPLT),(0,THISDATE)                            
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,(R3)                                
*                                  SAVE NEW END DATE                            
         GOTO1 DATCON,DMCB,(0,THISDATE),(3,BDEND)                               
         DROP  R6                                                               
*                                                                               
RT150    MVC   KEY+11(1),SBUYLINE  CHANGE LINE NUMBER IN KEY AND IOAREA         
         L     R6,AIO                  TO OUTPUT LINE NUMBER                    
         MVC   10(1,R6),SBUYLINE                                                
         BAS   RE,WRITBREC         WRITE BACK BUY RECORD                        
         B     RT180                                                            
*                                                                               
RT160    L     R6,AIO              FOR RECORDS THAT COME AFTER THE              
         BAS   RE,GETBYEL              TRANSFER START DATE, TEST IF THE         
         BNE   RT180                   FIRST BUY IS A PAID BUY                  
         USING REGELEM,R6                                                       
         OC    RPAY,RPAY                                                        
         BZ    *+6                                                              
         DC    H'0'                DIE IF BUY IS PAID                           
         DROP  R6                                                               
*                                                                               
RT180    LA    R5,1(R5)            BUMP INPUT LINE NUMBER                       
         B     RT70                                                             
*                                                                               
RTX      B     XIT                                                              
         EJECT                                                                  
* MAIN ROUTINE - FOR EACH PROGRAM RECORD CALL ROUTINE BFORPROG TO               
*     MAKE HOWEVER MANY BUYLINES THE PROGRAM WILL GENERATE                      
*                                                                               
MAIN     NTR1                                                                   
         BAS   RE,BLDDESC          BUILD BUY DESCRIPTION ELEMENT                
*                                                                               
         BAS   RE,BLDPKEY          BUILD PROGRAM KEY                            
*                                                                               
M10      MVI   RDUPDATE,C'N'       WHILE PROGRAM KEYS STILL EXIST               
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   MX                                                               
*                                                                               
         MVC   PROGKEY,KEY         BACK UP KEY INTO PROGKEY                     
         LA    R4,KEY              SAVE REFERENCE NUMBER                        
         USING CSOKEY,R4                                                        
         MVC   THISREF,CSOKREF                                                  
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1            READ RECORD INTO IO1                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,ADDPDESC         ADD PROGRAM INFO TO BUY DESC ELEM            
*                                                                               
         BAS   RE,GETDEMOV         TRY TO READ DEMO OVERRIDES INTO MYIO         
*                                                                               
         BAS   RE,BFORPROG         MAKE ALL BUYS FOR THIS PROGRAM               
*                                                                               
         MVC   KEY,PROGKEY         RESTORE PROGRAM KEY AND POINT TO             
         LA    R4,KEY                  NEXT REFERENCE NUMBER                    
         USING CSOKEY,R4                                                        
         ZIC   R2,CSOKREF                                                       
         LA    R2,1(R2)                                                         
         STC   R2,CSOKREF                                                       
         B     M10                 END OF WHILE LOOP                            
         DROP  R4                                                               
*                                                                               
MX       LA    R4,KEYSAVE          ERROR IF NO PROGRAM KEYS WERE FOUND          
         USING CSOKEY,R4                                                        
         CLI   CSOKREF,0                                                        
         BE    ERRPNF                                                           
         DROP  R4                                                               
*                                                                               
         BAS   RE,CLEANUP          ELSE DELETE UNUSED BUYLINES                  
*                                                                               
         BAS   RE,BLDREQ           ELSE GENERATE REQUEST                        
         B     XIT                                                              
         EJECT                                                                  
* BUILD BUY RECORD DESCRIPTION ELEMENT                                          
*                                                                               
BLDDESC  NTR1                                                                   
         LA    R6,DESCELEM                                                      
         USING BDELEM,R6                                                        
         XC    DESCELEM,DESCELEM                                                
         MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,X'46'                                                      
         MVI   BDINPUT,2                                                        
         MVI   BDWKIND,C'O'                                                     
         MVC   BDSEC,MASTSPLN                                                   
         MVC   BDNTAX,SVTAX                                                     
         GOTO1 DATCON,DMCB,(5,0),(3,BDCHG)                                      
         MVI   BDWHY,X'80'                                                      
         MVI   BDSTAT,X'04'                                                     
         MVC   BDMASPRD(1),PTAPRD  NON-ZERO FOR PTA ONLY                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* BUILD PROGRAM KEY FROM VALIDATED FIELDS                                       
*                                                                               
BLDPKEY  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CSOKEY,R4                                                        
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT(5),BMKTSTA                                               
         MVC   CSOKEST,BMEST                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* READ DEMO EVERRIDES FOR THIS PROGRAM INTO MYIO IF THE RECORD EXISTS           
*                                                                               
GETDEMOV NTR1                                                                   
         L     RE,MYIO                                                          
         MVI   0(RE),0                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DEMKEY,R4                                                        
         MVI   DEMKTYPE,DEMKTYPQ                                                
         MVI   DEMKSTYP,DEMKSTPQ                                                
         MVC   DEMKAM,BAGYMD                                                    
         MVC   DEMKCLT,BCLT                                                     
         MVC   DEMKMKT(5),BMKTSTA                                               
         MVC   DEMKEST,BMEST                                                    
         MVC   DEMKREF,THISREF                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XIT                                                              
*                                                                               
         MVC   AIO,MYIO                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     XIT                                                              
         EJECT                                                                  
* ADD PROGRAM INFORMATION TO BUY DESCRIPTION ELEMENT                            
*                                                                               
ADDPDESC NTR1                                                                   
         L     R6,AIO1             POINT TO PROGRAM DESCRIPTION ELEMENT         
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
         MVC   PROGNAME(17),DSCPROG                                             
         OC    PROGNAME(17),=CL17' '                                            
         MVI   PROGNAME+17,C' '                                                 
*                                                                               
         LA    R4,DESCELEM         POINT TO BUY DESCRIPTION ELEMENT             
         USING BDELEM,R4                                                        
*                                                                               
         MVC   BDTIMST(4),DSCTIME      MOVE IN TIMES AND PROGRAM NAME           
         MVC   BDDAYPT,DSCDPT                                                   
         MVC   BDPROGT,DSCADJ                                                   
         MVC   BDDAY,DSCDAY                                                     
*                                  CALCULATE DISPLACEMENT TO START DAY          
         GOTO1 UNDAY,DMCB,DSCDAY,(X'07',WORK)                                   
         LA    RE,WORK                                                          
         CLI   0(RE),C'.'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         LA    RF,WORK                                                          
         SR    RE,RF                                                            
         STC   RE,STARTDAY         SAVE DISPLACEMENT IN STARTDAY                
*                                                                               
         LA    RE,WORK+6           CALCULATE DISPLACEMENT TO END DAY            
         CLI   0(RE),C'.'                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RF,WORK                                                          
         SR    RE,RF                                                            
         STC   RE,ENDDAY           SAVE DISPLACEMENT IN ENDDAY                  
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* BUY HOWEVER MANY LINES THE PROGRAM RECORD GENERATES                           
*                                                                               
BFORPROG NTR1                                                                   
         L     R6,AIO1             POINT R6 TO FIRST WEEK ELEMENT               
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
         ST    R6,AFSTWK           SAVE ADDRESS OF FIRST WEEK ELEMENT           
         XC    BLINTAB,BLINTAB     CLEAR BUY LINE TABLE FOR THIS PROG           
*                                                                               
         LA    R7,SVSUBEST         POINT R7 TO SUB ESTIMATE TABLE               
*                                                                               
BFP10    CLI   0(R7),0             WHILE NOT END OF SUB-ESTIMATE LIST           
         BE    BFP100                                                           
*                                                                               
         ST    R6,ESTSTART         STORE POINTER TO START WEEK IN EST           
*                                                                               
BFP20    CLI   0(R6),WKCODEQ       WHILE POINTER BEFORE END OF EST              
         BNE   BFP30                                                            
         CLC   WKDATE,SEND                                                      
         BNL   BFP30                                                            
*                                                                               
         CLC   WKDATE,TRNSTART     IF THIS WEEK IS TRANSFER START WEEK          
         BNE   *+8                                                              
         ST    R6,ESTSTART         THEN SET ESTIMATE START TO THIS ELEM         
*                                                                               
         LA    R6,WKLENQ(R6)       BUMP POINTER                                 
         B     BFP20               END OF INNER WHILE LOOP                      
*                                                                               
BFP30    ST    R6,ESTEND           STORE POINTER TO END OF EST                  
*                                                                               
BFPP10   CLC   SEND,TRNSTART       SKIP ESTIMATES BEFORE START DATE             
         BL    BFP40                                                            
*                                                                               
         BAS   RE,BLDDEMO          BUILD DEMO ELEMENT                           
*                                                                               
         MVI   COSTFLAG,C'T'       BUY TRADE SPOTS FOR THIS EST                 
         BAS   RE,BFOREST                                                       
         MVI   COSTFLAG,C'C'       BUY CASH SPOTS FOR THIS EST                  
         BAS   RE,BFOREST                                                       
*                                                                               
BFP40    LA    R7,SESTLSTL(R7)     BUMP TO NEXT SUB-ESTIMATE                    
         B     BFP10               END OF OUTER WHILE LOOP                      
*                                                                               
BFP100   BAS   RE,ADDBLIN          ADD BLINTAB ELEMENT TO PROGRAM REC           
*                                                                               
BFPX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* BUILD DEMO ELEMENT FOR THIS PROGRAM/ESTIMATE                                  
*                                                                               
BLDDEMO  NTR1                                                                   
*                                                                               
         LA    R6,DEMOELEM         PUT ELCODE AND BOOK IN DEMO ELEMENT          
         USING NDELEM,R6                                                        
         XC    DEMOELEM,DEMOELEM                                                
         MVI   NDCODE,X'02'                                                     
         MVC   NDBOOK,SBOOK                                                     
*                                                                               
BLDD30   LA    R5,BLOCK            FILL DEMO LOOKUP PARAMETER BLOCK             
         USING SPDEMLK,R5                                                       
         XC    0(SPDEMLKL,R5),0(R5)                                             
         LA    RE,DEMLKSV                                                       
         ST    RE,SPLKXTND                                                      
         MVC   SPLKAREC,AIO3       A(1000 BYTE IOAREA)                          
         L     RE,SYSPARMS         A(COMFACS)                                   
         L     RE,16(RE)                                                        
         ST    RE,SPLKAFAC                                                      
         LA    RE,QDEMOS           A(DEMO LIST)                                 
         ST    RE,SPLKALST                                                      
         L     RE,AIO3             A(DEMO VALUES)                               
         LA    RE,1000(RE)                                                      
         ST    RE,SPLKAVAL                                                      
         MVI   SPLKFIL,C'T'        FILE CODE                                    
         MVI   SPLKMED,C'T'        MEDIA CODE                                   
         CLI   SVCPROF+3,C'0'      SOURCE CODE                                  
         BNE   *+12                                                             
         MVI   SPLKSRC,C'N'                                                     
         B     *+8                                                              
         MVI   SPLKSRC,C'A'                                                     
         MVC   SPLKAGY,AGENCY      AGENCY ALPHA                                 
         MVC   SPLKCLI,QCLT        CLIENT CODE                                  
         MVC   SPLKDBK,SBOOK       DEMO BOOK                                    
         MVC   SPLKSTA,QSTA        STATION CALL LETTERS                         
         MVC   SPLKUMK,BMKT        USER MARKET NUMBER                           
*                                                                               
         LA    R4,DESCELEM                                                      
         USING BDELEM,R4                                                        
         MVC   SPLKDAY,BDDAY       DAY CODE                                     
         MVC   SPLKTIM,BDTIMST     MILITARY START AND END TIMES                 
*                                                                               
         MVC   SPLKSVI,SHUTADJ     SVI CODE                                     
         OC    SHUTADJ,SHUTADJ     IF HUTADJ IS ZERO                            
         BNZ   BLDD40                                                           
         MVC   SPLKAUTF,BDWKIND    ADD INFO FOR AUTO SVI                        
         MVC   SPLKAUST,SSTART                                                  
         MVC   SPLKAUND,SEND                                                    
         DROP  R4                                                               
*                                                                               
BLDD40   GOTO1 GETDEM2,DMCB,BLOCK     GET DEMO VALUES                           
*                                                                               
         MVC   NDPROG,SPLKPRG                                                   
*                                                                               
         LA    R2,NDEMNO           PUT DEMO VALUES INTO ELEMENT                 
         L     R3,SPLKALST                                                      
         L     R4,SPLKAVAL                                                      
         DROP  R5                                                               
*                                                                               
BLDD50   CLI   0(R3),X'FF'         TEST END OF DEMO LIST                        
         BE    BLDD90                                                           
         MVC   0(3,R2),0(R3)                                                    
         MVC   3(1,R2),7(R4)                                                    
         MVC   4(4,R2),0(R4)                                                    
         LA    R2,8(R2)                                                         
         LA    R3,3(R3)                                                         
         LA    R4,8(R4)                                                         
         B     BLDD50                                                           
*                                                                               
BLDD90   LR    R4,R2               SAVE POINTER TO END OF ELEMENT               
         SR    R2,R6               CALCULATE ELEMENT LENGTH                     
         STC   R2,NDLEN            SAVE IN ELEMENT                              
*                                                                               
         L     RF,MYIO             IF DEMO OVERRIDE RECORD FOUND                
         CLI   0(RF),X'0D'                                                      
         BNE   BLDD190                                                          
*                                                                               
         LA    R2,NDEMNO           THEN USE IT TO OVERRIDE GIVEN DEMOS          
         MVC   AIO,MYIO                                                         
*                                                                               
BLDD100  CR    R2,R4               TEST END OF DEMO VALUE LIST                  
         BNL   BLDD190                                                          
         GOTO1 FINDOVR,DMCB,(R2),SBOOK                                          
         CLC   2(2,R1),=X'FFFF'    TEST NO OVERRIDE FOR THIS DEMO               
         BE    BLDD110                                                          
         MVI   3(R2),100           SET SVI % TO 100                             
         MVI   4(R2),X'80'         SET STATUS BIT FOR OVERRIDE                  
         MVC   6(2,R2),2(R1)       MOVE IN OVERRIDING VALUE                     
*                                                                               
BLDD110  LA    R2,8(R2)            BUMP TO NEXT DEMO                            
         B     BLDD100                                                          
*                                                                               
BLDD190  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* BUY HOWEVER MANY LINES THE ESTIMATE GENERATES                                 
*                                                                               
BFOREST  NTR1                                                                   
         L     R6,ESTSTART         POINT R6 TO FIRST WEEK IN EST                
         USING CSOWKEL,R6                                                       
*                                                                               
BFE10    C     R6,ESTEND           WHILE R6 ( END OF ESTIMATE                   
         BNL   BFEX                                                             
         BAS   RE,BLDBKEY          BUILD BUY KEY                                
         BAS   RE,MOVKIO2          MOVE BUY KEY TO IO2                          
*                                                                               
         MVC   NORMCOST,WKCOST     SET RATE FOR BUY DESC ELEM                   
         XC    BUYSTART,BUYSTART   CLEAR DATE OF FIRST BUY ELEMENT              
         XC    NUMWEEKS,NUMWEEKS   CLEAR WEEK COUNTER                           
         XC    MODETAB,MODETAB     CLEAR MODETAB                                
         SR    R2,R2               CLEAR SPOT COUNTER                           
*                                                                               
BFE20    C     R6,ESTEND           WHILE R6 ( END OF ESTIMATE                   
         BNL   BFE100                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(10),BUYELEM    CREATE BUY ELEMENT AND PUT THIS              
         CLI   PTAPRD,0            TEST TO ALLOCATE THIS SPOT                   
         BE    BFE22                                                            
         MVI   ELEM+1,14           RESET ELEMENT LENGTH                         
         MVC   ELEM+10(1),PTAPRD                                                
         MVC   ELEM+11(1),MASTSPLN                                              
*                                                                               
BFE22    LA    R5,ELEM                 WEEK'S DATE IN IT                        
         USING REGELEM,R5                                                       
         GOTO1 DATCON,DMCB,(2,WKDATE),(0,THISDATE)                              
         ZIC   R3,STARTDAY         ADD DISPLACEMENT TO FIRST DAY                
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,(R3)                                
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,RDATE)                               
*                                                                               
         CLC   WKCOST,NORMCOST     IF THIS WEEK'S COST IS DIFFERENT             
         BE    BFE30                                                            
         OI    RSTATUS,X'20'       SET BUY ELEM TO OVERRIDE NORMCOST            
         ICM   RF,15,WKCOST                                                     
         M     RE,=F'100'                                                       
         STCM  RF,7,RPCOST                                                      
         DROP  R5                                                               
*                                                                               
BFE30    CLI   COSTFLAG,C'T'       POINT TO TRADE/CASH SPOTS                    
         BNE   *+12                                                             
         LA    R4,WKTSPOTS                                                      
         B     *+8                                                              
         LA    R4,WKCSPOTS                                                      
         SR    R3,R3               R3 = NUMBER OF SPOTS                         
         ICM   R3,3,0(R4)                                                       
         C     R3,=F'0'                                                         
         BE    BFE90                                                            
*                                                                               
         C     R3,=A(MAXSPOTS)     IF SPOTS FOR THIS WEEK ALONE ARE             
         BNH   BFE40                   GREATER THAN MAXSPOTS AND THIS           
         C     R2,=F'0'                IS THE FIRST WEEK                        
         BNE   BFE40                                                            
         GOTO1 ADDBUYS,DMCB,MAXSPOTS    THEN ADD THE MAXIMUM SPOTS              
         S     R3,=A(MAXSPOTS)                                                  
         STCM  R3,3,0(R4)          AND SUBTRACT THEM FROM THE PROGREC           
         B     BFE100              THEN BREAK FROM INNER WHILE LOOP             
*                                                                               
BFE40    AR    R2,R3               ELSE INCREMENT SPOT COUNT BY SPOTS           
         C     R2,=A(MAXSPOTS)     IF SPOT COUNT GOES OVER                      
         BH    BFE100              THEN BREAK FROM INNER WHILE LOOP             
*                                                                               
         GOTO1 ADDBUYS,DMCB,(R3)   ELSE ADD SPOTS                               
*                                                                               
BFE90    LA    R6,WKLENQ(R6)       BUMP TO NEXT WEEK                            
         CLC   WKDATE,BUYSPLIT     SPLIT LINES AT THE BUY SPLIT DATE            
         BE    BFE100                                                           
         B     BFE20               END OF INNER WHILE LOOP                      
*                                                                               
BFE100   OC    MODETAB,MODETAB     IF NO BUYS MADE SKIP WRITBREC                
         BZ    BFE10                                                            
*                                                                               
         MVI   MODENUM,0           FIND THE MODE SPOTS/WEEK                     
         LA    RE,1                                                             
*                                                                               
BFE110   C     RE,=A(MAXSPOTS)     WHILE RE (= MAXSPOTS                         
         BH    BFE130                                                           
         LA    RF,MODETAB(RE)      IF MODETAB(RE) ) CURRENT MODE NUMBER         
         CLC   0(1,RF),MODENUM                                                  
         BNH   BFE120                                                           
         MVC   MODENUM,0(RF)       THEN MODE NUMBER = MODETAB(RE)               
         STC   RE,MODESPW          MODE SPOTS/WEEK = RE                         
*                                                                               
BFE120   LA    RE,1(RE)            BUMP RE                                      
         B     BFE110              END OF INNER WHILE LOOP                      
*                                                                               
BFE130   BAS   RE,ADDRDESC         ADD INFO SPECIFIC TO THIS BUYREC             
*                                                                               
         MVC   AIO,AIO2            ADD DESCRIPTION ELEMENT                      
         MVC   ELEM,DESCELEM                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         MVC   ELEM,DEMOELEM       ADD DEMO ELEMENT                             
         GOTO1 ADDELEM                                                          
*                                                                               
         BAS   RE,WRITBREC         WRITE BUY RECORD                             
         B     BFE10               END OF OUTER WHILE LOOP                      
*                                                                               
BFEX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* BUILD BUY KEY                                                                 
*                                                                               
BLDBKEY  NTR1                                                                   
         LA    R4,KEY              BUILD BUY KEY                                
         USING BUYKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVI   BUYKPRD,X'FF'       POOL PRODUCT                                 
         MVC   BUYMSTA,BMKTSTA                                                  
*                                                                               
         MVC   BUYKEST,SNUM        GET ESTIMATE NUMBER AND BUYLINE              
         MVC   BUYKBUY+1(1),SBUYLINE   NUMBER FROM SUB-ESTIMATE LIST            
         MVI   BUYKBUY+2,X'01'                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* MOVE BUY KEY TO IO2                                                           
*                                                                               
MOVKIO2  NTR1                                                                   
         L     RE,AIO2             CLEAR IO2                                    
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO2             MOVE KEY TO IO2                              
         USING BUYREC,R6                                                        
         MVC   BUYKEY(10),KEY                                                   
         MVC   BUYKEY+10(2),KEY+11     SHIFT OVER BUYLINE,X'01'                 
         MVI   BUYKEY+12,0                                                      
         MVC   BUYRLEN,DATADISP    INSERT LENGTH AND AGENCY CODE                
         MVC   BUYALPHA,AGENCY                                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* ADD THE NUMBER OF BUY ELEMS SPECIFED BY PARM ONE TO IO2                       
*                                                                               
ADDBUYS  NTR1                                                                   
         L     R2,0(R1)            R2 = NUMBER OF BUY LINES TO ADD              
*                                                                               
         SR    R0,R0               R1 = ADDRESS OF BLINTAB ENTRY FOR            
         LR    R1,R6                    THIS WEEK                               
         S     R1,AFSTWK                                                        
         D     R0,=A(WKLENQ)                                                    
         SLL   R1,1                                                             
         LA    RF,BLINTAB                                                       
         AR    R1,RF                                                            
         CLI   COSTFLAG,C'T'       TRADE GOES IN FIRST BYTE, CASH               
         BE    *+8                     IN SECOND                                
         LA    R1,1(R1)                                                         
*                                                                               
         ZIC   R0,0(R1)            ADD BUY LINES TO BLINTAB ENTRY               
         AR    R0,R2                                                            
         STC   R0,0(R1)                                                         
*                                                                               
         ZIC   R3,MODETAB(R2)      INCREMENT MODE COUNTER FOR N BUYS            
         LA    R3,1(R3)                                                         
         STC   R3,MODETAB(R2)                                                   
         MVC   AIO,AIO2                                                         
*                                                                               
AB10     GOTO1 ADDELEM             ADD N BUY ELEMENTS                           
         BCT   R2,AB10                                                          
*                                                                               
         USING CSOWKEL,R6                                                       
         OC    BUYSTART,BUYSTART   IF START DATE NOT SET                        
         BNZ   *+10                                                             
         MVC   BUYSTART,WKDATE     THEN SET START DATE TO THIS DATE             
         MVC   BUYEND,WKDATE       SET END DATE TO THIS DATE                    
         DROP  R6                                                               
*                                                                               
         ZIC   R3,NUMWEEKS         INCREMENT WEEK COUNTER                       
         LA    R3,1(R3)                                                         
         STC   R3,NUMWEEKS                                                      
         B     XIT                                                              
         EJECT                                                                  
* ADD INFO SPECIFIC TO THIS BUY RECORD TO THE DESCRIPTION ELEMENT               
*                                                                               
ADDRDESC NTR1                                                                   
         LA    R6,DESCELEM                                                      
         USING BDELEM,R6                                                        
*                                                                               
         MVC   BDPROGRM,PROGNAME   PROGRAM NAME                                 
         CLI   COSTFLAG,C'T'                                                    
         BNE   ARD10                                                            
         LA    RE,BDPROGRM+13      IF TRADE LINE THEN ADD '-T' TO NAME          
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVC   1(2,RE),=C'-T'                                                   
*                                  CALCULATE START DAY                          
ARD10    GOTO1 DATCON,DMCB,(2,BUYSTART),(0,THISDATE)                            
         ZIC   R2,STARTDAY                                                      
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,(R2)                                
         GOTO1 DATCON,DMCB,(0,THISDATE),(3,BDSTART)                             
*                                  CALCULATE END DAY                            
         GOTO1 DATCON,DMCB,(2,BUYEND),(0,THISDATE)                              
         ZIC   R2,ENDDAY                                                        
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,(R2)                                
         GOTO1 DATCON,DMCB,(0,THISDATE),(3,BDEND)                               
*                                                                               
         MVC   BDWKS,NUMWEEKS      NUMBER OF WEEKS                              
         MVC   BDNOWK,MODESPW      SPOTS PER WEEK                               
*                                                                               
         ICM   RF,15,NORMCOST      SPOT COST IN PENNIES                         
         M     RE,=F'100'                                                       
         STCM  RF,7,BDCOST                                                      
*                                                                               
         CLI   COSTFLAG,C'T'       COST INDICATOR = X'00' IF TRADE              
         BNE   *+12                                 X'20' IF CASH               
         MVI   BDCIND,X'00'                                                     
         B     *+8                                                              
         MVI   BDCIND,X'20'                                                     
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* ADD A BLINTAB ELEMENT TO RECORD NUMBER OF BUY LINES THAT WERE                 
* TRANSFERED EACH WEEK.                                                         
*                                                                               
ADDBLIN  NTR1                                                                   
         MVC   AIO,AIO1            READ PROGRAM RECORD FOR UPDATE               
         L     R6,AIO                                                           
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         GOTO1 TRCREC,DMCB,1                                                    
*                                                                               
         MVI   ELCODE,BLCODEQ      REMOVE BLINTAB ELEM IF EXISTS                
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM             BUILD AND ADD BUY LINE TABLE ELEM            
         USING CSOBLEL,R6                                                       
         MVI   BLCODE,BLCODEQ                                                   
         MVI   BLLEN,BLLENQ                                                     
         MVC   BLTAB,BLINTAB                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 TRCREC,DMCB,2                                                    
         CLI   SKIPWRT,C'Y'                                                     
         BE    XIT                                                              
         GOTO1 PUTREC              WRITE RECORD BACK                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* WRITE BUY RECORD TO SPOT FILE                                                 
*                                                                               
WRITBREC NTR1                                                                   
         MVI   ACTELOPT,C'N'       SUPPRESS ACTIVITY ELEMENTS                   
*                                                                               
         ZIC   R2,SBUYLINE         INCREMENT BUYLINE IN SUB-EST LIST            
         LA    R2,1(R2)                                                         
         STC   R2,SBUYLINE                                                      
*                                                                               
         OI    DMINBTS,X'08'       CHECK IF RECORD ISN'T ALREADY THERE          
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     IF RECORD ISN'T ALREADY THERE                
         BE    WBR10                                                            
*                                                                               
         MVC   AIO,AIO2            THEN ADD IT                                  
         GOTO1 TRCREC,DMCB,0                                                    
         CLI   SKIPWRT,C'Y'                                                     
         BE    WBRX                                                             
         GOTO1 ADDREC                                                           
         B     WBRX                                                             
*                                                                               
WBR10    MVC   AIO,AIO3                                                         
         TM    KEY+13,X'80'        ELSE IF IT IS MARKED FOR DELETION            
         BZ    WBR20                                                            
         NI    KEY+13,X'7F'        UNDELETE POINTER AND WRITE BACK              
         CLI   SKIPWRT,C'Y'                                                     
         BE    WBR20                                                            
         GOTO1 WRITE                                                            
*                                                                               
WBR20    GOTO1 GETREC              READ OLD RECORD AND THROW AWAY               
         GOTO1 TRCREC,DMCB,1                                                    
*                                                                               
WBR40    MVC   AIO,AIO2            WRITE NEW RECORD BACK IN ITS PLACE           
         BAS   RE,BLDP6                (POINT DMCB P6 TO PRD LIST)              
         GOTO1 TRCREC,DMCB,2                                                    
         CLI   SKIPWRT,C'Y'                                                     
         BE    WBRX                                                             
         GOTO1 PUTREC                                                           
         XC    DMCB+20(4),DMCB+20      (CLEAR P6)                               
*                                                                               
WBRX     NI    DMINBTS,X'F7'       CLEAR FLAG FOR DELETED RECORDS               
         MVI   ACTELOPT,C'Y'       RESTORE  ACTIVITY ELEMENTS                   
         B     XIT                                                              
         EJECT                                                                  
* BUILD ALLOCATED PRODUCT LIST AND POINT DMCB PARAMETER 6 TO IT.                
*                                                                               
BLDP6    NTR1                                                                   
         XC    ELEM(256),ELEM      R2 = A(PRDS ALLOCATED ELEM)                  
         LA    R2,ELEM                                                          
*                                                                               
         L     R6,AIO              R6 = A(FIRST BUY ELEMENT)                    
         BAS   RE,GETBYEL                                                       
         BNE   BPX                                                              
         USING REGELEM,R6                                                       
*                                                                               
BP10     CLI   RLEN,X'0E'          IF THIS ELEMENT HAS PRD ALLOCATED            
         BNE   BP50                                                             
         ZIC   R3,RPPRD            THEN SET FLAG IN ELEM TO INDICATE            
         AR    R3,R2                   THAT THIS PRODUCT ALLOCATED              
         MVI   0(R3),1                                                          
*                                                                               
BP50     BAS   RE,NEXTBYEL         REPEAT UNTIL NO MORE ELEMENTS                
         BE    BP10                                                             
*                                                                               
         LA    R3,BLOCK            R3 = A(ALLOCATED PRODUCTS LIST)              
         XC    0(256,R3),0(R3)                                                  
         LA    R2,254              R2 = PRODUCT CODE  01-254                    
*                                                                               
BP60     LA    RF,ELEM(R2)         IF THIS PRODUCT IS ALLOCATED                 
         CLI   0(RF),1                                                          
         BNE   BP90                                                             
         STC   R2,0(R3)            THEN SAVE CODE IN LIST AND BUMP LIST         
         LA    R3,1(R3)                                                         
*                                                                               
BP90     BCT   R2,BP60             REPEAT UNTIL PRD CODE IS ZERO                
*                                                                               
         LA    RF,BLOCK            SAVE A(PRD LIST) IN DMCB PARM6               
         ST    RF,DMCB+20                                                       
*                                                                               
BPX      B     XIT                                                              
         EJECT                                                                  
* CLEAN OUT BUY LINES THAT WERE ON THE FILE BUT NO LONGER SHOULD BE             
*                                                                               
CLEANUP  NTR1                                                                   
         OI    DMINBTS,X'08'       SET FLAG TO PASS BACK DELETED RECS           
         MVC   AIO,AIO3                                                         
         LA    R7,SVSUBEST         POINT R7 TO SUB-ESTIMATE LIST                
*                                                                               
CL10     CLI   SNUM,0              TEST END OF SUB-ESTIMATE LIST                
         BE    CLX                                                              
         CLC   SEND,TRNSTART       SKIP ESTIMATES BEFORE START DATE             
         BL    CL90                                                             
         BAS   RE,BLDBKEY          BUILD BUY KEY                                
         GOTO1 HIGH                                                             
*                                                                               
CL20     CLC   KEY(10),KEYSAVE     TEST NO MORE FOR THIS ESTIMATE               
         BNE   CL90                                                             
         TM    KEY+13,X'80'        IF KEY NOT ALREADY DELETED                   
         BO    CL30                                                             
         OI    KEY+13,X'80'        WRITE BACK DELETED KEY                       
         CLI   SKIPWRT,C'Y'                                                     
         BE    CL22                                                             
         GOTO1 WRITE                                                            
*                                                                               
CL22     GOTO1 GETREC              AND WRITE BACK DELETED RECORD                
         GOTO1 TRCREC,DMCB,1                                                    
*                                                                               
         L     R6,AIO              BUT FIRST MAKE SURE OLD REC HAS NO           
         BAS   RE,GETBYEL              PAID BUYS                                
         BNE   CL27                                                             
         USING REGELEM,R6                                                       
*                                                                               
CL25     OC    RPAY,RPAY                                                        
         BZ    *+6                                                              
         DC    H'0'                DIE IF PAID BUY FOUND                        
         BAS   RE,NEXTBYEL                                                      
         BE    CL25                                                             
*                                                                               
CL27     L     R6,AIO                                                           
         OI    15(R6),X'80'        WRITE BACK DELETED RECORD                    
         GOTO1 TRCREC,DMCB,2                                                    
         CLI   SKIPWRT,C'Y'                                                     
         BE    CL30                                                             
         GOTO1 PUTREC                                                           
*                                                                               
CL30     GOTO1 SEQ                 NEXT KEY                                     
         B     CL20                                                             
*                                                                               
CL90     LA    R7,SESTLSTL(R7)     NEXT SUB-ESTIMATE                            
         B     CL10                                                             
*                                                                               
CLX      NI    DMINBTS,X'F7'       CLEAR FLAG FOR DELETED RECORDS               
         B     XIT                                                              
         EJECT                                                                  
* TWO ROUTINES TO TRACE KEYS AND RECORDS                                        
*                                                                               
TRCKEY   NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         CLI   CONREC,C'O'                                                      
         BNE   TKX                                                              
         CLI   TRACEOPT,C'Y'                                                    
         BNE   TKX                                                              
         L     R2,0(R1)            0 - SEQ, 1 - HIGH                            
         MVC   P(3),=C'KEY'                                                     
         GOTO1 SPOOL,DMCB,(R5)                                                  
         GOTO1 DMCB,TRCMEM,KEY,13                                               
         LTR   R2,R2                                                            
         BZ    TKX                                                              
         MVC   P(7),=C'KEYSAVE'                                                 
         GOTO1 SPOOL,DMCB,(R5)                                                  
         GOTO1 DMCB,TRCMEM,KEYSAVE,13                                           
TKX      B     XIT                                                              
TRCREC   NTR1                                                                   
         L     R5,ASPOOLD                                                       
         CLI   CONREC,C'O'                                                      
         BNE   TKX                                                              
         CLI   TRACEOPT,C'Y'                                                    
         BNE   TRX                                                              
         L     R2,0(R1)            0 - ADD, 1 - COPY, 2 -CHANGE                 
         L     R3,AIO                                                           
         SR    R4,R4               R3 = AIO, R4 = RECORD LENGTH                 
         ICM   R4,3,CSOLEN-CSORECD(R3)                                          
         MVC   P(3),=C'ADD'                                                     
         LTR   R2,R2                                                            
         BZ    TR10                                                             
         MVC   P(4),=C'COPY'                                                    
         C     R2,=F'1'                                                         
         BE    TR10                                                             
         MVC   P(6),=C'CHANGE'                                                  
TR10     GOTO1 SPOOL,DMCB,(R5)                                                  
         GOTO1 TRCMEM,DMCB,(R3),(R4)                                            
TRX      B     XIT                                                              
TRCMEM   NTR1                                                                   
         L     R5,ASPOOLD                                                       
         LM    R2,R3,0(R1)         R2 = A(DATA), R3 = LEN(DATA)                 
TM10     LA    R4,64                                                            
         CR    R4,R3                                                            
         BNH   *+6                                                              
         LR    R4,R3                                                            
         GOTO1 HEXOUT,DMCB,0(R2),P,(R4)                                         
         MVC   BLK32,P+96                                                       
         MVC   P+99(32),BLK32                                                   
         MVI   P+98,C' '                                                        
         MVC   BLK32,P+64                                                       
         MVC   P+68(32),BLK32                                                   
         MVI   P+67,C' '                                                        
         MVC   BLK32,P+32                                                       
         MVC   P+33(32),BLK32                                                   
         MVI   P+32,C' '                                                        
         GOTO1 SPOOL,DMCB,(R5)                                                  
         C     R3,=F'64'                                                        
         BNH   TMX                                                              
         S     R3,=F'64'                                                        
         LA    R2,64(R2)                                                        
         B     TM10                                                             
TMX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* GENERATE U3 REQUESTS FOR EACH SUB-ESTIMATE                                    
*                                                                               
BLDREQ   NTR1                                                                   
         L     R6,AIO              BUILD REQUEST RECORD                         
         XC    0(26,R6),0(R6)                                                   
         MVI   14(R6),106                                                       
         LA    R6,26(R6)                                                        
         MVC   0(80,R6),MYSPACES                                                
         MVC   0(2,R6),=C'U3'                                                   
         MVC   2(2,R6),AGENCY                                                   
         MVC   4(1,R6),QMED                                                     
         MVC   5(3,R6),QCLT                                                     
         MVC   8(2,R6),=C'NN'                                                   
         MVC   11(3,R6),=C'POL'                                                 
         EDIT  (2,BMKT),(4,14(R6)),FILL=0                                       
         MVC   18(5,R6),QSTA                                                    
         MVC   68(12,R6),=CL12'JOSEPHINE'                                       
*                                                                               
         LA    R7,SVSUBEST         POINT TO SUB ESTIMATE LIST                   
*                                                                               
BQ10     CLI   SNUM,0              WHILE NOT END OF ESTIMATE LIST               
         BE    BQX                                                              
         CLC   SEND,TRNSTART       SKIP ESTIMATES BEFORE START DATE             
         BL    BQ20                                                             
*                                                                               
         EDIT  (1,SNUM),(3,23(R6)),FILL=0                                       
         GOTO1 DATCON,DMCB,(2,SSTART),(0,37(R6))                                
         GOTO1 DATCON,DMCB,(2,SEND),(0,43(R6))                                  
*                                                                               
         CLI   CONREC,C'O'         IF OVERNITE TRANSFER THEN PUT                
         BNE   BQ18                    REQUEST TO VIRTUAL REQFILE               
         L     R1,AREQDSN                                                       
         PUT   (R1),(R6)                                                        
         B     BQ20                                                             
*                                                                               
BQ18     CLI   SKIPWRT,C'Y'        ELSE WRITE REQUEST TO REAL REQFILE           
         BE    BQ20                                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
*                                                                               
BQ20     LA    R7,SESTLSTL(R7)     BUMP TO NEXT SUBEST                          
         B     BQ10                                                             
*                                                                               
BQX      B     XIT                                                              
         EJECT                                                                  
* TWO SPECIAL ROUTINES TO CICLE THROUGH BUY ELEMENTS AND THEN                   
* THE NORMAL GETEL MACRO.                                                       
*                                                                               
GETBYEL  DS    0H                                                               
         AH    R6,DATADISP                                                      
*                                                                               
CHKBYEL  CLI   0(R6),0                                                          
         BE    GBNE                                                             
         CLI   0(R6),X'0B'                                                      
         BE    GBEQ                                                             
         CLI   0(R6),X'0C'                                                      
         BE    GBEQ                                                             
         CLI   0(R6),X'0D'                                                      
         BE    GBEQ                                                             
NEXTBYEL ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CHKBYEL                                                          
*                                                                               
GBEQ     CR    RC,RC                                                            
         BR    RE                                                               
*                                                                               
GBNE     LTR   RC,RC                                                            
         BR    RE                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
ERRDATE  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'** ERROR INVALID DATE EXPRESSION'                 
         GOTO1 ERREX2                                                           
*                                                                               
ERROUT   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'** ERROR DATE OUTSIDE ESTIMATE DATES'             
         GOTO1 ERREX2                                                           
*                                                                               
ERRMON   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'** ERROR MUST BE MONDAY DATE'                     
         GOTO1 ERREX2                                                           
*                                                                               
ERRPNF   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'** ERROR PROGRAM RECORDS NOT FOUND'               
         LA    R2,BUYMEDH             SET CURSOR                                
         GOTO1 ERREX2                                                           
*                                                                               
RETRERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'ENTER ''OK'' OR ''YES'' TO RETRANSFER'            
         LA    R2,BUYRETRH            SET CURSOR                                
         GOTO1 ERREX2                                                           
*                                                                               
ERREMP   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(42),=C'** ERROR - NOTHING HAS BEEN TRANSFERED YEX        
               T'                                                               
         GOTO1 ERREX2                                                           
*                                                                               
ERRLOCK  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(49),=C'** ERROR - BUY DATA LOCKED FOR OFFLINE PRX        
               OCESSING'                                                        
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(18),=C'TRANSFER COMPLETED'                               
         LA    R2,BUYMEDH             SET CURSOR                                
         GOTO1 ERREX2                                                           
*                                                                               
BUYELEM  DC    X'0B',X'0A',9X'00'                                               
MYSPACES DC    CL80' '                                                          
*                                                                               
         DS    0D                                                               
AREQDSN  DS    A                   CORE RES ADDRESS OF DCB                      
REQDSN   DCB   DDNAME=REQDSN,DSORG=PS,RECFM=FB,LRECL=80,               X        
               BLKSIZE=2000,MACRF=PM                                            
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------*              
* FOR CLIENT PTA, FIND THE PRODUCT CODE FOR THIS ESTIMATE NUMBER *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
GETPTAPR NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         LA    R4,SVCLIST-4                                                     
*                                                                               
GP10     LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNH   GPX                                                              
         CLC   =C'POL',0(R4)                                                    
         BE    GP10                                                             
* BUILD NEXT ESTHDR KEY                                                         
         MVC   KEY+4(3),0(R4)      MOVE IN PRODUCT                              
         MVC   KEY+7(1),BMEST      READ FOR THE MASTER ESTIMATE                 
         XC    KEY+8(5),KEY+8                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   GP10                                                             
         CLI   PTAPRD,0                                                         
         BNE   GPERR                                                            
         MVC   PTAPRD,3(R4)        SET BINARY PRODUCT CODE                      
         B     GP10                                                             
*                                                                               
GPX      CLI   PTAPRD,0            TEST FOUND ANY PRODUCT                       
         BE    GPERR2                                                           
         XIT1                                                                   
*                                                                               
GPERR    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(41),=C'** MORE THAN ONE PRODUCT OPEN FOR EST **'         
         GOTO1 ERREX2                                                           
GPERR2   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'** NO PRODUCT OPEN FOR THIS EST **'               
         GOTO1 ERREX2                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BLK32,BLK32                                                      
L        USING LKKEYD,BLK32                                                     
*                                                                               
         MVC   L.LOCKAGY,AGENCY                                                 
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         MVC   L.LOCKMED,QMED                                                   
         MVC   L.LOCKCLT,QCLT                                                   
         MVC   L.LOCKSTA,QSTA                                                   
         DROP  L                                                                
*                                                                               
TSTLOCK2 L     RF,ACOMFACS                                                      
         ICM   RF,15,CLOCKET-COMFACSD(RF)                                       
         BNZ   TSTLOCK4                                                         
* NEED TO GET LOCKET ADDRESS                                                    
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CCALLOV-COMFACSD(RF)                                       
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,ACOMFACS                                                      
         MVC   CLOCKET-COMFACSD(4,RF),DMCB   SET A(LOCKET) IN COMFACS           
         ICM   RF,15,CLOCKET-COMFACSD(RF)                                       
*                                                                               
TSTLOCK4 GOTO1 (RF),DMCB,('LKTESTQ',BLK32),ACOMFACS                             
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOCK2                                                         
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
       ++INCLUDE FALOCKETD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKSTA  DS    XL5                                                              
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOF8D                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
MYIO     DS    A                   A(EXTRA IOAREA FOR DEMOS)                    
MYIOL    EQU   2000                IOAREA FOR READING DEMOS                     
TRNSTART DS    XL2                 TRANSFER START DATE (COMPRESSED)             
LASTSPLT DS    XL2                 LAST DATE IN SPLIT RECORDS                   
*                                                                               
PROGKEY  DS    XL48                BACKUP FOR PROGRAM KEY                       
*                                                                               
DESCELEM DS    XL256               AREA TO BUILD BUY DESCRIPTION ELEM           
BUYSTART DS    XL2                 MONDAY OF START WEEK                         
STARTDAY DS    X                   START DAY OF WEEK                            
BUYEND   DS    XL2                 MONDAY OF END WEEK                           
ENDDAY   DS    X                   END DAY OF WEEK                              
NUMWEEKS DS    X                   NUMBER OF WEEKS FOR THIS BUYLINE             
PROGNAME DS    CL18                PROGRAM NAME                                 
MODESPW  DS    X                   MODE SPOTS/WEEK                              
*                                                                               
DEMOELEM DS    XL256               AREA TO BUILD DEMO ELEMENT                   
THISREF  DS    X                   REFERENCE NUMBER OF CURRENT PROGRAM          
*                                                                               
ESTSTART DS    A                   A(WEEKLY ELEMENT FOR BEGIN OF EST)           
ESTEND   DS    A                   A(WEEKLY ELEMENT FOR END OF EST)             
*                                                                               
NORMCOST DS    F                   NORMAL COST FOR BUY ELEMENTS                 
COSTFLAG DS    C                   'T' = TRADE , 'C' = CASH                     
MAXSPOTS EQU   115                                                              
MODETAB  DS    XL(MAXSPOTS+1)      TABLE TO FIGURE OUT MODE SPOTS/WEEK          
MODENUM  DS    X                   CURRENT MODE                                 
PTAPRD   DS    C                   ALLOCATION PRD CODE FOR CLT PTA              
*                                                                               
SVMKTSTA DS    0XL5                MKT/STA REQESTED FOR OVERNITE XFER           
SVMKT    DS    XL2                                                              
SVSTA    DS    XL3                                                              
FAKEFLD  DS    XL16                FAKE FIELD FOR VALISTA CALL                  
*                                                                               
TRACEOPT DS    C                   TRACE KEYS AND RECS TO REPORT (Y/N)          
BLK32    DS    XL32                BLOCK FOR TRACE ROUTINE                      
SKIPWRT  DS    C                   SKIP UPDATES TO FILE (Y/N)                   
*                                                                               
AFSTWK   DS    A                   A(FIRST WEEKLY ELEMENT)                      
BLINTAB  DS    XL106               NUMBER OF BLINES PER WEEK/PROGREC            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPCSO08   11/07/03'                                      
         END                                                                    
