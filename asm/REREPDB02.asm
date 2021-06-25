*          DATA SET REREPDB02  AT LEVEL 015 AS OF 05/01/02                      
*PHASE REDB02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE REGENBUC                                                               
*INCLUDE SORTER                                                                 
         TITLE 'REREPDB02 (REDB02) - REP BUYS-TO-CON DAILY BALANCE'             
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                    *          
*  REREPDB02 (REDB02) --- REPPAK DAILY (RECOVERY FILE) CHECK FOR     *          
*                          BUYS TO CONTRACTS                         *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
* WHILE NOT EOF ON RECOVERY FILE                                     *          
*     -TEST FOR A BUYS AND CONTRACTS                                 *          
*     -FORMAT AND RELEASE A SORT RECORD                              *          
* END WHILE                                                          *          
*                                                                    *          
* SORT                                                               *          
*                                                                    *          
* WHILE NOT EOF ON SORT FILE                                         *          
*   -IF A BUY RECORD                                                 *          
*     -BUCKET BEFORE IMAGE FOR THAT BUY                              *          
*     -LOOP TILL BREAK ON THAT BUY                                   *          
*     -BUCKET FINAL AFTER IMAGE FOR THAT BUY                         *          
*     -CALCULATE AND BUCKET DIFFERANCE                               *          
*   -IF A CONTRACT RECORD                                            *          
*     -BUCKET BEFORE IMAGE FOR THAT CONTRACT                         *          
*     -LOOP TILL BREAK ON THAT BUY                                   *          
*     -BUCKET FINAL AFTER IMAGE FOR THAT CONTRACT                    *          
*     -CALCULATE AND COMPARE TO BUY DIFFERANCE                       *          
*     -IF GRIDS DO NOT AGREE                                         *          
*        -PRODUCE AN ERROR LINE                                      *          
*     -ELSE                                                          *          
*        -BUCKET SYSTEM CHANGE BY REP                                *          
* END WHILE                                                          *          
*                                                                    *          
* PRODUCE CONTROL REPORT                                             *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
* ------------------------------------------------------------------ *          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  JAN06/92 (MRR) --- >INITIAL DEVELOPMENT                           *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
REDB02   CSECT                                                                  
         NMOD1 0,REDB02,R9,R8,RR=R2                                             
         ST    R2,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAINLINE                                                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
*        MAIN LINE                                                              
*                                                                               
MAINLINE EQU   *                                                                
         BAS   RE,INIT                                                          
         BNZ   MAINERR                                                          
         BAS   RE,READRCV                                                       
         BNZ   MAINERR                                                          
         CLC   SORTRECS(4),=F'1'                                                
         BE    MAIN100                                                          
MAIN10   EQU   *                                                                
         BAS   RE,PROCESS                                                       
MAIN100  EQU   *                                                                
         GOTO1 ASORT,DMCB,=C'END'                                               
         BAS   RE,DOREPORT                                                      
         BNZ   MAINERR                                                          
*                                                                               
*        END MAINLINE / FINISH RUN                                              
*                                                                               
MAINGOOD EQU   *                                                                
         MVC   P(21),=C'*** END OF REPORT ***'                                  
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
MAINERR  EQU   *                                                                
         MVC   P(40),=C'* * * ERROR ENCOUNTERED DURING RUN * * *'               
         GOTO1 REPORT                                                           
         MVC   P(60),RUNERROR                                                   
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(26),=C'* * * RUN TERMINATED * * *'                             
         GOTO1 REPORT                                                           
MAINEXIT EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        INIT --- SET INITIAL VALUES FOR THIS RUN                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         LA    RE,REPTAB                                                        
         LA    RF,REPTABL                                                       
         XCEFL                                                                  
*                                                                               
         XC    RBLOCK,RBLOCK                                                    
         MVC   RBLOCK+00(4),GETBROAD                                            
         MVC   RBLOCK+04(4),GETDAY                                              
         MVC   RBLOCK+08(4),ADDAY                                               
         MVC   RBLOCK+12(4),DATCON                                              
*                                                                               
         LA    RE,IOAREA1                                                       
         ST    RE,AIOAREA                                                       
*                                                                               
         MVI   DODISK,0                                                         
         CLI   QOPTION1,C'D'                                                    
         BNE   INIT20                                                           
         MVI   DODISK,C'D'                                                      
INIT20   EQU   *                                                                
*                                                                               
         CLI   DODISK,C'D'                                                      
         BE    INIT30                                                           
         OPEN  (RCVTAPE,(INPUT))                                                
         LTR   RF,RF                                                            
         BZ    INIT40                                                           
         DC    H'0'                                                             
INIT30   EQU   *                                                                
         OPEN  (RCVIN,(INPUT))                                                  
         LTR   RF,RF                                                            
         BZ    INIT40                                                           
         DC    H'0'                                                             
INIT40   EQU   *                                                                
*                                                                               
         L     RF,=V(SORTER)                                                    
         A     RF,RELO                                                          
         ST    RF,ASORT                                                         
*                                                                               
         LA    R4,SORTDATA-SORTKEY                                              
         LA    R5,SORTCARD+15                                                   
         EDIT  (R4),(3,(R5)),ZERO=NOBLANK,FILL=0   SORT KEY LENGTH              
         LA    R4,SORTRECX-SORTREC                                              
         LA    R5,RECCARD+21                                                    
         EDIT  (R4),(4,(R5)),ZERO=NOBLANK,FILL=0   RECORD LENGTH                
         GOTO1 ASORT,DMCB,SORTCARD,RECCARD         INIT THE SORT                
*                                                                               
         LA    RE,SORTREC                                                       
         LA    RF,SORTRECX-SORTREC                                              
         XCEFL                                                                  
*                                                                               
         MVI   SORTKEY,X'FF'                   SEND A 'EOF' REC OUT             
         MVC   SORTKEY+1(49),SORTKEY                                            
         GOTO1 ASORT,DMCB,=C'PUT',SORTREC                                       
         L     RF,SORTRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,SORTRECS                                                      
*                                                                               
         LA    RE,SORTREC                                                       
         LA    RF,SORTRECX-SORTREC                                              
         XCEFL                                                                  
*                                                                               
*        INIT EXIT                                                              
*                                                                               
INITGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        READRCV --- READ AND PROCESS THE REP RECOVERY FILE                     
*                                                                               
READRCV  NTR1                                                                   
*                                                                               
RRCV10   EQU   *                                                                
         BAS   RE,GETRCV                                                        
         BNZ   RRCVGOOD                 NON-ZERO IS END-OF-FILE                 
*                                                                               
         LA    RE,SORTREC                                                       
         LA    RF,SORTRECX-SORTREC                                              
         XCEFL                                                                  
*                                                                               
         LA    R6,RCVDATA                                                       
         CLI   RCVDATA,X'0B'            BUY?                                    
         BNE   RRCV50                                                           
         USING RBUYRECD,R6                                                      
         MVC   SORTKREP(2),RBUYKREP                                             
         ZAP   WORK(5),=P'99999999'                                             
         PACK  WORK+10(1),RBUYKCON+3                                            
         PACK  WORK+11(1),RBUYKCON+2                                            
         PACK  WORK+12(1),RBUYKCON+1                                            
         PACK  WORK+13(1),RBUYKCON+0                                            
         MVI   WORK+14,X'0C'                                                    
         SRP   WORK+10(5),64-1,0                                                
         SP    WORK(5),WORK+10(5)                                               
         SRP   WORK(5),1,0                                                      
         MVC   SORTKCON(4),WORK                                                 
         MVI   SORTKSOU,0                                                       
         MVC   SORTKBUY(5),RBUYKPLN                                             
         DROP  R6                                                               
         B     RRCV60                                                           
RRCV50   EQU   *                                                                
         USING RCONREC,R6                                                       
         MVC   SORTKREP(2),RCONKREP                                             
         MVC   SORTKCON(4),RCONKCON                                             
         MVI   SORTKSOU,2                                                       
         XC    SORTKBUY(5),SORTKBUY                                             
         DROP  R6                                                               
RRCV60   EQU   *                                                                
         MVC   SORTKDAT(3),RDATE                                                
         MVC   SORTKTIM(4),RTIME                                                
         MVC   SORTKTYP(1),RRECTY                                               
         MOVE  (SORTDATA,1000),RCVDATA                                          
*                                                                               
         GOTO1 ASORT,DMCB,=C'PUT',SORTREC                                       
         L     RF,SORTRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,SORTRECS                                                      
*                                                                               
         B     RRCV10                  GET NEXT RECOVERY RECORD                 
*                                                                               
RRCVGOOD EQU   *                                                                
         B     TESTGOOD                                                         
RRCVBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        PROCESS --- READ BACK THE SORTED FILE AND CHECK DOLLARS                
*                                                                               
PROCESS  NTR1                                                                   
*                                                                               
CESS10   EQU   *                                                                
         GOTO1 ASORT,DMCB,=C'GET'                                               
         XC    BBUCKETS,BBUCKETS                                                
         GOTO1 =V(REGENBUC),DMCB,RCVDATA,BBUCKETS,RBLOCK                        
         LA    R7,BBUCKETS+2                                                    
         SR    R5,R5                      R5 WILL HOLD DOLLARS                  
         SR    R6,R6                      R6 WILL HOLD SPOTS                    
CESS50   EQU   *                                                                
         CLI   0(R7),0                                                          
         BE    CESS60                                                           
         A     R5,6(R7)                   ADD DOLLARS                           
         A     R6,10(R7)                  ADD SPOTS                             
         LA    R7,14(R7)                  NEXT BUCKET                           
         B     CESS50                     ...LOOP                               
CESS60   EQU   *                                                                
         L     R3,CURAGY                                                        
         USING AGYTABD,R3                                                       
         LR    R4,R5                                                            
         SR    R5,R5                                                            
         SRDA  R4,31                                                            
         D     R4,=F'100'                                                       
         LTR   R5,R5                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         A     R5,ATABDOLS                                                      
         ST    R5,ATABDOLS                                                      
         A     R6,ATABSPOT                                                      
         ST    R6,ATABSPOT                                                      
         L     R7,ATABRECS                                                      
         A     R7,=F'1'                                                         
         ST    R7,ATABRECS                                                      
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*        GETRCV --- RETURN A BUY RECORD FROM THE RECOVERY FILE IN THE           
*                    RECOVERY RECORD AREA                                       
*                                                                               
GETRCV   NTR1                                                                   
*                                                                               
         LA    RE,RCVREC                  CLEAR THE RCVREC AREA                 
         LA    RF,RCVDATAX-RCVREC                                               
         XCEFL                                                                  
*                                                                               
         CLI   DODISK,C'D'                                                      
         BE    GRCV1                                                            
         BAS   RE,GETTAPE                                                       
         BZ    GRCVGOOD                                                         
         B     GRCVBAD                                                          
*                                                                               
GRCV1    EQU   *                                                                
         OC    ARCVREC,ARCVREC            THIS IS ZERO ON FIRST CALL            
         BNZ   GRCV20                                                           
*                                                                               
GRCV10   EQU   *                                                                
         LA    RE,RCVBLK                                                        
         LR    RF,RE                                                            
         LA    RF,6(RF)                                                         
         ST    RF,ARCVREC                                                       
         L     RF,=A(RCVBLKX-RCVBLK)                                            
         XCEFL                                                                  
         GET   RCVIN,RCVBLK                                                     
         L     RF,RCVBLKS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVBLKS                                                       
         L     R2,ARCVREC                                                       
         B     GRCV30                                                           
*                                                                               
GRCV20   EQU   *                                                                
         L     R2,ARCVREC                                                       
         ZICM  R3,0(R2),2                 GET RECORD LENGTH                     
         AR    R2,R3                      POINT TO THE NEW RECORD               
         OC    0(2,R2),0(R2)              ANYTHING THERE?                       
         BZ    GRCV10                     NOPE, GET NEXT BLOCK                  
         ST    R2,ARCVREC                                                       
*                                                                               
GRCV30   EQU   *                                                                
         L     RF,RCVRECS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVRECS                                                       
         CLI   RFILTY-RECVHDR+2(R2),X'81'    REP DIRECTORY?                     
         BNE   GRCV40                                                           
         BAS   RE,MOVERCV                    IF SO, CHECK FOR DELETED           
         BAS   RE,CHKDIR                      MASTER RECORDS                    
         B     GRCV20                        LOOP FOR ANOTHER RECORD            
GRCV40   EQU   *                                                                
         CLI   RFILTY-RECVHDR+2(R2),X'82'    REP FILE?                          
         BNE   GRCV20                         NOT REP, GET NEXT                 
         L     RF,RCVFRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,RCVFRECS                                                      
         CLI   RCVDATA-RCVREC+2(R2),X'0B'    BUY RECORDS?                       
         BNE   GRCV50                                                           
         L     RF,RCVBUYS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVBUYS                                                       
         B     GRCV100                                                          
GRCV50   EQU   *                                                                
         CLI   RCVDATA-RCVREC+2(R2),X'0C'    CONTRACTS?                         
         BNE   GRCV20                                                           
         L     RF,RCVCONS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVCONS                                                       
*                                                                               
GRCV100  EQU   *                                                                
         BAS   RE,MOVERCV                                                       
*                                                                               
GRCVGOOD EQU   *                                                                
         B     TESTGOOD                                                         
GRCVBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 3                                                                
RRCVEOD  EQU   *                                                                
         B     GRCVBAD                                                          
         EJECT                                                                  
*                                                                               
*        MOVE THE RECOVERY RECORD FROM WHERE IT IS TO WHERE WE CAN              
*         USE IT                                                                
*                                                                               
MOVERCV  NTR1                                                                   
*                                                                               
         ZICM  R1,0(R2),2                                                       
         LA    RE,2(R2)                                                         
         LA    RF,RCVREC                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        GETTAPE --- GET A TAPE RECOVERY RECORD                                 
*                                                                               
GETTAPE  NTR1                                                                   
*                                                                               
GTAP10   EQU   *                                                                
         GET   RCVTAPE,RCVRECT                                                  
         L     RF,RCVBLKS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVBLKS                                                       
         L     RF,RCVRECS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVRECS                                                       
*                                                                               
         CLI   RFILTY,X'81'                  REP DIRECTORY?                     
         BNE   GTAP50                                                           
         BAS   RE,CHKDIR                                                        
         B     GTAP10                                                           
GTAP50   EQU   *                                                                
         CLI   RFILTY,X'82'                  REP FILE?                          
         BNE   GTAP10                                                           
         L     RF,RCVFRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,RCVFRECS                                                      
         CLI   RCVDATA,X'0B'                 BUY RECORDS?                       
         BNE   GTAP60                                                           
         L     RF,RCVBUYS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVBUYS                                                       
         B     GTAPGOOD                                                         
GTAP60   EQU   *                                                                
         CLI   RCVDATA,X'0C'                 CONTRACTS                          
         BNE   GTAP10                                                           
         L     RF,RCVCONS                                                       
         A     RF,=F'1'                                                         
         ST    RF,RCVCONS                                                       
*                                                                               
*        GETTAPE EXIT                                                           
*                                                                               
GTAPGOOD EQU   *                                                                
         B     TESTGOOD                                                         
GTAPBAD  EQU   *                                                                
         B     TESTBAD                                                          
         SPACE 3                                                                
RTAPEEOD EQU   *                                                                
         B     GTAPBAD                                                          
         EJECT                                                                  
*                                                                               
*        CHKDIR --- CHECK FOR DELETED MASTER FILE RECORDS.  WE SHOULD           
*                    NEVER FIND ANY.                                            
*                                                                               
CHKDIR   NTR1                                                                   
*                                                                               
         L     RF,RCVDRECS                                                      
         A     RF,=F'1'                                                         
         ST    RF,RCVDRECS                                                      
*                                                                               
         LA    R2,CDIRLIST                                                      
CDIR10   EQU   *                                                                
         CLI   0(R2),0                                                          
         BE    CDIRGOOD                                                         
         CLC   RCVDATA(1),0(R2)                                                 
         BE    CDIR50                                                           
         LA    R2,13(R2)                                                        
         B     CDIR10                                                           
CDIR50   EQU   *                                                                
         TM    RCVDATA+27,X'80'                DELETED                          
         BZ    CDIRGOOD                                                         
         MVC   P(L'MASTDEL),MASTDEL                                             
         GOTO1 REPORT                                                           
         MVC   P(12),1(R2)                                                      
         MVC   P+14(L'MASTDEL2),MASTDEL2                                        
         GOTO1 REPORT                                                           
         MVC   P(34),RCVDATA                                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
*        CHKDIR EXIT                                                            
*                                                                               
CDIRGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         SPACE 3                                                                
CDIRLIST EQU   *                                                                
         DC    X'01',CL12'REP'                                                  
         DC    X'02',CL12'STATION'                                              
         DC    X'03',CL12'REGION'                                               
         DC    X'04',CL12'OFFICE'                                               
         DC    X'05',CL12'TEAM'                                                 
         DC    X'06',CL12'SALESPERSON'                                          
         DC    X'07',CL12'GROUP'                                                
         DC    X'08',CL12'ADVERTISER'                                           
         DC    X'09',CL12'PRODUCT'                                              
         DC    X'0A',CL12'AGENCY'                                               
         DC    X'0D',CL12'CLASS'                                                
         DC    X'0F',CL12'CATAGORY'                                             
         DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*        CKREP --- GET THE REP RECORD AND THE CONTRACT NUMBER START             
*                                                                               
*        P1    =   A(ENTRY TO LOAD IN THE TABLE)                                
*                                                                               
CKREP    NTR1                                                                   
*                                                                               
         L     R6,0(R1)                                                         
*                                                                               
         LA    R5,RCVDATA                                                       
         USING RBUYREC,R5                                                       
*                                                                               
         XC    REPKEY,REPKEY                                                    
         MVI   REPKEY,X'01'                                                     
         MVC   REPKEY+25(2),RBUYKREP                                            
         BAS   RE,REPHIGH                                                       
         CLC   REPKEY(27),REPKSAVE                                              
         BNE   CREPBAD                                                          
         BAS   RE,REPGET                                                        
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFILE '),(X'05',AIOAREA),     X        
               RR=RELO                                                          
         CLI   P4,0                                                             
         BNE   CREPBAD                                                          
         L     R4,P4                                                            
         USING RREPSPOT,R4                                                      
         MVC   0(2,R6),RBUYKREP                                                 
         MVC   2(4,R6),RREPSPC#                                                 
         DROP  R4,R5                                                            
*                                                                               
*        CKCON EXIT                                                             
*                                                                               
CREPGOOD EQU   *                                                                
         B     TESTGOOD                                                         
CREPBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        DOREPORT --- PRODUCE EXTRACT CONTROL REPORT                            
*                                                                               
DOREPORT NTR1                                                                   
*                                                                               
         EDIT  (4,RCVBLKS),(10,P),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+11(25),=C'RECOVERY BLOCKS WERE READ'                           
         GOTO1 REPORT                                                           
         EDIT  (4,RCVRECS),(10,P),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+11(26),=C'RECOVERY RECORDS WERE READ'                          
         GOTO1 REPORT                                                           
         EDIT  (4,RCVFRECS),(10,P),COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+11(34),=C'REPFILE RECOVERY RECORDS WERE READ'                  
         GOTO1 REPORT                                                           
         EDIT  (4,RCVBUYS),(10,P),COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+11(21),=C'BUY RECORDS WERE READ'                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         EDIT  (4,SORTRECS),(10,P),COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+11(29),=C'RECORDS WERE SENT TO THE SORT'                       
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,AGYOK                                                         
         USING AGYTABD,R2                                                       
         SR    R4,R4                COUNT UP SORT RECS                          
         SR    R5,R5                COUNT UP SPOTS                              
         SR    R6,R6                COUNT UP DOLLARS                            
DREP10   EQU   *                                                                
         ZICM  RF,0(R2),2                                                       
         BZ    DREP100                                                          
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         BAS   RE,SETMSG                                                        
         MVC   PAGYCODE(2),ATABCODE                                             
         GOTO1 GETMYAGY,DMCB,ATABCODE,PAGYNAME                                  
         EDIT  (4,ATABRECS),(6,PNUMRECS),COMMAS=YES,ZERO=NOBLANK                
         EDIT  (4,ATABSPOT),(6,PSPOTS),COMMAS=YES,ZERO=NOBLANK                  
         EDIT  (4,ATABDOLS),(9,PDOLS),COMMAS=YES,ZERO=NOBLANK                   
         A     R4,ATABRECS                                                      
         A     R5,ATABSPOT                                                      
         A     R6,ATABDOLS                                                      
         DROP  R2,R3                                                            
         GOTO1 REPORT                                                           
         LA    R2,ATABEND-ATABCODE(R2)                                          
         B     DREP10                                                           
*                                                                               
DREP100  EQU   *                                                                
         GOTO1 REPORT                                                           
         LA    R3,P                                                             
         USING PLINED,R3                                                        
         BAS   RE,SETMSG                                                        
         MVC   PAGYCODE(36),=CL36'* * * RUN TOTALS * * *'                       
         EDIT  (R4),(6,PNUMRECS),COMMAS=YES,ZERO=NOBLANK                        
         EDIT  (R5),(6,PSPOTS),COMMAS=YES,ZERO=NOBLANK                          
         EDIT  (R6),(9,PDOLS),COMMAS=YES,ZERO=NOBLANK                           
         DROP  R3                                                               
         GOTO1 REPORT                                                           
*                                                                               
*        DOREPORT EXIT                                                          
*                                                                               
DREPGOOD EQU   *                                                                
         B     TESTGOOD                                                         
DREPBAD  EQU   *                                                                
         B     TESTBAD                                                          
         EJECT                                                                  
*                                                                               
*        SETMSG --- PUT TEXTS INTO THE PRINT LINE                               
*                                                                               
SETMSG   NTR1                                                                   
*                                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         MVC   PMSG1(L'PMSG1),=C'AGENCY'                                        
         MVI   PMSG2,C'-'                                                       
         MVC   PMSG3(L'PMSG3),=C'HAD'                                           
         MVC   PMSG4(L'PMSG4),=C'SORT RECORDS, THAT REPRESENTS'                 
         MVC   PMSG5(L'PMSG5),=C'SPOTS AND'                                     
         MVC   PMSG6(L'PMSG6),=C'DOLLARS'                                       
         DROP  R2                                                               
*                                                                               
SMSGGOOD EQU   *                                                                
         B     TESTGOOD                                                         
         EJECT                                                                  
*                                                                               
*        REPPAK COMMUNICATION WITH DATA MANAGER (DIRECTORY)                     
*                                                                               
         SPACE 3                                                                
REPREAD  MVC   COMMAND,DMREAD                                                   
         MVC   FILE,REPDIR                                                      
         B     REPDIRL                                                          
         SPACE 2                                                                
REPSEQ   MVC   COMMAND,DMRSEQ                                                   
         MVC   FILE,REPDIR                                                      
         B     REPDIRL                                                          
         SPACE 2                                                                
REPHIGH  MVC   COMMAND,DMRDHI                                                   
         MVC   FILE,REPDIR                                                      
         MVC   REPKSAVE,REPKEY                                                  
         B     REPDIRL                                                          
         SPACE 2                                                                
REPDIRL  NTR1                                                                   
         L     R3,UTL                                                           
         MVC   4(1,R3),REPSYS                                                   
         IC    R4,DMINBTS                                                       
         MVC   REPKSAVE,REPKEY                                                  
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,REPKSAVE,REPKEY                 
         MVC   4(1,R3),SPOTSYS                                                  
         B     DMCHECK                                                          
         SPACE 3                                                                
*                                                                               
*        REPPAK COMMUNICATION WITH DATA MANAGER (FILE)                          
*                                                                               
         SPACE 3                                                                
REPGET   MVC   COMMAND,GETREC                                                   
         MVC   FILE,REPFIL                                                      
         L     R3,UTL                                                           
         MVC   4(1,R3),REPSYS                                                   
         LA    R2,REPKEY+28                                                     
REPFILL  NTR1                                                                   
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,(R2),AIOAREA,DMWORK,0           
         MVC   4(1,R3),SPOTSYS                                                  
         SPACE 3                                                                
DMCHECK  EQU   *                                                                
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         MVI   DMOUTBTS,X'FF'                                                   
         BNZ   DMERRS                                                           
         B     TESTGOOD                                                         
DMERRS   EQU   *                                                                
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*        COMMON CODE                                                            
*                                                                               
TESTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
TESTBAD  EQU   *                                                                
         LA    R0,1                                                             
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
*        ERROR TEXTS                                                            
*                                                                               
MASTDEL  DC    C'*** A MASTER RECORD WILL BE DELETED ***'                       
MASTDEL2 DC    C'RECORD POINTER HAS BEEN SET FOR DELETE'                        
         EJECT                                                                  
*                                                                               
*        DCB'S                                                                  
*                                                                               
RCVIN    DCB   DDNAME=RCVIN,DSORG=PS,RECFM=U,                          X        
               BLKSIZE=9500,MACRF=GM,EODAD=RRCVEOD                              
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=V,LRECL=2024,             X        
               BLKSIZE=2028,MACRF=GM,EODAD=RTAPEEOD                             
         EJECT                                                                  
*                                                                               
*        LITERAL POOL                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SORT RECORDS, ET AL                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
SORTREC  DS    0CL1020                                                          
SORTKEY  DS    0CL20                                                            
SORTKREP DS    CL2   +00            REP CODE                                    
SORTKCON DS    CL4   +02            CONTRACT NUMBER                             
SORTKSOU DS    CL1   +06            DATA SOURCE:0-BUYS,1-INVS,2-CON             
SORTKBUY DS    CL5   +07            CONTRACT BUYLINE NUMBER                     
SORTKDAT DS    CL3   +12            TRANSACTION DATE                            
SORTKTIM DS    CL4   +15            TRANSACTION TIME                            
SORTKTYP DS    CL1   +19            RECOVERY TYPE:1-COPY,2-AFTER,3-ADD          
*                    +20                                                        
SORTDATA DS    CL1000                                                           
SORTRECX EQU   *                                                                
         DS    0D                                                               
         DC    CL8'*OLDSORT'                                                    
OLDSORT  DS    1020C                                                            
         DS    0D                                                               
         DC    CL8'SORTCARD'                                                    
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=CH,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=0000'                                  
*                                                                               
*        REP TABLES                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*REPTAB*'                                                    
REPTAB   DS    100CL6                                                           
*              CL2                REP CODE                                      
*              CL4                CHANGED DOLLARS                               
REPTABX  EQU   *                                                                
REPTABL  EQU   REPTABX-REPTAB                                                   
*                                                                               
*        IO AREAS                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*IOAREA1'                                                    
IOAREA1  DS    1008C                                                            
IOAREA1X EQU   *                                                                
IOAREA1L EQU   IOAREA1X-IOAREA1                                                 
         DS    0D                                                               
         DC    CL8'*IOAREA2'                                                    
IOAREA2  DS    1008C                                                            
IOAREA2X EQU   *                                                                
IOAREA2L EQU   IOAREA2X-IOAREA2                                                 
*                                                                               
*        LOCAL VARIABLES                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*LOCAL**'                                                    
CURAGY   DS    A            A(CURRENT AGY IN AGYTAB)                            
SORTRECS DS    F            NUMBER OF RECORDS SENT TO THE SORT                  
RCVBLKS  DS    F            NUMBER OF RCV BLOCKS READ                           
RCVRECS  DS    F            NUMBER OF RCV RECORDS READ                          
RCVFRECS DS    F            NUMBER OF RCV RECORDS READ FROM REPFILE             
RCVDRECS DS    F            NUMBER OF RCV RECORDS READ FROM REPDIR              
RCVBUYS  DS    F            NUMBER OF RCV BUY RECORDS READ                      
RCVCONS  DS    F            NUMBER OF RCV CONTRACTS READ                        
ARCVREC  DS    F            A(CURRENT RCV RECORD IN BLOCK)                      
ASORT    DS    F            A(SORT)                                             
BBUCKETS DS    CL200        REP BUY RECORD BUCKET OUTPUT                        
RBLOCK   DS    CL12         REGENBUC ADDR BLOCK                                 
RUNERROR DS    CL60                                                             
DODISK   DS    X            IF NOT ZERO, TAKE INPUT FROM 9500 DISK BLK          
ELCODE   DS    X                                                                
SPOTSYS  DS    X            SPOT SYS NUMBER (TO AND FROM UTL)                   
REPSYS   DS    X            REP SYS NUMBER                                      
REPDIR   DC    CL8'REPDIR  '                                                    
REPFIL   DC    CL8'REPFIL  '                                                    
REPKEY   DS    CL32                                                             
REPKSAVE DS    CL32                                                             
REPFLIST DC    CL8' REPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
*                                                                               
*        RECOVERY FILE RECORD AREA                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'*RCVREC*'                                                    
RCVRECT  DS    CL4          TAPE INPUT GOES HERE                                
RCVREC   EQU   *                                                                
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RCVDATA  EQU   *                                                                
         DS    1000C        WILL HOLD ONLY A REP SYSTEM RECORD                  
RCVDATAX EQU   *                                                                
*                                                                               
*        RECOVERY FILE BLOCK AREA                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*RCVBLK*'                                                    
RCVBLK   EQU   *                                                                
         DS    9500C                                                            
RCVBLKX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
* DSECT FOR PRINT LINE                                                          
*                                                                               
PLINED   DSECT                                                                  
PMSG1    DS    CL6         'AGENCY'                                             
         DS    CL1                                                              
PAGYCODE DS    CL2                                                              
PMSG2    DS    CL1         '-'                                                  
PAGYNAME DS    CL33                                                             
         DS    CL1                                                              
PMSG3    DS    CL3         'HAD'                                                
         DS    CL1                                                              
PNUMRECS DS    CL6                                                              
         DS    CL1                                                              
PMSG4    DS    CL29        'SORT RECORDS, THAT REPRESENTS'                      
         DS    CL1                                                              
PSPOTS   DS    CL6                                                              
         DS    CL1                                                              
PMSG5    DS    CL9         'SPOTS AND'                                          
         DS    CL1                                                              
PDOLS    DS    CL9                                                              
         DS    CL1                                                              
PMSG6    DS    CL7         'DOLLARS'                                            
*                                                                               
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
RBUYRECD DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
RREPRECD DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015REREPDB02 05/01/02'                                      
         END                                                                    
