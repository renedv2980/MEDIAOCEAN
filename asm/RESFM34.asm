*          DATA SET RESFM34    AT LEVEL 158 AS OF 07/05/00                      
*PHASE T81834A,*                                                                
         TITLE 'T81834 - RESFM34 - CONTRACT MOVE HISTORY'                       
*                                                                               
***********************************************************************         
*                                                                     *         
*        RESFM34 --- REP SFM CONTRACT MOVE HISTORY                    *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* APR17/97 RHV  THE SPAWNING                                          *         
*                                                                     *         
* JUL05/00 BU   REMOVE REFERENCES TO GLV1GOTO PER MEL H.              *         
*                                                                     *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
*                                                                               
         PRINT NOGEN                                                            
T81834   CSECT                                                                  
         NMOD1 0,**1834**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HHOOK                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
***********************************************************************         
* MAIN PROGRAM                                                                  
***********************************************************************         
         OI    GENSTAT4,NODELLST   INITIALIZE GENCON                            
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    REPORT                                                           
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DREC - DISPLAY RECORD - CALLS GLOBBER TO SWAP OUT TO CONTRACT                 
***********************************************************************         
DREC     DS    0H                                                               
         XC    WORK,WORK           BUILD GLOBBER CONTROL ELEM                   
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'SFM'    SFM PROGRAM                                  
         MVC   GLVXTOSY,=C'REP'    TO THE CALLEE'S SYSTEM                       
         MVC   GLVXTOPR,=C'CON'    TO THE CALLEE'S PROGRAM                      
***      OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',WORK,24,GLVXCTL                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO              BUILD DISPLAY K ELEM FOR CONTRACT            
         USING RCONREC,R4                                                       
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',RCONKCON,4,GLRDISPK                           
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
***********************************************************************         
* VKEY - VALIDATE KEY ROUTINE                                                   
***********************************************************************         
VKEY     DS    0H                  VKEY MAIN ROUTINE                            
         BAS   RE,CHECKSTA                                                      
         BAS   RE,CHECKREP                                                      
         BAS   RE,CHECKDAT                                                      
         B     XIT                                                              
*                                                                               
* CHECKSTA - VALIDATES STATION KEY FIELD                                        
*                                                                               
CHECKSTA NTR1                                                                   
         XC    STATION,STATION     DEFAULT - NO STATION FILTER                  
         LA    R2,MOVSTAH          SCREEN FIELD                                 
         CLI   5(R2),0             STATION PROVIDED?                            
         BE    XIT                 NO - DONE HERE                               
*                                                                               
         MVC   RERROR(2),=AL2(INVSTA)                                           
*                                                                               
         OC    MOVSTA,SPACES       PARSE OUT CALL LETTERS & BAND                
         MVC   STATION,SPACES                                                   
         GOTO1 SCANNER,DMCB,(R2),(1,WORK),C',=,-'                               
         ZIC   R4,WORK                  L'FIRST INPUT FIELD                     
         CH    R4,=H'3'                 AT LEAST 3 CALL LETTERS?                
         BL    ERREND                                                           
         CH    R4,=H'4'                 MORE THAN 4 CALL LETTERS?               
         BH    ERREND                                                           
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   STATION(0),WORK+12       SAVE STATION CALL LETTERS               
         CLI   WORK+1,0                 ZERO LENGTH BAND?                       
         BE    CKSTA10                                                          
         CLI   WORK+1,1                 ONLY 1 CHAR BAND ALLOWED                
         BNE   ERREND                                                           
         CLI   WORK+22,C'F'                                                     
         BE    CKSTA10                                                          
         CLI   WORK+22,C'A'                                                     
         BE    CKSTA10                                                          
         CLI   WORK+22,C'C'                                                     
         BE    CKSTA10                                                          
         CLI   WORK+22,C'L'                                                     
         BE    CKSTA10                                                          
         CLI   WORK+22,C'T'                                                     
         BNE   ERREND                                                           
         MVI   WORK+22,C' '                                                     
CKSTA10  MVC   STATION+4(1),WORK+22                                             
*                                                                               
         LA    R4,KEY              READ STATION REC                             
         USING RSTAREC,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,STATION                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    XIT                                                              
         B     ERREND                                                           
         DROP  R4                                                               
*                                                                               
* CHECKREP - VALIDATES REP KEY FIELD                                            
*                                                                               
CHECKREP NTR1                                                                   
         XC    OLDREP,OLDREP       DEFAULT - NO REP FILTER                      
         LA    R2,MOVREPH                                                       
         CLI   5(R2),0             REP PROVIDED?                                
         BE    XIT                 NO - DONE HERE                               
*                                                                               
         XC    KEY,KEY             READ REPREC                                  
         LA    R4,KEY                                                           
         USING RREPREC,R4                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    CKREP10                                                          
         MVC   RERROR(2),=AL2(102)                                              
         B     ERREND                                                           
CKREP10  MVC   OLDREP,RREPKREP                                                  
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
* CHECKDAT - VALIDATES ACTIVITY DATE KEY FIELD                                  
*                                                                               
CHECKDAT NTR1                                                                   
         XC    SDATE(4),SDATE      DEFAULT - NO DATE FITLERS                    
         LA    R2,MOVDATH                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(0,WORK)                               
         TM    DMCB+4,X'FF'-X'04'  VALID?                                       
         BZ    CKDAT10                                                          
         MVC   RERROR(2),=AL2(32)                                               
         B     ERREND                                                           
CKDAT10  LA    R3,WORK                                                          
         USING PERVALD,R3                                                       
         MVC   SDATE,PVALCSTA      START DATE                                   
         MVC   EDATE,PVALCEND      END DATE                                     
         B     XIT                                                              
         DROP  R3                                                               
***********************************************************************         
* REPORT - GENERATES REPORT OF CONTRACT MOVE RECORDS                            
***********************************************************************         
REPORT   DS    0H                                                               
         XC    LASTSTA,LASTSTA                                                  
         XC    GRANDTOT,GRANDTOT                                                
         XC    SUBTOT,SUBTOT                                                    
         XC    COUNT,COUNT                                                      
         XC    SUBCOUNT,SUBCOUNT                                                
*                                                                               
         LA    R4,KEY              READ THROUGH 9E PASSIVE K KEYS               
         USING RCONREC,R4                                                       
         MVI   RCON9ETP,X'9E'                                                   
         MVC   RCON9ERP,AGENCY                                                  
         GOTO1 HIGH                                                             
         B     RPT20                                                            
RPT10    GOTO1 SEQ                                                              
*                                                                               
         LA    R4,KEY              MAKE SURE WE'RE POINTING TO KEY              
RPT20    CLC   KEY(14),KEYSAVE     MATCH REP & RECTYPE?                         
         BNE   RPT50               NO - DONE READING KEYS                       
*                                                                               
         OC    STATION,STATION     CHECK STATION FILTER                         
         BZ    RPT25                                                            
         CLC   RCON9EST,STATION                                                 
         BNE   RPT10                                                            
*                                                                               
RPT25    OC    OLDREP,OLDREP       CHECK OLD REP FILTER                         
         BZ    RPT27                                                            
         CLC   RCON9ESR,OLDREP                                                  
         BNE   RPT10                                                            
*                                                                               
RPT27    OC    SDATE,SDATE         CHECK ACTIVITY DATE FILTER                   
         BZ    RPT30                                                            
         CLC   RCON9EDT,SDATE                                                   
         BL    RPT10                                                            
         CLC   RCON9EDT,EDATE                                                   
         BH    RPT10                                                            
*                                                                               
RPT30    GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R5,P                                                             
         USING LISTD,R5                                                         
*                                                                               
         CLC   LASTSTA,RCONKSTA    UNIQUE STATION?                              
         BE    RPT32               NO                                           
         OC    LASTSTA,LASTSTA     FIRST RECORD                                 
         BZ    RPT32               YES                                          
*                                                                               
         BAS   RE,DOSUBTOT         DISPLAY SUBTOTAL                             
         XC    SUBTOT,SUBTOT       CLEAR SUBTOTAL                               
         XC    SUBCOUNT,SUBCOUNT                                                
*                                                                               
RPT32    DS    0H                                                               
         MVC   LASTSTA,RCONKSTA                                                 
*                                  DISPLAY K NUMBER                             
         GOTO1 HEXOUT,DMCB,RCONKCON,LISTK,L'RCONKCON                            
         LA    R2,LISTK                                                         
         BAS   RE,FORMATK                                                       
*                                                                               
         MVC   LISTSTA(4),RCONKSTA DISPLAY STATION                              
         CLI   RCONKSTA+4,C' '                                                  
         BE    RPT35                                                            
         MVI   LISTSTA+4,C'-'                                                   
         MVC   LISTSTA+5(1),RCONKSTA+4                                          
*                                                                               
RPT35    MVI   ELCODE,X'2A'        MOVE HISTORY ELEMENT                         
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE IF WE HAVE A 9E KEY            
         USING RCONMMEL,R6                                                      
*                                  DISPLAY ACTIVITY DATE                        
         GOTO1 DATCON,DMCB,(2,RCONMMDT),(11,LISTDATE)                           
*                                  DISPLAY ORIGINAL K NUMBER                    
         GOTO1 HEXOUT,DMCB,RCONMMOC,LISTOK,L'RCONMMOC                           
         LA    R2,LISTOK                                                        
         BAS   RE,FORMATK                                                       
*                                                                               
         MVC   LISTOR,RCONMMOR     DISPLAY ORIGINAL REP                         
         DROP  R6                                                               
*                                                                               
         BAS   RE,CONTOT           TOTAL BUCKETS FOR CURRENT CONTRACT           
         EDIT  FULL,(9,LISTTOT),FLOAT=$,COMMAS=YES,ZERO=NOBLANK                 
         L     RF,SUBTOT           INCREMENT SUBTOTAL                           
         A     RF,FULL                                                          
         ST    RF,SUBTOT                                                        
         L     RF,GRANDTOT         INCREMENT GRAND TOTAL                        
         A     RF,FULL                                                          
         ST    RF,GRANDTOT                                                      
         LH    RF,SUBCOUNT                                                      
         LA    RF,1(RF)                                                         
         STH   RF,SUBCOUNT                                                      
         LH    RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         STH   RF,COUNT                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     RPT10               BACK FOR NEXT MOVE REC                       
         DROP  R4,R5                                                            
*                                                                               
RPT50    DS    0H                                                               
         OC    STATION,STATION     ARE WE FILTERING ON STATION                  
         BNZ   *+8                 YES - DON'T BOTHER WITH SUBTOTAL             
         BAS   RE,DOSUBTOT                                                      
         MVI   ALLOWLIN,3                                                       
         MVC   P+53(11),=11C'='                                                 
         MVC   P3+53(11),=11C'='                                                
         MVC   P2+14(36),=C'GRAND TOTAL       CONTRACTS ------->'               
         EDIT  COUNT,(5,P2+26),ZERO=NOBLANK                                     
         EDIT  GRANDTOT,(11,P2+53),FLOAT=$,COMMAS=YES,ZERO=NOBLANK              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DOSUBTOT - PRINTS A SUBTOTAL BY STATION LINE ON REPORT                        
*                                                                               
DOSUBTOT NTR1                                                                   
         MVI   ALLOWLIN,3                                                       
         MVC   P+53(11),=11C'-'                                                 
         MVC   P3+53(11),=11C'-'                                                
         MVI   P4,0                                                             
*                                                                               
         LA    R2,P2+06            FORMAT TEXT ON LINE                          
         MVC  0(43,R2),=C'SUBTOTAL FOR STATION              CONTRACTS'          
         EDIT  SUBCOUNT,(5,P2+34),ZERO=NOBLANK                                  
         LA    R2,21(R2)                                                        
         MVC   0(4,R2),LASTSTA                                                  
         LA    R2,3(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   LASTSTA+4,C' '                                                   
         BE    *+8                                                              
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),LASTSTA+4                                                
*                                                                               
         EDIT  SUBTOT,(11,P2+53),FLOAT=$,COMMAS=YES,ZERO=NOBLANK                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
                                                                                
***********************************************************************         
* LIST - DISPLAYS LIST/SELECT SCREEN OF MOVE RECORDS                            
***********************************************************************         
LIST     DS    0H                                                               
         LA    R4,KEY              READ THROUGH 9E PASSIVE K KEYS               
         USING RCONREC,R4                                                       
         OC    KEY,KEY             ALREADY HAVE A KEY                           
         BZ    LST05               NO - GO MAKE ONE                             
         BAS   RE,KEY9E            YES - MAKE IT A 9E PASSIVE KEY               
         B     LST08                                                            
LST05    MVI   RCON9ETP,X'9E'                                                   
         MVC   RCON9ERP,AGENCY                                                  
LST08    GOTO1 HIGH                                                             
         B     LST20                                                            
LST10    GOTO1 SEQ                                                              
*                                                                               
         LA    R4,KEY              MAKE SURE WE'RE POINTING TO KEY              
LST20    CLC   KEY(14),KEYSAVE     MATCH REP & RECTYPE?                         
         BNE   LST50               NO - DONE READING KEYS                       
*                                                                               
         OC    STATION,STATION     CHECK STATION FILTER                         
         BZ    LST25                                                            
         CLC   RCON9EST,STATION                                                 
         BNE   LST10                                                            
*                                                                               
LST25    OC    OLDREP,OLDREP       CHECK OLD REP FILTER                         
         BZ    LST27                                                            
         CLC   RCON9ESR,OLDREP                                                  
         BNE   LST10                                                            
*                                                                               
LST27    OC    SDATE,SDATE         CHECK ACTIVITY DATE FILTER                   
         BZ    LST30                                                            
         CLC   RCON9EDT,SDATE                                                   
         BL    LST10                                                            
         CLC   RCON9EDT,EDATE                                                   
         BH    LST10                                                            
*                                                                               
LST30    GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R5,LISTAR                                                        
         USING LISTD,R5                                                         
*                                  DISPLAY K NUMBER                             
         GOTO1 HEXOUT,DMCB,RCONKCON,LISTK,L'RCONKCON                            
         LA    R2,LISTK                                                         
         BAS   RE,FORMATK                                                       
*                                                                               
         MVC   LISTSTA(4),RCONKSTA DISPLAY STATION                              
         CLI   RCONKSTA+4,C' '                                                  
         BE    LST35                                                            
         MVI   LISTSTA+4,C'-'                                                   
         MVC   LISTSTA+5(1),RCONKSTA+4                                          
*                                                                               
LST35    MVI   ELCODE,X'2A'        MOVE HISTORY ELEMENT                         
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE IF WE HAVE A 9E KEY            
         USING RCONMMEL,R6                                                      
*                                  DISPLAY ACTIVITY DATE                        
         GOTO1 DATCON,DMCB,(2,RCONMMDT),(11,LISTDATE)                           
*                                  DISPLAY ORIGINAL K NUMBER                    
         GOTO1 HEXOUT,DMCB,RCONMMOC,LISTOK,L'RCONMMOC                           
         LA    R2,LISTOK                                                        
         BAS   RE,FORMATK                                                       
*                                                                               
         MVC   LISTOR,RCONMMOR     DISPLAY ORIGINAL REP                         
         DROP  R6                                                               
*                                                                               
         BAS   RE,CONTOT           TOTAL BUCKETS FOR CURRENT CONTRACT           
LST40    EDIT  FULL,(9,LISTTOT),FLOAT=$,COMMAS=YES,ZERO=NOBLANK                 
*                                                                               
         GOTO1 LISTMON             DISPLAY IT                                   
         B     LST10               BACK FOR NEXT MOVE REC                       
         DROP  R4,R5                                                            
*                                                                               
LST50    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* KEY9E - GENCON CHANGES KEY TO THE ACTIVE KEY. WE NEED TO REBUILD THE          
*         9E PASSIVE KEY FOR THE CURRENT K SO WE CAN PICKUP READING             
*         WHERE WE LEFT OFF.                                                    
*                                                                               
KEY9E    NTR1                                                                   
         LA    R4,KEY                                                           
         USING RCONREC,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RCON9ETP,X'9E'                                                   
         MVC   RCON9ERP,AGENCY                                                  
         L     R6,AIO                                                           
         MVC   RCON9EST,RCONKSTA-RCONKEY(R6)                                    
         MVI   ELCODE,X'2A'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONMMEL,R6                                                      
         MVC   RCON9EDT,RCONMMDT                                                
         MVC   RCON9ESR,RCONMMOR                                                
         MVC   RCON9ESC,RCONMMOC                                                
         DROP  R6,R4                                                            
         B     XIT                                                              
*                                                                               
* FORMATK - STRIPS THE LEADING ZEROS FROM AN 8 BYTE K NUMBER AT R2              
*                                                                               
FORMATK  NTR1                                                                   
         LA    R3,8                                                             
FMTK10   CLI   0(R2),C'0'                                                       
         BNE   XIT                                                              
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         BCT   R3,FMTK10                                                        
         B     XIT                                                              
*                                                                               
* CONTOT - TOTALS BUCKETS FOR K, ROUNDS TO DOLLAR, RESULT IN FULL               
*                                                                               
CONTOT   NTR1                                                                   
         SR    R3,R3               READ 1F ELEM FOR K $TOTAL                    
         L     R4,AIO                                                           
         USING RCONREC,R4                                                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CONTOT10 BAS   RE,NEXTEL                                                        
         BNE   CONTOT20                                                         
         USING RCONBKEL,R6                                                      
         ICM   R4,15,RCONBKAM                                                   
         AR    R3,R4                                                            
         B     CONTOT10                                                         
CONTOT20 SLL   R3,1                ROUND TO DOLLAR                              
         A     R3,=F'100'                                                       
         SR    R2,R2                                                            
         D     R2,=F'100'                                                       
         SRL   R3,1                                                             
         ST    R3,FULL                                                          
         B     XIT                                                              
         DROP  R4,R6                                                            
***********************************************************************         
* HHOOK - HEADHOOK ROUTINE                                                      
***********************************************************************         
HHOOK    NTR1                                                                   
         OC    STATION,STATION                                                  
         BNZ   *+14                                                             
         MVC   H3+17(3),=C'ALL'                                                 
         B     HOOK10                                                           
         LA    R2,H3+17                                                         
         MVC   0(4,R2),STATION                                                  
         LA    R2,3(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   STATION+4,C' '                                                   
         BE    *+8                                                              
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),STATION+4                                                
HOOK10   DS    0H                                                               
         OC    OLDREP,OLDREP                                                    
         BNZ   *+14                                                             
         MVC   H3+34(3),=C'ALL'                                                 
         B     HOOK20                                                           
         MVC   H3+34(2),OLDREP                                                  
HOOK20   DS    0H                                                               
         OC    SDATE,SDATE                                                      
         BNZ   *+14                                                             
         MVC   H3+54(3),=C'ALL'                                                 
         B     HOOK30                                                           
         GOTO1 DATCON,DMCB,(2,SDATE),(11,H3+54)                                 
         CLC   SDATE,EDATE                                                      
         BE    HOOK30                                                           
         MVI   H3+62,C'-'                                                       
         GOTO1 DATCON,DMCB,(2,EDATE),(11,H3+63)                                 
HOOK30   DS    0H                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
* HEADSPECS                                                                     
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H2,1,AGYADD                                                      
         PSPEC H1,38,C'CONTRACT MOVE HISTORY'                                   
         PSPEC H2,38,C'---------------------'                                   
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         PSPEC H3,1,C'FILTERS:'                                                 
         PSPEC H3,10,C'STATION='                                                
         PSPEC H3,27,C'OLD REP='                                                
         PSPEC H3,41,C'ACTIVITY DATE='                                          
         PSPEC H4,1,SPACES                                                      
         PSPEC H5,1,SPACES                                                      
         PSPEC H6,02,C'CONTRACT'                                                
         PSPEC H6,13,C'STATION'                                                 
         PSPEC H6,23,C'ORIG REP'                                                
         PSPEC H6,34,C'ACT DATE'                                                
         PSPEC H6,45,C'ORIG CON'                                                
         PSPEC H6,56,C'CON TOTAL'                                               
         PSPEC H7,02,C'--------'                                                
         PSPEC H7,13,C'-------'                                                 
         PSPEC H7,23,C'--------'                                                
         PSPEC H7,34,C'--------'                                                
         PSPEC H7,45,C'--------'                                                
         PSPEC H7,56,C'---------'                                               
         SPACE 1                                                                
         DC    X'00'                                                            
***********************************************************************         
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
ERREND   GOTO1 MYERROR                                                          
         SPACE 3                                                                
RELO     DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMWTWA                                                                     
* RESFMFB4                                                                      
* RESFMWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMB4D                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWTWA                                                      
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
         DS    0F                                                               
*               WORK AREA                                                       
GRANDTOT DS    F                                                                
SUBTOT   DS    F                                                                
COUNT    DS    H                                                                
SUBCOUNT DS    H                                                                
STATION  DS    CL5                                                              
OLDREP   DS    CL2                                                              
SDATE    DS    XL2                                                              
EDATE    DS    XL2                                                              
LASTSTA  DS    CL5               LAST STATION TO BE PROCESSED (REPORT)          
*                                                                               
         DS    0F                                                               
*                                                                               
LISTD    DSECT                                                                  
         DS    CL1                                                              
LISTK    DS    CL8                                                              
         DS    CL3                                                              
LISTSTA  DS    CL6                                                              
         DS    CL4                                                              
LISTOR   DS    CL2                                                              
         DS    CL9                                                              
LISTDATE DS    CL8                                                              
         DS    CL3                                                              
LISTOK   DS    CL8                                                              
         DS    CL3                                                              
LISTTOT  DS    CL9                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE REGENCON                                                       
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
*  DEDBLOCK                                                                     
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*  DDFLDIND                                                                     
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'158RESFM34   07/05/00'                                      
         END                                                                    
