*          DATA SET SPLDEXTMVC AT LEVEL 078 AS OF 04/01/99                      
*PHASE SPEXTMVC                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE BINSRCH2                                                               
*        TITLE 'SPLDEXT -SPTDIR/FIL LOAD/DUMP MODEL EXTERN'                     
         TITLE 'SPLDEXT -SPTDIR/FIL LOAD/DUMP MODEL EXTERN'                     
***********************************************************************         
*                                                                     *         
*        MOVE HEADERS,BUYS AND GOALS FROM ONE AGENCY TO ANOTHER       *         
*                                                                     *         
*        DO NOT DELETE                                                *         
*        SAVE FOR POSTERITY                                           *         
*                                                                     *         
*        BIGGEST PROBLEM IS MATCHING MARKET NUMBERS. THIS IS DONE     *         
*        BY READING IN A DATASET WITH MATCHES HAND ENTERED            *         
*                                                                     *         
*        THIS MODULE RESTRICTED TO SPTDIR/FIL                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
* PARAMETER LIST                                                      *         
*                                                                     *         
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                     *         
*                               X'01'= RECORD IN CORE                 *         
*                               X'FF'= END OF FILE                    *         
*               RETURN VALUE    X'00'= KEEP RECORD                    *         
*                               X'FF'= PURGE RECORD                   *         
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ        *         
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                     *         
*                               X'40'= TAPE OUTPUT                    *         
*                               X'20'= RECORD IS I/S FILE RECORD      *         
* P3=A(PARAM CARD)                                                    *         
* P4=A(FILE DEFN)                                                     *         
* P5=A(PRINTER)                                                       *         
* P6=A(CPRINT)                                                        *         
*                                                                     *         
***********************************************************************         
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - INIT'                      
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 0,SPLDEXT                                                        
*                                                                               
         ST    R1,APARM            SAVE A(PARAMETER LIST)                       
         MVC   PLIST,0(R1)         SAVE PARAMETER LIST                          
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         L     R9,VLDDEFN          ESTABLISH LOAD CONTROLS                      
         USING LDDEFND,R9                                                       
*                                                                               
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
*                                                                               
         L     RF,=V(CLPACK)                                                    
         ST    RF,VCLPACK                                                       
*                                                                               
         B     INITX                                                            
*                                                                               
INITX    DS    0H                                                               
*                                                                               
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - DMCTL'                     
***********************************************************************         
*                                                                     *         
*        CONTROL FLOW LOGIC                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXCTL   DS    0H                                                               
*                                                                               
         CLI   PRMMODE,PRMMINIQ                                                 
         BE    DMXINIT             INITIALIZE                                   
*                                                                               
         CLI   PRMMODE,PRMMRECQ    NEW RECORD IN CORE                           
         BE    DMXREC              PROCESS                                      
*                                                                               
         CLI   PRMMODE,PRMMEOFQ                                                 
         BE    DMXEOF              END-OF-FILE                                  
*                                                                               
         B     DMXIT                                                            
*                                                                               
*        EXITS                                                                  
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),PRMRKPQ                                                    
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),PRMRPRGQ                                                   
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),PRMREOJQ                                                   
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
*                                                                               
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - DMXINIT'                   
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
*                                                                               
         MVI   EOFTOSW,0           INIT END OF FILE SWITCH                      
*                                                                               
*        PACK CLIENTS                                                           
*                                                                               
         LA    R4,MVTAB            POINT TO SELECTION TABLE                     
         USING MVTABD,R4           ESTABLISH TABLE                              
*                                                                               
DMXINSLP DS    0H                                                               
*                                                                               
         CLI   MVTAGYMD,X'FF'      DONE AT END OF TABLE                         
         BE    DMXINSDN                                                         
*                                                                               
         GOTO1 VCLPACK,DMCB,MVTCLT,MVTCLTPK                                     
*                                                                               
DMXINSCN DS    0H                                                               
         LA    R4,MVTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
         B     DMXINSLP                                                         
*                                                                               
DMXINSDN DS    0H                                                               
*                                                                               
         OPEN  (FILEFROM,(INPUT))    OPEN FROM AGENCY STATION FILE              
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FILETO,(INPUT))      OPEN TO   AGENCY STATION FILE              
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ISSUE GETMAIN FOR STORAGE TO HOLD CONVERSION TABLE                     
*                                                                               
         LHI   R0,MVCRECLQ         RECORD LENGTH                                
         MHI   R0,MVCRMAXQ         *MAXIMUM NUMBER OF RECORDS                   
*                                                                               
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R1,15,TABADDR       SAVE A(GETMAIN AREA)                         
         LR    R3,R1               SAVE A(GETMAIN AREA)                         
         SR    R5,R5                                                            
*                                                                               
*        INITIALIZE BINSRCH PARAMETERS                                          
*                                                                               
         MVC   BSPATAB,TABADDR     A(TABLE)                                     
*                                                                               
         LA    RF,MVCRECLQ         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         XC    BSPNOR,BSPNOR       INIT RECORD COUNTER                          
*                                                                               
         LA    RF,MVCKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,MVCKEY-MVCREC KEY DISPLACEMENT                           
*                                                                               
         LHI   RF,MVCRMAXQ         SET # OF AVAILABLE ENTRIES                   
         ST    RF,BSPMAX                                                        
*                                                                               
*        FILL CONVERSION TABLE FROM INPUT FILE                                  
*                                                                               
         LA    R5,FROMRECC         ESTABLISH FROM AGENCY STATION REC            
         USING FRMRECD,R5                                                       
*                                                                               
         LA    R6,TORECC           ESTABLISH TO   AGENCY STATION REC            
         USING TORECD,R6                                                        
*                                                                               
         LA    R7,MVCRECC          ESTABLISH TABLE RECORD                       
         USING MVCRECD,R7                                                       
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1+2(20),=CL20'CONVERSION FILE'  LABEL PRINT OUT               
*                                                                               
DMXINLP  DS    0H                                                               
*                                                                               
         GET   FILEFROM,FRMREC      READ NEXT RECORD                            
*                                                                               
*        COMPARE TO CURRENT TO RECORD                                           
*                                                                               
DMXINTLP DS    0H                                                               
*                                                                               
         CLC   FRMTYPE,TOTYPE      TYPES MUST MATCH                             
         BH    DMXINTCN                                                         
         BL    DMXINTDN                                                         
*                                                                               
         CLC   FRMMED,TOMED        MEDIA MUST MATCH                             
         BH    DMXINTCN                                                         
         BL    DMXINTDN                                                         
*                                                                               
         CLC   FRMCALL,TOCALL      STATIONS MUST MATCH                          
         BH    DMXINTCN                                                         
         BL    DMXINTDN                                                         
*                                                                               
         CLC   TOCLT,=C'000'       TOREC MUST BE MASTER STATION REC             
         BNE   DMXINTCN                                                         
*                                                                               
         B     DMXINTFD            MATCH FOUND                                  
*                                                                               
DMXINTCN DS    0H                                                               
*                                                                               
         CLI   EOFTOSW,0           SKIP IF END OF FILE WAS REACHED              
         BNE   DMXINTDN                                                         
*                                                                               
         GET   FILETO,TOREC        READ NEXT TO RECORD                          
*                                                                               
         B     DMXINTLP                                                         
*                                                                               
DMXINTC1 DS    0H                                                               
*                                                                               
         MVI   EOFTOSW,X'FF'       INDICATE TO FILE EXHAUSTED                   
*                                                                               
DMXINTDN DS    0H                  NO MATCH                                     
*                                                                               
*        STATION NOT ON RECEIVING AGENCY FILE                                   
*        DROP STATION FROM TABLE                                                
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   MVCAMOLD,FRMAGYMD   SET WITH FROM AGY/MED                        
         MVC   MVCMSOLD,FRMMKSTA   SET WITH FROM MKT/STA                        
         MVC   MVCMKOLD,FRMMKT     SET WITH FROM MARKET                         
         MVC   MVCSTOLD,FRMCALL    SET WITH FROM STATION                        
         MVC   MVCCLOLD,TOCLT      SET WITH FROM CLIENT                         
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PLINED,R4                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVCAMOLD,PAMOLD,1,0,0 AGENCY/MEDIA - OLD            
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVCMSOLD,POLDMSTA,5,0,0  MKT/STA - OLD              
*                                                                               
         MVC   POLDMK,FRMMKT       OLD MARKET NUMBER                            
*                                                                               
         MVC   POLDSTA,FRMCALL     OLD STATION                                  
*                                                                               
         MVC   PNEWMK(15),=C'UNKNOWN STATION'                                   
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXINCN                                                          
*                                                                               
DMXINTFD DS    0H                                                               
*                                                                               
*        FORMAT TABLE RECORD                                                    
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   MVCAMOLD,FRMAGYMD   SET WITH FROM AGY/MED                        
         MVC   MVCMSOLD,FRMMKSTA   SET WITH FROM MKT/STA                        
         MVC   MVCMKOLD,FRMMKT     SET WITH FROM MARKET                         
         MVC   MVCSTOLD,FRMCALL    SET WITH FROM STATION                        
         MVC   MVCCLOLD,FRMCLT     SET WITH FROM CLIENT                         
*                                                                               
         CLC   FRMCLT,=C'000'      IF MASTER STATION REC                        
         BNE   *+14                                                             
         MVC   MVCCPOLD,=X'FFFF'      FORCE HIGH                                
         B     DMXINTF1                                                         
*                                                                               
         GOTO1 VCLPACK,DMCB,FRMCLT,MVCCPOLD   PACK CLIENT                       
*                                                                               
DMXINTF1 DS    0H                                                               
*                                                                               
         MVC   MVCAMNEW,TOAGYMD    SET WITH TO   AGY/MED                        
         MVC   MVCMSNEW,TOMKSTA    SET WITH TO   MKT/STA                        
         MVC   MVCMKNEW,TOMKT      SET WITH FROM MARKET                         
         MVC   MVCSTNEW,TOCALL     SET WITH FROM STATION                        
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PLINED,R4                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVCAMOLD,PAMOLD,1,0,0 AGENCY/MEDIA - OLD            
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVCAMNEW,PAMNEW,1,0,0 AGENCY/MEDIA - NEW            
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVCMSOLD,POLDMSTA,5,0,0  MKT/STA - OLD              
*                                                                               
         MVC   POLDMK,FRMMKT       OLD MARKET NUMBER                            
*                                                                               
         MVC   POLDSTA,FRMCALL     OLD STATION                                  
         MVC   POLDCLT,FRMCLT      OLD CLIENT                                   
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVCMSNEW,PNEWMSTA,5,0,0  MKT/STA - NEW              
*                                                                               
         MVC   PNEWMK,TOMKT        NEW MARKET NUMBER                            
*                                                                               
         MVC   PNEWSTA,TOCALL      NEW STATION                                  
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',MVCREC) ADD REC TO TABLE           
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FILLED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R2,15,BSPAREC                                                    
*                                                                               
         MVC   P+80(32),0(R2)      DISPLAY TABLE ENTRY                          
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
DMXINCN  DS    0H                                                               
*                                                                               
         B     DMXINLP                                                          
*                                                                               
DMXINDN  DS    0H                                                               
*                                                                               
         CLOSE FILEFROM            CLOSE INPUT FILE                             
         CLOSE FILETO              CLOSE INPUT FILE                             
*                                                                               
*        PRINT TITLES                                                           
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         MVC   MID1+2(8),=CL8'OLD KEY'                                          
         MVC   MID1+32(8),=CL8'NEW KEY'                                         
         MVC   MID1+65(3),=C'OLD'                                               
         MVC   MID1+72(3),=C'NEW'                                               
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))    OPEN OUTPUT FILE                           
*                                                                               
         B     DMXIT                                                            
*                                                                               
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - DMXREC'                    
***********************************************************************         
*                                                                     *         
*        PROCESS NEXT RECORD TO BE ADDED TO FILE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXREC   DS    0H                                                               
*                                                                               
         SR    R6,R6               INIT ELEMENT POINTER                         
*                                                                               
         L     R3,AREC             POINT TO RECORD FOR PROCESSING               
*                                                                               
*        DETERMINE RECORD TYPE                                                  
*                                                                               
         CLI   0(R3),X'00'         TEST HEADER                                  
         BE    HDR                                                              
*                                                                               
         CLI   0(R3),X'10'         TEST BUYREC                                  
         BH    BUY                                                              
*                                                                               
         CLI   0(R3),X'02'         TEST GOALREC                                 
         BE    GOAL                                                             
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - HDR'                       
***********************************************************************         
*                                                                     *         
*        HEADER RECORDS - CLIENT, PRODUCT, ESTIMATE - X'00'           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HDR      DS    0H                                                               
*                                                                               
         USING ESTHDRD,R3          ESTABLISH SPOT GENERIC HEADER RECORD         
*                                                                               
         LA    R4,MVTAB            POINT TO TABLE OF ITEMS TO BE MOVED          
         USING MVTABD,R4                                                        
*                                                                               
HDRKEYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    HDRKEYDN                                                         
*                                                                               
         CLC   EKEYAM,MVTAGYMD     MATCH ON AGENCY/MEDIA                        
         BNE   HDRKEYCN                                                         
*                                                                               
         CLC   EKEYCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   HDRKEYCN                                                         
*                                                                               
         OC    EKEYPRD(9),EKEYPRD  ACCEPT IF CLIENT HEADER                      
         BZ    HDRKEYFD                                                         
*                                                                               
         OC    EKEYEST(6),EKEYEST  ACCEPT IF PRODUCT HEADER                     
         BZ    HDRKEYFD                                                         
*                                                                               
         CLC   EKEYEST,MVTEST      MATCH ON ESTIMATE                            
         BNE   HDRKEYCN                                                         
*                                                                               
         OC    EKEYEST+1(5),EKEYEST+1 SKIP IF NOT ESTIMATE(IE.BILLREC)          
         BNZ   HDRKEYCN                                                         
*                                                                               
         B     HDRKEYFD            ACCEPT HDR                                   
*                                                                               
HDRKEYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTABL(R4)       POINT TO NEXT ENTRY IN TABLE                 
         B     HDRKEYLP                                                         
*                                                                               
HDRKEYFD DS    0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         LA    R3,NEWREC           POINT TO NEW RECORD                          
         USING ESTHDRD,R3          ESTABLISH SPOT GENERIC HEADER RECORD         
*                                                                               
         NI    EKEYAM,X'0F'        KILL OLD AGENCY NYBBLE                       
         OC    EKEYAM,MVAGNEWP     ADD IN NEW AGECNY NYBBLE                     
*                                                                               
         BRAS  RE,WRITE            WRITE RECORD TO OUTPUT DATASET               
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         GOTO1 VPRINTER            PRINT KEYS                                   
*                                                                               
HDRKEYDN DS    0H                                                               
*                                                                               
HDRX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - BUY'                       
***********************************************************************         
*                                                                     *         
*        BUY RECORD - >X'10'                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUY      DS    0H                                                               
*                                                                               
         USING BUYRECD,R3          ESTABLISH SPOT BUY RECORD                    
*                                                                               
         LA    R4,MVTAB            POINT TO TABLE OF ITEMS TO BE MOVED          
         USING MVTABD,R4                                                        
*                                                                               
BUYKEYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    BUYKEYDN                                                         
*                                                                               
         CLC   BUYKAM,MVTAGYMD     MATCH ON AGENCY/MEDIA                        
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   BUYKCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   BUYKEST,MVTEST      MATCH ON ESTIMATE                            
         BNE   BUYKEYCN                                                         
*                                                                               
         B     BUYKEYFD            ACCEPT BUY                                   
*                                                                               
BUYKEYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTABL(R4)       POINT TO NEXT ENTRY IN TABLE                 
         B     BUYKEYLP                                                         
*                                                                               
BUYKEYFD DS    0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         LA    R3,NEWREC           POINT TO NEW RECORD                          
         USING BUYRECD,R3          ESTABLISH SPOT BUY RECORD                    
*                                                                               
         NI    BUYKAM,X'0F'        KILL OLD AGENCY NYBBLE                       
         OC    BUYKAM,MVAGNEWP     ADD IN NEW AGENCY NYBBLE                     
*                                                                               
         LA    R2,BUYMSTA          R2 POINTS TO MARKET CODE                     
*                                  R4 POINTS TO CONVERSION TABLE ENTRY          
*                                                                               
         MVC   WRKCLT,BUYKCLT      SAVE PACKED CLIENT                           
*                                                                               
         MVI   SWITCH,C'B'         INDICATE BUY RECORD                          
         BRAS  RE,GETSTA           FIND NEW MARKET/STATION                      
         MVI   SWITCH,0            CLEAR SWITCH                                 
*                                                                               
         BRAS  RE,WRITE            WRITE RECORD TO OUTPUT DATASET               
*                                                                               
BUYKEYDN DS    0H                                                               
*                                                                               
BUYX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - GOAL'                      
***********************************************************************         
*                                                                     *         
*        GOAL RECORD - X'02'                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GOAL     DS    0H                                                               
*                                                                               
         USING GOALRECD,R3         ESTABLISH SPOT GOAL RECORD                   
*                                                                               
         LA    R4,MVTAB            POINT TO TABLE OF ITEMS TO BE MOVED          
         USING MVTABD,R4                                                        
*                                                                               
GOALKYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    GOALKYDN                                                         
*                                                                               
         CLC   GKEYAM,MVTAGYMD     MATCH ON AGENCY/MEDIA                        
         BNE   GOALKYCN                                                         
*                                                                               
         CLC   GKEYCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   GOALKYCN                                                         
*                                                                               
         CLC   GKEYEST,MVTEST      MATCH ON ESTIMATE                            
         BNE   GOALKYCN                                                         
*                                                                               
         B     GOALKYFD            ACCEPT GOAL RECORD                           
*                                                                               
GOALKYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTABL(R4)       POINT TO NEXT ENTRY IN TABLE                 
         B     GOALKYLP                                                         
*                                                                               
GOALKYFD DS    0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         LA    R3,NEWREC           POINT TO NEW RECORD                          
         USING GOALRECD,R3         ESTABLISH SPOT GOAL RECORD                   
*                                                                               
         NI    GKEYAM,X'0F'        KILL OLD AGENCY NYBBLE                       
         OC    GKEYAM,MVAGNEWP     ADD IN NEW AGECNY NYBBLE                     
*                                                                               
         LA    R2,GKEYMKT          R2 POINTS TO MARKET CODE                     
*                                  R4 POINTS TO CONVERSION TABLE ENTRY          
*                                                                               
         BRAS  RE,GETMKT           FIND NEW MARKET                              
*                                                                               
         BRAS  RE,WRITE            WRITE RECORD TO OUTPUT DATASET               
*                                                                               
GOALKYDN DS    0H                                                               
*                                                                               
GOALX    DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - GETSTA'                    
***********************************************************************         
*                                                                     *         
*        FIND STATION IN CONVERSION TABLE                             *         
*                                                                     *         
*NTRY    R2==> BINARY MARKET CODE                                     *         
*        R4==> ENTRY IN TABLE OF THINGS TO MOVE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETSTA   NTR1  LABEL=*                                                          
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETSTA1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
GETSTA1  DS    0H                                                               
*                                                                               
         LA    R7,MVCRECC          ESTABLISH BINSRCH TABLE WORKAREA             
         USING MVCRECD,R7                                                       
*                                                                               
         XC    MVCREC(MVCRECLQ),MVCREC INIT WORKAREA                            
*                                                                               
         MVC   MVCAMOLD,MVTAGYMD   SET OLD AGY/MD  IN KEY                       
         MVC   MVCMSOLD,0(R2)      SET OLD MKT/STA IN KEY                       
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',MVCKEY)   FIND IN TABLE           
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETSTAER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETSTAX             SKIP IF NOT FOUND                            
*                                                                               
GSTCLTLP DS    0H                                                               
*                                                                               
         CLC   MVCAMOLD,MVTAGYMD   MUST MATCH AGY/MED                           
         BNE   GETSTAER                                                         
*                                                                               
         CLC   MVCMSOLD,0(R2)      MUST MATCH MKT/STA                           
         BNE   GETSTAER                                                         
*                                                                               
         CLC   MVCCPOLD,=X'FFFF'   USE IF MASTER STATION                        
         BE    GSTCLTFD                                                         
*                                                                               
         CLC   MVCCPOLD,WRKCLT     USE IF CLIENT MATCHES                        
         BE    GSTCLTFD                                                         
*                                                                               
GSTCLTCN DS    0H                                                               
*                                                                               
         LA    R7,MVCRECLQ(R7)     BUMP TO NEXT TABLE ENTRY                     
         B     GSTCLTLP                                                         
*                                                                               
GSTCLTFD DS    0H                                                               
*                                                                               
         OC    MVCMSNEW,MVCMSNEW   ERROR IF NO NEW MKT/STA                      
         BZ    GETSTAER                                                         
*                                                                               
         MVC   0(5,R2),MVCMSNEW    SET NEW MARKET/STATION                       
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETSTAXX                                                         
*                                                                               
         MVC   P+65(4),MVCMKOLD       OLD MARKET                                
         MVC   P+70(5),MVCSTOLD       OLD STATION                               
         MVC   P+76(3),MVCCLOLD       OLD CLIENT                                
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+80(4),MVCMKNEW       NEW MARKET                                
         MVC   P+85(5),MVCSTNEW       NEW STATION                               
*                                                                               
GETSTAX  DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     GETSTAXX                                                         
*                                                                               
GETSTAER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R2),P+65,5,0,0   PRINT OLD MKT/STA                 
*                                                                               
         GOTO1 VHEXOUT,DMCB,WRKCLT,P+80,2,0,0   PRINT OLD CLIENT                
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+90(15),=CL15'UNKNOWN STATION'                                  
*                                                                               
GETSTAEX DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
GETSTAXX DS    0H                                                               
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BP    *+10                                                             
         ZAP   CTR,=P'10'          RESET COUNTER                                
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - GETMKT'                    
***********************************************************************         
*                                                                     *         
*        FIND MARKET  IN CONVERSION TABLE                             *         
*                                                                     *         
*NTRY    R2==> BINARY MARKET CODE                                     *         
*        R4==> ENTRY IN TABLE OF THINGS TO MOVE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETMKT   NTR1  LABEL=*                                                          
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETMKT1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
GETMKT1  DS    0H                                                               
*                                                                               
         LA    R7,MVCRECC          ESTABLISH BINSRCH TABLE WORKAREA             
         USING MVCRECD,R7                                                       
*                                                                               
         XC    MVCREC(MVCRECLQ),MVCREC INIT WORKAREA                            
*                                                                               
         MVC   MVCAMOLD,MVTAGYMD   SET OLD AGY/MED IN KEY                       
         MVC   MVCMSOLD(2),0(R2)   SET OLD MKT/STA IN KEY                       
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',MVCKEY)   FIND IN TABLE           
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETMKTER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETMKTER            SKIP IF NOT FOUND                            
*                                                                               
         OC    MVCMSNEW,MVCMSNEW   ERROR IF NO NEW MKT/STA                      
         BZ    GETMKTER                                                         
*                                                                               
         CLC   MVCMSOLD(2),0(R2)   FIND MARKET MATCH                            
         BNE   GETMKTER                                                         
*                                                                               
         MVC   0(2,R2),MVCMSNEW    SET NEW MARKET                               
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETMKTXX                                                         
*                                                                               
         MVC   P+65(4),MVCMKOLD       OLD MARKET                                
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+80(4),MVCMKNEW       NEW MARKET                                
*                                                                               
GETMKTX  DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     GETMKTXX                                                         
*                                                                               
GETMKTER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
         EDIT  (B2,0(R2)),(4,P+65)   OLD MARKET                                 
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+80(15),=CL15'UNKNOWN MARKET'                                   
*                                                                               
GETMKTEX DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
GETMKTXX DS    0H                                                               
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BP    *+10                                                             
         ZAP   CTR,=P'10'          RESET COUNTER                                
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - COPYREC'                   
***********************************************************************         
*                                                                     *         
*        COPY CURRENT RECORD TO WORKAREA                              *         
*                                                                     *         
*NTRY    R3==> RECORD TO BE COPIED                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COPY     NTR1  LABEL=*                                                          
*                                                                               
         LR    R0,R3               POINT TO INCOMING RECORD                     
         LA    RE,NEWREC           POINT TO STORAGE AREA                        
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         GET RECORD LENGTH                            
         LR    RF,R1               COPY LENGTH                                  
*                                                                               
         LA    RF,4(RF)            ALLOW FOR LENGTH BYTES                       
         STCM  RF,3,RRLEN          SET OUTPUT FILE LENGTH                       
         AHI   RF,-4               RESTORE TRUE RECORD LENGTH                   
*                                                                               
         MVCL  RE,R0               COPY INCOMING RECORD                         
*                                                                               
         CLC   MVAGYOLD,NEWREC+20  IF ALPHA AGENCY PRESENT                      
         BNE   *+10                                                             
         MVC   NEWREC+20(2),MVAGYNEW                                            
*                                                                               
COPYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - WRITE'                     
***********************************************************************         
*                                                                     *         
*        WRITE WORKAREA RECORD TO OUTPUT DATASET                      *         
*                                                                     *         
*NTRY    NEWREC HAS RECORD TO GO                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WRITE    NTR1  LABEL=*                                                          
*                                                                               
         PUT   FILEOUT,RRLEN       WRITE TO OUTPUT FILE                         
*                                                                               
WRITEX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         GETEL R6,42,ELCODE                                                     
*                                                                               
FILEFROM DCB   DDNAME=FILEFROM,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,    X        
               EODAD=DMXINDN                                                    
*                                                                               
FILETO   DCB   DDNAME=FILETO,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXINTC1                                                   
*                                                                               
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=25000                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVTAB'                     
***********************************************************************         
*                                                                     *         
*        DATA FOR THIS CONVERSION                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVAGYOLD DC    C'TH'               OLD AGENCY - ZENITH                          
MVAGOLDP DC    X'90'               OLD AGENCY - ZENITH - PACKED                 
*                                                                               
MVAGYNEW DC    C'BS'               NEW AGENCY - BATES                           
MVAGNEWP DC    X'50'               NEW AGENCY - BATES  - PACKED                 
*                                                                               
*        TABLE OF WHAT TO MOVE                                                  
*                                                                               
MVTAB    DS    0X                  TABLE ENTRY                                  
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(107)                          
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(108)                          
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(109)                          
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(110)                          
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(111)                          
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(112)                          
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(207)                          
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(208)                          
         DC    X'92',C'WJ9',X'0000',X'000000',AL1(209)                          
*                                                                               
         DC    X'91',C'WJ9',X'0000',X'000000',AL1(007)                          
         DC    X'91',C'WJ9',X'0000',X'000000',AL1(008)                          
         DC    X'91',C'WJ9',X'0000',X'000000',AL1(009)                          
         DC    X'91',C'WJ9',X'0000',X'000000',AL1(010)                          
         DC    X'91',C'WJ9',X'0000',X'000000',AL1(011)                          
         DC    X'91',C'WJ9',X'0000',X'000000',AL1(012)                          
*                                                                               
         DC    X'92',C'WH9',X'0000',X'000000',AL1(107)                          
         DC    X'92',C'WH9',X'0000',X'000000',AL1(108)                          
         DC    X'92',C'WH9',X'0000',X'000000',AL1(109)                          
         DC    X'92',C'WH9',X'0000',X'000000',AL1(110)                          
         DC    X'92',C'WH9',X'0000',X'000000',AL1(111)                          
         DC    X'92',C'WH9',X'0000',X'000000',AL1(112)                          
         DC    X'92',C'WH9',X'0000',X'000000',AL1(130)                          
*                                                                               
         DC    X'91',C'WH9',X'0000',X'000000',AL1(007)                          
*        DC    X'91',C'WH9',X'0000',X'000000',AL1(008)                          
         DC    X'91',C'WH9',X'0000',X'000000',AL1(009)                          
         DC    X'91',C'WH9',X'0000',X'000000',AL1(010)                          
*        DC    X'91',C'WH9',X'0000',X'000000',AL1(011)                          
*        DC    X'91',C'WH9',X'0000',X'000000',AL1(012)                          
         DC    X'91',C'WH9',X'0000',X'000000',AL1(048)                          
         DC    X'91',C'WH9',X'0000',X'000000',AL1(049)                          
         DC    X'91',C'WH9',X'0000',X'000000',AL1(051)                          
*                                                                               
         DC    X'92',C'WN9',X'0000',X'000000',AL1(107)                          
         DC    X'92',C'WN9',X'0000',X'000000',AL1(109)                          
         DC    X'92',C'WN9',X'0000',X'000000',AL1(212)                          
*                                                                               
         DC    X'FF'               EOT                                          
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - WORKD'                     
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DS    0D                                                               
WORK     DS    CL128                                                            
DMCB     DS    6F                                                               
APARM    DS    A                   A(PARAMETER LIST)                            
*                                                                               
BYTE     DS    X                                                                
SWITCH   DC    X'0'                C'B' - INDCATES MKT FROM BUY RECORD          
COUNT    DS    F                                                                
CTR      DC    PL2'10'             TRACE COUNTER                                
*                                                                               
FROMRECC DS    CL(FRMRECLQ)        FROM AGENCY STATION RECORD                   
TORECC   DS    CL(TORECLQ)         TO   AGENCY STATION RECORD                   
MVCRECC  DS    XL(MVCRECLQ)        TABLE ENTRY BUILD AREA                       
TABADDR  DS    A                   A(BINSRCH TABLE)                             
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMD   DS    CL1                                                              
WAGYMD   DS    CL1                 MASTER AGY/MEDIA                             
PRINTSW  DC    XL1'00'             X'01' - PRINT TRACE                          
OLDKEY   DC    XL13'00'            OLD KEY SAVEAREA                             
NEWKEY   DC    XL13'00'            NEW KEY SAVEAREA                             
EOFTOSW  DC    X'00'               END OF FILE SWITCH FOR TO FILE               
*                                                                               
*                                                                               
*        PARAMETER LIST SAVE AREA                                               
*                                                                               
         DS    0D                  ALIGNMENT                                    
PLIST    DS    0CL24               PARAMETER LIST - SAVED                       
PRMMODE  DS    0XL1                CALLING MODE                                 
PRMMINIQ EQU   X'00'                 X'00'= INITIALISE                          
PRMMRECQ EQU   X'01'                 X'01'= RECORD IN CORE                      
PRMMEOFQ EQU   X'FF'                 X'FF'= END OF FILE                         
*                                                                               
PRMRTNCD DS    0XL1                RETURN CODE                                  
PRMRKPQ  EQU   X'00'               X'00'= KEEP RECORD                           
PRMRPRGQ EQU   X'FF'               X'FF'= PURGE RECORD                          
PRMREOJQ EQU   X'FF'               X'FF'/C'EOJ'=PURGE & CAUSE EOJ               
*                                                                               
AREC     DS    A                   A(CURRENT RECORD)                            
*                                                                               
VTAPEOUT DS    A                   V(TAPEOUT DCB)                               
APARAMC  DS    A                   A(PARAMETER CARD)                            
VLDDEFN  DS    A                   A(FILE DEFINITION)                           
VPRINTER DS    A                   V(PRINTER)                                   
VCPRINT  DS    A                   V(CPRINT)                                    
VHEXOUT  DS    A                   V(HEXOUT)                                    
VCLPACK  DS    A                   V(HEXOUT)                                    
*                                                                               
ELCODE   DS    X                                                                
WRKCLT   DS    XL2                 PACKED CLIENT CODE                           
*                                                                               
         DS    0A                  ALIGNMENT                                    
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
RRLEN    DS    XL2                 OUTPUT RECORD LENGTH                         
         DS    XL2                 SPARE                                        
NEWREC   DS    XL4096              NEW RECORD BUILD AREA                        
*                                                                               
WORKLQ   EQU   *-WORKD             LENGTH OF WORKING STORAGE                    
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVTABD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DATA TO MOVE                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVTABD   DSECT                                                                  
MVTAGYMD DC    X'92'               AGENCY/MEDIA                                 
MVTCLT   DC    C'WJ9'              CLIENT                                       
MVTCLTPK DC    XL2'00'             CLIENT - PACKED                              
MVTPRD   DC    XL3'00'             PRODUCT  - ALL                               
MVTEST   DC    AL1(107)            ESTIMATE - ALL                               
MVTABL   EQU   *-MVTABD            TABLE ENTRY LENGTH                           
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVCRECD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR NUMBER CONVERSION TABLE - DIFFERENT ORDER THAN     *         
*              ORIGINAL FILE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVCRECD  DSECT                                                                  
MVCREC   DS    0XL1                CONVERSION RECORD                            
MVCKEY   DS    0XL1                KEY FOR TABLE                                
MVCAMOLD DS    XL1                 OLD AGENCY/MEDIA                             
MVCMSOLD DS    XL5                 OLD MARKET/STATION                           
MVCCPOLD DS    CL2                 OLD CLIENT - PACKED                          
MVCKEYLQ EQU   *-MVCKEY            KEY LENGTH                                   
*                                                                               
MVCMKOLD DS    CL4                 OLD MARKET NUMBER                            
MVCSTOLD DS    CL5                 OLD STATION                                  
MVCCLOLD DS    CL3                 OLD CLIENT                                   
MVCAMNEW DS    XL1                 NEW AGENCY/MEDIA                             
MVCMSNEW DS    XL5                 NEW MARKET/STATION                           
MVCMKNEW DS    CL4                 NEW MARKET NUMBER                            
MVCSTNEW DS    CL5                 NEW STATION                                  
MVCRECLQ EQU   *-MVCREC            RECORD LENGTH                                
*                                                                               
MVCRMAXQ EQU   10000               MACXIMUM NUMBER OF RECORDS IN FILE           
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - FROMRECD'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR FROM MARKET/STATION CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FRMRECD  DSECT                                                                  
FRMREC   DS    0XL1                CONVERSION RECORD                            
FRMKEY   DS    0XL15               STATION RECORD KEY                           
FRMTYPE  DS    CL1'S'              STATION RECORD TYPE                          
FRMMED   DS    CL1                 MEDIA                                        
FRMCALL  DS    CL5                 STATION CALL LETTERS                         
FRMAGY   DS    CL2                 AGENCY                                       
FRMCLT   DS    CL3                 CLIENT                                       
         DS    CL3'000'            FILL                                         
*                                                                               
FRMMKT   DS    CL4                 MARKET                                       
FRMMKSTA DS    XL5                 MKT/STA PACKED                               
FRMAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-FRMRECD))  SPARE                                        
FRMRECLQ EQU   *-FRMREC            RECORD LENGTH                                
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - TORECD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TO   MARKET/STATION CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TORECD   DSECT                                                                  
TOREC    DS    0XL1                CONVERSION RECORD                            
TOKEY    DS    0XL15               STATION RECORD KEY                           
TOTYPE   DS    CL1'S'              STATION RECORD TYPE                          
TOMED    DS    CL1                 MEDIA                                        
TOCALL   DS    CL5                 STATION CALL LETTERS                         
TOAGY    DS    CL2                 AGENCY                                       
TOCLT    DS    CL3                 CLIENT                                       
         DS    CL3'000'            FILL                                         
*                                                                               
TOMKT    DS    CL4                 MARKET                                       
TOMKSTA  DS    XL5                 MKT/STA PACKED                               
TOAGYMD  DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-TORECD))   SPARE                                        
TORECLQ EQU    *-TOREC             RECORD LENGTH                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PAMOLD   DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL1                                                              
POLDMSTA DS    CL10                MARKET/STATION - OLD - HEX                   
         DS    CL2                                                              
POLDMK   DS    CL4                 MARKET NUMBER  - OLD - DECIMAL               
         DS    CL2                                                              
POLDSTA  DS    CL5                 STATION CALL   - OLD - DECIMAL               
         DS    CL2                                                              
POLDCLT  DS    CL3                 CLIENT         - OLD                         
         DS    CL1                                                              
PAMNEW   DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PNEWMSTA DS    CL10                MARKET/STATION - NEW - HEX                   
         DS    CL2                                                              
PNEWMK   DS    CL4                 MARKET NUMBER  - NEW - DECIMAL               
         DS    CL2                                                              
PNEWSTA  DS    CL5                 STATION CALL   - NEW - DECIMAL               
         DS    CL2                                                              
* DSECT FOR TITLE PRINT LINE                                                    
PTLINED  DSECT                                                                  
         DS    CL1                                                              
PTAMOLD  DS    CL6'AM-OLD'         AGENCY/MEDIA - OLD - HEX                     
         DS    CL1                                                              
PTAMNEW  DS    CL6'AM-NEW'         AGENCY/MEDIA - NEW - HEX                     
         DS    CL1                                                              
         DS    CL2                                                              
PTCLTOLD DS    CL3'CLT'            CLIENT - OLD                                 
         DS    CL2                                                              
PTCLPOLD DS    CL4'CLT'            PACKED CLIENT - OLD                          
         DS    CL2                                                              
PTCLTNEW DS    CL3'CLT'            CLIENT - NEW                                 
         DS    CL2                                                              
PTCLPNEW DS    CL4'CLT'            PACKED CLIENT - NEW                          
         DS    CL2                                                              
PTOLDMST DS    CL10'MKT/STA'       MARKET/STATION - OLD - HEX                   
         DS    CL2                                                              
PTOLDMKT DS    CL4'MKT'            MARKET NUMBER  - OLD - DECIMAL               
         DS    CL2                                                              
PTOLDSTA DS    CL5'STA'            STATION CALL   - OLD                         
         DS    CL2                                                              
PTNEWMST DS    CL10'MKT/STA'       MARKET/STATION - NEW - HEX                   
         DS    CL2                                                              
PTNEWMKT DS    CL4'MKT'            MARKET NUMBER  - NEW - DECIMAL               
         DS    CL2                                                              
PTNEWSTA DS    CL5'STA'            STATION CALL   - NEW                         
         DS    CL1                                                              
         EJECT                                                                  
*SPGENBUY                                                                       
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENEST                                                                       
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENGOAL                                                                      
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078SPLDEXTMVC04/01/99'                                      
         END                                                                    
*                                                                               
