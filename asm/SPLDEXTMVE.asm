*          DATA SET SPLDEXTMVE AT LEVEL 050 AS OF 02/25/99                      
*PHASE SPEXTMVE                                                                 
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
         TITLE 'SPLDEXTMVE - MOVE BETWEEN AGENCIES - INIT'                      
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
*        DATA FOR THIS CONVERSION                                               
*                                                                               
MVAGYOLD DC    C'TH'               OLD AGENCY - ZENITH                          
MVAGOLD  DC    X'90'               OLD LOCAL AGENCY - ZENITH                    
*                                                                               
MVCLTOLD DC    C'OND'              OLD CLIENT                                   
MVCLPOLD DS    XL2'00'             OLD CLIENT - PACKED                          
*                                                                               
MVAGYNEW DC    C'OM'               NEW AGENCY - O&M                             
MVAGNEW  DC    X'10'               NEW LOCAL AGENCY - O&M                       
*                                                                               
MVCLTNEW DC    C'XXX'              NEW CLIENT                                   
MVCLPNEW DS    XL2'00'             NEW CLIENT PACKED                            
*                                                                               
*        TABLE OF WHAT TO MOVE                                                  
*                                                                               
MVTAB    DS    0X                  TABLE ENTRY                                  
MVTAGYMD DC    X'91'               AGENCY/MEDIA                                 
MVTEST   DC    AL1(13)             ESTIMATE                                     
MVTABL   EQU   *-MVTAB             TABLE ENTRY LENGTH                           
*                                                                               
         DC    X'91',AL1(14)                                                    
         DC    X'91',AL1(15)                                                    
         DC    X'91',AL1(16)                                                    
         DC    X'92',AL1(17)                                                    
         DC    X'92',AL1(18)                                                    
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
INITX    DS    0H                                                               
*                                                                               
         TITLE 'SPLDEXTMVE - MOVE BETWEEN AGENCIES - DMCTL'                     
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
         TITLE 'SPLDEXTMVE - MOVE BETWEEN AGENCIES - DMXINIT'                   
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
*                                                                               
*        PACK OLD AND NEW CLIENTS                                               
*                                                                               
         GOTO1 VCLPACK,DMCB,MVCLTOLD,MVCLPOLD                                   
*                                                                               
         GOTO1 VCLPACK,DMCB,MVCLTNEW,MVCLPNEW                                   
*                                                                               
         OPEN  (FILEIN,(INPUT))    OPEN CONVERSION FILE                         
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ISSUE GETMAIN FOR STORAGE TO HOLD CONVERSION TABLE                     
*                                                                               
         LHI   R0,MVERECLQ         RECORD LENGTH                                
         MHI   R0,MVERMAXQ         *MAXIMUM NUMBER OF RECORDS                   
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
         LA    RF,MVERECLQ         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         XC    BSPNOR,BSPNOR       INIT RECORD COUNTER                          
*                                                                               
         LA    RF,MVEKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,MVEKEY-MVEREC KEY DISPLACEMENT                           
*                                                                               
         LHI   RF,MVERMAXQ         SET # OF AVAILABLE ENTRIES                   
         ST    RF,BSPMAX                                                        
*                                                                               
*        FILL CONVERSION TABLE FROM INPUT FILE                                  
*                                                                               
         LA    R5,TEMP             ESTABLISH INPUT RECORD                       
         USING CNVRECD,R5                                                       
*                                                                               
         LA    R7,MVERECC          ESTABLISH TABLE RECORD                       
         USING MVERECD,R7                                                       
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1+2(20),=CL20'CONVERSION FILE'  LABEL PRINT OUT               
*                                                                               
         LA    R4,MID2             ESTABLISH TITLE LINE                         
         USING PTLINED,R4                                                       
*                                                                               
         MVC   PTAMOLD,=C'AM-OLD'                                               
         MVC   PTAMNEW,=C'AM-NEW'                                               
         MVC   PTCLTOLD,=C'CLT'                                                 
         MVC   PTCLPOLD,=C'CLT '                                                
         MVC   PTCLTNEW,=C'CLT'                                                 
         MVC   PTCLPNEW,=C'CLT '                                                
         MVC   PTMKTNM,=CL31'MARKET'                                            
         MVC   PTOLDMK,=CL4' MKT'                                               
         MVC   PTOLDMKD,=C'OLD '                                                
         MVC   PTNEWMK,=CL4' MKT'                                               
         MVC   PTNEWMKD,=C'NEW '                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
DMXINLP  DS    0H                                                               
*                                                                               
         GET   FILEIN,TEMP         READ NEXT RECORD                             
*                                                                               
*        FORMAT TABLE RECORD                                                    
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   MVEAMOLD,MVAGOLD    INIT WITH LOCAL AGENCY CODE                  
*                                                                               
         CLI   CNVMED,C'T'         SET MEDIA NIBBLE                             
         BNE   *+12                                                             
         OI    MVEAMOLD,X'01'         TV                                        
         B     *+8                                                              
         OI    MVEAMOLD,X'02'         RADIO                                     
*                                                                               
         MVC   MVEAMNEW,MVAGNEW    INIT WITH LOCAL AGENCY CODE                  
*                                                                               
         CLI   CNVMED,C'T'         SET MEDIA NIBBLE                             
         BNE   *+12                                                             
         OI    MVEAMNEW,X'01'         TV                                        
         B     *+8                                                              
         OI    MVEAMNEW,X'02'         RADIO                                     
*                                                                               
         MVC   MVECLT,CNVCLT       CLIENT                                       
*                                                                               
         GOTO1 VCLPACK,DMCB,MVECLT,MVECLTPK    PACK CLIENT CODE                 
*                                                                               
         MVC   MVEMKTNM,CNVMKTNM   MARKET NAME                                  
*                                                                               
         PACK  DUB,CNVMKOLD        OLD MARKET NUMBER                            
         CVB   RF,DUB                                                           
         STCM  RF,3,MVEMKOLD                                                    
*                                                                               
         PACK  DUB,CNVMKNEW        NEW MARKET NUMBER                            
         CVB   RF,DUB                                                           
         STCM  RF,3,MVEMKNEW                                                    
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PLINED,R4                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVEAMOLD,PAMOLD,1,0,0 AGENCY/MEDIA - OLD            
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVEAMNEW,PAMNEW,1,0,0 AGENCY/MEDIA - NEW            
*                                                                               
         MVC   PCLTOLD,MVCLTOLD    CLIENT OLD                                   
         GOTO1 VHEXOUT,DMCB,MVCLPOLD,PCLPOLD,2,0,0 OLD PACKED CLIENT            
*                                                                               
         MVC   PCLTNEW,MVCLTNEW    CLIENT NEW                                   
         GOTO1 VHEXOUT,DMCB,MVCLPNEW,PCLPNEW,2,0,0 NEW PACKED CLIENT            
*                                                                               
         MVC   PMKTNM,MVEMKTNM                  MARKET NAME                     
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVEMKOLD,POLDMK,2,0,0 NEW MARKET NUMBER             
*                                                                               
         MVC   POLDMKD-1(6),=C'(    )'  IN DECIMAL                              
         EDIT  (B2,MVEMKOLD),(4,POLDMKD),0                                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVEMKNEW,PNEWMK,2,0,0 NEW MARKET NUMBER             
*                                                                               
         MVC   PNEWMKD-1(6),=C'(    )'  IN DECIMAL                              
         EDIT  (B2,MVEMKNEW),(4,PNEWMKD),0                                      
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',MVEREC) ADD REC TO TABLE           
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FILLED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
DMXINCN  DS    0H                                                               
*                                                                               
         B     DMXINLP                                                          
*                                                                               
DMXINDN  DS    0H                                                               
*                                                                               
         CLOSE FILEIN              CLOSE INPUT FILE                             
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
         TITLE 'SPLDEXTMVE - MOVE BETWEEN AGENCIES - DMXREC'                    
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
         TITLE 'SPLDEXTMVE - MOVE BETWEEN AGENCIES - HDR'                       
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
*                                                                               
HDRKEYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    HDRKEYDN                                                         
*                                                                               
         CLC   EKEYAM,MVTAGYMD-MVTAB(R4)  MATCH ON AGENCY/MEDIA                 
         BNE   HDRKEYCN                                                         
*                                                                               
         CLC   EKEYCLT,MVCLPOLD           MATCH ON OLD PACKED CLIENT            
         BNE   HDRKEYCN                                                         
*                                                                               
         OC    EKEYPRD(9),EKEYPRD  ACCEPT IF CLIENT HEADER                      
         BZ    HDRKEYFD                                                         
*                                                                               
         OC    EKEYEST(6),EKEYEST  ACCEPT IF PRODUCT HEADER                     
         BZ    HDRKEYFD                                                         
*                                                                               
         CLC   EKEYEST,MVTEST-MVTAB(R4)    MATCH ON ESTIMATE                    
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
         BRAS  RE,COPY                                                          
*                                                                               
         LA    R3,NEWREC           POINT TO NEW RECORD                          
         USING ESTHDRD,R3          ESTABLISH SPOT GENERIC HEADER RECORD         
*                                                                               
         NI    EKEYAM,X'0F'        KILL OLD AGENCY NYBBLE                       
         OC    EKEYAM,MVAGNEW      ADD IN NEW AGECNY NYBBLE                     
*                                                                               
         MVC   EKEYCLT,MVCLPNEW    REPLACE CLIENT CODE                          
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
         TITLE 'SPLDEXTMVE - MOVE BETWEEN AGENCIES - BUY'                       
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
*                                                                               
BUYKEYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    BUYKEYDN                                                         
*                                                                               
         CLC   BUYKAM,MVTAGYMD-MVTAB(R4)  MATCH ON AGENCY/MEDIA                 
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   BUYKCLT,MVCLPOLD            MATCH ON PACKED CLIENT               
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   BUYKEST,MVTEST-MVTAB(R4)    MATCH ON ESTIMATE                    
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
         OC    BUYKAM,MVAGNEW      ADD IN NEW AGENCY NYBBLE                     
*                                                                               
         MVC   BUYKCLT,MVCLPNEW    REPLACE CLIENT CODE                          
*                                                                               
         LA    R2,BUYMSTA          R2 POINTS TO MARKET CODE                     
*                                  R4 POINTS TO CONVERSION TABLE ENTRY          
*                                                                               
         MVI   SWITCH,C'B'         INDICATE BUY RECORD                          
         BRAS  RE,GETMKT           FIND NEW MARKET                              
         MVI   SWITCH,0            CLEAR SWITCH                                 
*                                                                               
         BRAS  RE,WRITE            WRITE RECORD TO OUTPUT DATASET               
*                                                                               
BUYKEYDN DS    0H                                                               
*                                                                               
BUYX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTMVE - MOVE BETWEEN AGENCIES - GOAL'                      
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
*                                                                               
GOALKYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    GOALKYDN                                                         
*                                                                               
         CLC   GKEYAM,MVTAGYMD-MVTAB(R4)  MATCH ON AGENCY/MEDIA                 
         BNE   GOALKYCN                                                         
*                                                                               
         CLC   GKEYCLT,MVCLPOLD           MATCH ON OLD PACKED CLIENT            
         BNE   GOALKYCN                                                         
*                                                                               
         CLC   GKEYEST,MVTEST-MVTAB(R4)    MATCH ON ESTIMATE                    
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
         OC    GKEYAM,MVAGNEW      ADD IN NEW AGECNY NYBBLE                     
*                                                                               
         MVC   GKEYCLT,MVCLPNEW    REPLACE CLIENT CODE                          
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
         TITLE 'STLDEXTMVE - MOVE BETWEEN AGENCIES - GETMKT'                    
***********************************************************************         
*                                                                     *         
*        FIND STATION IN CONVERSION TABLE                             *         
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
         GOTO1 VHEXOUT,DMCB,(R2),P+65,2,0,0  OLD MARKET                         
*                                                                               
GETMKT1  DS    0H                                                               
*                                                                               
         LA    R7,MVERECC          ESTABLISH BINSRCH TABLE WORKAREA             
         USING MVERECD,R7                                                       
*                                                                               
         XC    MVEREC(MVERECLQ),MVEREC INIT WORKAREA                            
*                                                                               
         MVC   MVEAMOLD,MVTAGYMD-MVTAB(R4) SET OLD AGENCY/MEDIAIN KEY           
         MVC   MVECLTPK,MVCLPOLD           SET OLD PACKED CLIENT IN KEY         
         MVC   MVEMKOLD,0(R2)              SET OLD MKT NUMBER    IN KEY         
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',MVEKEY)   FIND IN TABLE           
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETMKTER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETMKTX             SKIP IF NOT FOUND                            
*                                                                               
         MVC   0(2,R2),MVEMKNEW    SET NEW MARKET NUMBER                        
*                                                                               
         CLC   MVEMKOLD,=X'02BA'   IF CHICAGO                                   
         BNE   *+10                                                             
         CLC   2(3,R2),=X'C791A1'  AND WKFS-FM                                  
         BNE   *+10                                                             
         MVC   0(2,R2),=X'0285'       SWITCH MKT TO CINCINNATI                  
*                                                                               
         CLC   MVEMKOLD,=X'0299'   IF CHARLESTON                                
         BNE   *+10                                                             
         CLC   2(3,R2),=X'C476C1'  AND WBEE-FM                                  
         BNE   *+10                                                             
         MVC   0(2,R2),=X'0267'       SWITCH MKT TO CHICAGO                     
*                                                                               
         CLC   MVEMKOLD,=X'0C95'   IF SAN FRANCISCO                             
         BNE   *+10                                                             
         CLC   2(3,R2),=X'618141'  AND KYLZ-FM                                  
         BNE   *+10                                                             
         MVC   0(2,R2),=X'004B'       SWITCH MKT TO ALBUQUERQUE                 
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETMKTXX                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
         GOTO1 VHEXOUT,DMCB,(R2),P+72,2,0,0   NEW NUMBER                        
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
         GOTO1 VHEXOUT,DMCB,(R2),P+65,2,0,0  OLD NUMBER                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
         MVC   P+72(4),=C'****'               NEW NUMBER                        
*                                                                               
         EDIT  (B2,0(R2)),(4,P+78),0                                            
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
         TITLE 'STLDEXTMVE - MOVE BETWEEN AGENCIES - COPYREC'                   
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
         TITLE 'STLDEXTMVE - MOVE BETWEEN AGENCIES - WRITE'                     
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
FILEIN   DCB   DDNAME=TEMPIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXINDN                                                    
*                                                                               
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=25000                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTMVE - MOVE BETWEEN AGENCIES - WORKD'                     
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
TEMP     DS    CL(CNVRECLQ)        CONVERSION FILE INPUT AREA                   
MVERECC  DS    XL(MVERECLQ)        TABLE ENTRY BUILD AREA                       
TABADDR  DS    A                   A(BINSRCH TABLE)                             
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMD   DS    CL1                                                              
WAGYMD   DS    CL1                 MASTER AGY/MEDIA                             
PRINTSW  DC    XL1'00'             X'01' - PRINT TRACE                          
OLDKEY   DC    XL13'00'            OLD KEY SAVEAREA                             
NEWKEY   DC    XL13'00'            NEW KEY SAVEAREA                             
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
         TITLE 'STLDEXTMVE - MOVE BETWEEN AGENCIES - MVERECD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR NUMBER CONVERSION TABLE - DIFFERENT ORDER THAN     *         
*              ORIGINAL FILE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVERECD  DSECT                                                                  
MVEREC   DS    0XL1                CONVERSION RECORD                            
MVEKEY   DS    0XL1                KEY FOR TABLE                                
MVEAMOLD DS    XL1                 OLD AGENCY/MEDIA                             
MVECLTPK DS    XL2                 PACKED CLIENT                                
MVEMKOLD DS    XL2                 OLD MARKET NUMBER                            
MVEKEYLQ EQU   *-MVEKEY            KEY LENGTH                                   
*                                                                               
MVEAMNEW DS    XL1                 NEW AGENCY/MEDIA                             
MVECLT   DS    CL3                 OLD CLIENT                                   
MVEMKTNM DS    CL31                MARKET NAME                                  
MVEMKNEW DS    XL2                 NEW MARKET NUMBER                            
         DS    XL(80-(*-MVERECD))  SPARE                                        
MVERECLQ EQU   *-MVEREC            RECORD LENGTH                                
MVERMAXQ EQU   5000                MACXIMUM NUMBER OF RECORDS IN FILE           
*                                                                               
         TITLE 'STLDEXTMVE - MOVE BETWEEN AGENCIES - CNVRECD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR MARKET CONVERSION FILE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CNVRECD  DSECT                                                                  
CNVREC   DS    0XL1                CONVERSION RECORD                            
CNVMED   DS    CL1                 MEDIA                                        
         DS    XL1                 FIELD SEPARATOR                              
CNVCLT   DS    CL3                 CLIENT                                       
         DS    XL1                 FIELD SEPARATOR                              
CNVMKTNM DS    CL31                MARKET NAME                                  
         DS    XL1                 FIELD SEPARATOR                              
CNVMKOLD DS    CL4                 OLD MARKET NUMBER                            
         DS    XL1                 FIELD SEPARATOR                              
CNVMKNEW DS    CL4                 NEW MARKET NUMBER                            
         DS    XL1                 FIELD SEPARATOR                              
         DS    XL(80-(*-CNVRECD))  SPARE                                        
CNVRECLQ EQU   *-CNVREC            RECORD LENGTH                                
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
PAMOLD   DS    CL2                 AGENCY/MEDIA - OLD - HEX                     
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PAMNEW   DS    CL2                 AGENCY/MEDIA - NEW - HEX                     
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PCLTOLD  DS    CL3                 CLIENT - OLD                                 
         DS    CL2                                                              
PCLPOLD  DS    CL4                 PACKED CLIENT - OLD                          
         DS    CL2                                                              
PCLTNEW  DS    CL3                 CLIENT - NEW                                 
         DS    CL2                                                              
PCLPNEW  DS    CL4                 PACKED CLIENT - NEW                          
         DS    CL2                                                              
PMKTNM   DS    CL31                MARKET NAME                                  
         DS    CL2                                                              
POLDMK   DS    CL4                 MARKET NUMBER - OLD - HEX                    
         DS    CL1                                                              
POLDMKD  DS    CL4                 MARKET NUMBER - NEW - DECIMAL                
         DS    CL3                                                              
PNEWMK   DS    CL4                 MARKET NUMBER - NEW - HEX                    
         DS    CL1                                                              
PNEWMKD  DS    CL4                 MARKET NUMBER - NEW - DECIMAL                
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
PTMKTNM  DS    CL31'MARKET'        MARKET NAME                                  
         DS    CL2                                                              
PTOLDMK  DS    CL4'MKT'            MARKET NUMBER - OLD - HEX                    
         DS    CL1                                                              
PTOLDMKD DS    CL4'OLD'            MARKET NUMBER - NEW - DECIMAL                
         DS    CL3                                                              
PTNEWMK  DS    CL4'MKT'            MARKET NUMBER - NEW - HEX                    
         DS    CL1                                                              
PTNEWMKD DS    CL4'NEW'            MARKET NUMBER - NEW - DECIMAL                
         DS    CL2                                                              
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
**PAN#1  DC    CL21'050SPLDEXTMVE02/25/99'                                      
         END                                                                    
*                                                                               
