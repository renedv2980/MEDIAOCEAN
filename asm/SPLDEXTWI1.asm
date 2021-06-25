*          DATA SET SPLDEXTWI1 AT LEVEL 021 AS OF 05/14/99                      
*PHASE SPEXTWI1                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE BINSRCH2                                                               
*        TITLE 'SPLDEXT -SPTDIR/FIL LOAD/DUMP MODEL EXTERN'                     
         TITLE 'SPLDEXT -SPTDIR/FIL LOAD/DUMP MODEL EXTERN'                     
***********************************************************************         
*                                                                     *         
*        MOVE HEADERS,BUYS AND GOALS FROM ONE AGENCY TO ANOTHER       *         
*                                                                     *         
*        WIAPL TO WI                                                  *         
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
         L     RC,=A(WORKD)        ESTABLISH WORKING STORAGE                    
         USING WORKD,RC                                                         
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
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - DMXISTA'                   
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION - STATION TABLE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXISTA  DS    0H                                                               
*                                                                               
         OPEN  (FSTAFROM,(INPUT))    OPEN FROM AGENCY STATION FILE              
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FSTATO,(INPUT))      OPEN TO   AGENCY STATION FILE              
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ISSUE GETMAIN FOR STORAGE TO HOLD CONVERSION TABLE                     
*                                                                               
         LHI   R0,STANTRYL         RECORD LENGTH                                
         MHI   R0,STTRMAXQ         *MAXIMUM NUMBER OF RECORDS                   
*                                                                               
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R1,15,STATABA       SAVE A(GETMAIN AREA)                         
         LR    R3,R1               SAVE A(GETMAIN AREA)                         
         SR    R5,R5                                                            
*                                                                               
*        INITIALIZE BINSRCH PARAMETERS                                          
*                                                                               
         LA    R2,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R2          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         MVC   BSPATAB,STATABA     A(TABLE)                                     
*                                                                               
         LA    RF,STANTRYL         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         XC    BSPNOR,BSPNOR       INIT RECORD COUNTER                          
*                                                                               
         LA    RF,STTKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,STTKEY-STANTRY KEY DISPLACEMENT                          
*                                                                               
         LHI   RF,STTRMAXQ         SET # OF AVAILABLE ENTRIES                   
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
         LA    R7,STANTRYC         ESTABLISH TABLE RECORD                       
         USING STANTRYD,R7                                                      
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1+2(20),=CL20'CONVERSION FILE'  LABEL PRINT OUT               
*                                                                               
DMXINLP  DS    0H                                                               
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY   INIT TABLE ENTRY                     
*                                                                               
         GET   FSTAFROM,FRMREC      READ NEXT RECORD                            
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
         GET   FSTATO,TOREC        READ NEXT TO RECORD                          
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
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   STTAMOLD,FRMAGYMD   SET WITH FROM AGY/MED                        
         MVC   STTMSOLD,FRMMKSTA   SET WITH FROM MKT/STA                        
         MVC   STTMKOLD,FRMMKT     SET WITH FROM MARKET                         
         MVC   STTSTOLD,FRMCALL    SET WITH FROM STATION                        
         MVC   STTCLOLD,FRMCLT     SET WITH FROM CLIENT                         
         MVC   STTCPOLD,=X'FFFF'      FORCE HIGH                                
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PLINED,R4                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTAMOLD,PAMOLD,1,0,0 AGENCY/MEDIA - OLD            
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTMSOLD,POLDMSTA,5,0,0  MKT/STA - OLD              
*                                                                               
         MVC   POLDMK,FRMMKT       OLD MARKET NUMBER                            
*                                                                               
         MVC   POLDSTA,FRMCALL     OLD STATION                                  
*                                                                               
         MVC   PNEWMK(15),=C'UNKNOWN STATION'                                   
*                                                                               
         B     DMXINTF2            ADD TO TABLE                                 
*                                                                               
DMXINTFD DS    0H                                                               
*                                                                               
*        FORMAT TABLE RECORD                                                    
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   STTAMOLD,FRMAGYMD   SET WITH FROM AGY/MED                        
         MVC   STTMSOLD,FRMMKSTA   SET WITH FROM MKT/STA                        
         MVC   STTMKOLD,FRMMKT     SET WITH FROM MARKET                         
         MVC   STTSTOLD,FRMCALL    SET WITH FROM STATION                        
         MVC   STTCLOLD,FRMCLT     SET WITH FROM CLIENT                         
*                                                                               
         CLC   FRMCLT,=C'000'      IF MASTER STATION REC                        
         BNE   *+14                                                             
         MVC   STTCPOLD,=X'FFFF'      FORCE HIGH                                
         B     DMXINTF1                                                         
*                                                                               
         GOTO1 VCLPACK,DMCB,FRMCLT,STTCPOLD   PACK CLIENT                       
*                                                                               
DMXINTF1 DS    0H                                                               
*                                                                               
         MVC   STTAMNEW,TOAGYMD    SET WITH TO   AGY/MED                        
         MVC   STTMSNEW,TOMKSTA    SET WITH TO   MKT/STA                        
         MVC   STTMKNEW,TOMKT      SET WITH FROM MARKET                         
         MVC   STTSTNEW,TOCALL     SET WITH FROM STATION                        
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PLINED,R4                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTAMOLD,PAMOLD,1,0,0 AGENCY/MEDIA - OLD            
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTAMNEW,PAMNEW,1,0,0 AGENCY/MEDIA - NEW            
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTMSOLD,POLDMSTA,5,0,0  MKT/STA - OLD              
*                                                                               
         MVC   POLDMK,FRMMKT       OLD MARKET NUMBER                            
*                                                                               
         MVC   POLDSTA,FRMCALL     OLD STATION                                  
         MVC   POLDCLT,FRMCLT      OLD CLIENT                                   
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTMSNEW,PNEWMSTA,5,0,0  MKT/STA - NEW              
*                                                                               
         MVC   PNEWMK,TOMKT        NEW MARKET NUMBER                            
*                                                                               
         MVC   PNEWSTA,TOCALL      NEW STATION                                  
*                                                                               
DMXINTF2 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',STANTRY) ADD REC TO TABLE          
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FILLED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RF,15,BSPAREC                                                    
*                                                                               
         MVC   P+80(32),0(RF)      DISPLAY TABLE ENTRY                          
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
DMXINCN  DS    0H                                                               
*                                                                               
         B     DMXINLP                                                          
*                                                                               
DMXINDN  DS    0H                                                               
*                                                                               
         CLOSE FSTAFROM            CLOSE INPUT FILE                             
         CLOSE FSTATO              CLOSE INPUT FILE                             
*                                                                               
DMXISTAX DS    0H                                                               
*                                                                               
         TITLE 'SPLDEXTMVC - MOVE BETWEEN AGENCIES - DMXIBYR'                   
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION - BUYER TABLE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXIBYR  DS    0H                                                               
*                                                                               
         CLI   MVNWS,C'Y'          MOVE BUYSHEET IF ASKED                       
         BNE   DMXIBYRX                                                         
*                                                                               
         MVI   EOFTOSW,0           INIT EOF SWITCH                              
*                                                                               
         XC    FROMRECC,FROMRECC   INIT FILE READIN AREAS                       
         XC    TORECC,TORECC                                                    
*                                                                               
         OPEN  (FBYRFROM,(INPUT))    OPEN FROM AGENCY BUYER FILE                
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FBYRTO,(INPUT))      OPEN TO   AGENCY BUYER FILE                
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ISSUE GETMAIN FOR STORAGE TO HOLD CONVERSION TABLE                     
*                                                                               
         LHI   R0,BYRNTRYL         RECORD LENGTH                                
         MHI   R0,BYTRMAXQ         *MAXIMUM NUMBER OF RECORDS                   
*                                                                               
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R1,15,BYRTABA       SAVE A(GETMAIN AREA)                         
         LR    R3,R1               SAVE A(GETMAIN AREA)                         
         SR    R5,R5                                                            
*                                                                               
*        INITIALIZE BINSRCH PARAMETERS                                          
*                                                                               
         LA    R2,BSPBYR           POINT TO BUYER BINSRCH PARMS                 
         USING BSRPRMD,R2          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         MVC   BSPATAB,BYRTABA     A(TABLE)                                     
*                                                                               
         LA    RF,BYRNTRYL         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         XC    BSPNOR,BSPNOR       INIT RECORD COUNTER                          
*                                                                               
         LA    RF,BYTKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,BYTKEY-BYRNTRY KEY DISPLACEMENT                          
*                                                                               
         LHI   RF,BYTRMAXQ         SET # OF AVAILABLE ENTRIES                   
         ST    RF,BSPMAX                                                        
*                                                                               
*        FILL CONVERSION TABLE FROM INPUT FILE                                  
*                                                                               
         LA    R5,FROMRECC         ESTABLISH FROM AGENCY BUYER REC              
         USING BYRFRMD,R5                                                       
*                                                                               
         LA    R6,TORECC           ESTABLISH TO   AGENCY BUYER REC              
         USING BYRTOD,R6                                                        
*                                                                               
         LA    R7,BYRNTRYC         ESTABLISH TABLE ENTRY                        
         USING BYRNTRYD,R7                                                      
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1+2(20),=CL20'CONVERSION FILE'  LABEL PRINT OUT               
*                                                                               
DMXIBYLP DS    0H                                                               
*                                                                               
         GET   FBYRFROM,FROMRECC    READ NEXT RECORD                            
*                                                                               
*        COMPARE TO CURRENT TO RECORD                                           
*                                                                               
DMXIBY1L DS    0H                                                               
*                                                                               
         CLC   BFRKTYP,BTOKTYP     RECORD TYPES MUST MATCH                      
         BH    DMXIBY1C                                                         
         BL    DMXIBY1D                                                         
*                                                                               
         CLC   BFRKSUB,BTOKSUB     RECORD SUB-TYPES MUST MATCH                  
         BH    DMXIBY1C                                                         
         BL    DMXIBY1D                                                         
*                                                                               
         MVC   HALF(1),BFRKAM      DROP AGENCY CODE                             
         NI    HALF,X'0F'            TO GET MEDIA                               
         MVC   HALF+1(1),BTOKAM                                                 
         NI    HALF+1,X'0F'                                                     
*                                                                               
         CLC   HALF(1),HALF+1      MEDIA MUST MATCH                             
         BH    DMXIBY1C                                                         
         BL    DMXIBY1D                                                         
*                                                                               
         CLC   BFRKBYR,BTOKBYR     BUYERS MUST MATCH                            
         BH    DMXIBY1C                                                         
         BL    DMXIBY1D                                                         
*                                                                               
         B     DMXIBY1F            MATCH FOUND                                  
*                                                                               
DMXIBY1C DS    0H                                                               
*                                                                               
         CLI   EOFTOSW,0           SKIP IF END OF FILE WAS REACHED              
         BNE   DMXIBY1D                                                         
*                                                                               
         GET   FBYRTO,TORECC       READ NEXT TO RECORD                          
*                                                                               
         B     DMXIBY1L                                                         
*                                                                               
DMXIBYC1 DS    0H                                                               
*                                                                               
         MVI   EOFTOSW,X'FF'       INDICATE TO FILE EXHAUSTED                   
*                                                                               
DMXIBY1D DS    0H                  NO MATCH                                     
*                                                                               
*        BUYER NOT ON RECEIVING AGENCY FILE                                     
*        DROP BUYER FROM TABLE                                                  
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   BYTAMOLD,BFRKAM     SET WITH FROM AGY/MED                        
         MVC   BYTBCOLD,BFRBYRCD   SET WITH FROM BUYER CODE                     
         MVC   BYTBYOLD,BFRKBYR    SET WITH FROM BUYER ID                       
         MVC   BYTBNOLD,BFRBYRNM   SET WITH FROM BUYER NAME                     
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PBLINED,R4                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,BYTAMOLD,PBAMOLD,1,0,0 AGENCY/MEDIA - OLD           
*                                                                               
         EDIT  (B1,BYTBCOLD),(3,PBOLDBC)   OLD BUYER CODE                       
*                                                                               
         MVC   PBOLDBYR,BYTBYOLD   OLD BUYER                                    
         MVC   PBOLDBN,BYTBNOLD    OLD BUYER NAME                               
*                                                                               
         MVC   PBNEWBN,=CL20'UNKNOWN BUYER'                                     
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIBYCN                                                         
*                                                                               
DMXIBY1F DS    0H                                                               
*                                                                               
*        FORMAT TABLE RECORD                                                    
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   BYTAMOLD,BFRKAM     SET WITH FROM AGY/MED                        
         MVC   BYTBCOLD,BFRBYRCD   SET WITH FROM BUYER CODE                     
         MVC   BYTBYOLD,BFRKBYR    SET WITH FROM BUYER ID                       
         MVC   BYTBNOLD,BFRBYRNM   SET WITH FROM BUYER NAME                     
*                                                                               
         MVC   BYTAMNEW,BTOKAM     SET WITH TO   AGY/MED                        
         MVC   BYTBCNEW,BTOBYRCD   SET WITH TO   BUYER CODE                     
         MVC   BYTBYNEW,BTOKBYR    SET WITH TO   BUYER ID                       
         MVC   BYTBNNEW,BTOBYRNM   SET WITH TO   BUYER NAME                     
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PBLINED,R4                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,BYTAMOLD,PBAMOLD,1,0,0 AGENCY/MEDIA - OLD           
*                                                                               
         EDIT  (B1,BYTBCOLD),(3,PBOLDBC)   OLD BUYER CODE                       
*                                                                               
         MVC   PBOLDBYR,BYTBYOLD   OLD BUYER                                    
         MVC   PBOLDBN,BYTBNOLD    OLD BUYER NAME                               
*                                                                               
         MVC   PBNEWBN,=CL20'UNKNOWN BUYER'                                     
*                                                                               
         GOTO1 VHEXOUT,DMCB,BYTAMNEW,PBAMNEW,1,0,0 AGENCY/MEDIA - NEW           
*                                                                               
         EDIT  (B1,BYTBCNEW),(3,PBNEWBC)   NEW BUYER CODE                       
*                                                                               
         MVC   PBNEWBYR,BYTBYNEW   NEW BUYER                                    
         MVC   PBNEWBN,BYTBNNEW    NEW BUYER NAME                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',BYRNTRY) ADD REC TO TABLE          
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FILLED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RF,15,BSPAREC                                                    
*                                                                               
         MVC   P+80(BYRNTRYL),0(RF)      DISPLAY TABLE ENTRY                    
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
DMXIBYCN DS    0H                                                               
*                                                                               
         B     DMXIBYLP                                                         
*                                                                               
DMXIBYDN DS    0H                                                               
*                                                                               
         CLOSE FSTAFROM            CLOSE INPUT FILE                             
         CLOSE FSTATO              CLOSE INPUT FILE                             
*                                                                               
DMXIBYRX DS    0H                                                               
*                                                                               
*        PRINT COLUMN TITLES                                                    
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
*****                                                                           
*****    SP    RECCTR,=P'1'        DECREMENT RECORD COUNTER                     
*****    BNZ   DMXREC1                                                          
*****                                                                           
*****    ZAP   RECCTR,=P'100'      RESET COUNTER                                
*****                                                                           
*****    L     R3,AREC                                                          
*****                                                                           
*****    GOTO1 VHEXOUT,DMCB,(R3),P+2,13,0,0  PRINT OLD KEY                      
*****                                                                           
*****    GOTO1 VPRINTER                                                         
*                                                                               
DMXREC1  DS    0H                                                               
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
RECCTR   DC    PL2'100'            RECORD COUNTER                               
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
         OC    EKEYPRD(9),EKEYPRD  DROP IF CLIENT HEADER                        
         BZ    HDRKEYCN                                                         
*                                                                               
         OC    EKEYEST(6),EKEYEST  DROP IF PRODUCT HEADER                       
         BZ    HDRKEYCN                                                         
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
         CLI   MVGOALS,C'Y'        SKIP IF GOALS NOT WANTED                     
         BNE   GOALX                                                            
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
         DROP  R2                                                               
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
         LA    R7,STAEXCPS         POINT TO STATION EXCEPTIONS TABLE            
         USING STANTRYD,R7         ESTABLISH AS ENTRY IN STATION TABLE          
*                                                                               
GSTEXCLP DS    0H                                                               
*                                                                               
         CLI   STTAMOLD,X'00'      DONE AT END OF TABLE                         
         BE    GSTEXCDN                                                         
*                                                                               
         CLC   MVTAGYMD,STTAMOLD   MATCH ON AGENCY/MEDIA                        
         BNE   GSTEXCCN                                                         
*                                                                               
         CLC   STTMSOLD,0(R2)      MATCH ON INCOMING MKT/STATION                
         BE    GSTEXCFD                                                         
*                                                                               
GSTEXCCN DS    0H                                                               
*                                                                               
         LA    R7,STANTRYL(R7)     BUMP TO NEXT TABLE ENTRY                     
         B     GSTEXCLP                                                         
*                                                                               
GSTEXCFD DS    0H                                                               
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
         ZAP   CTR,=P'10'          FORCE TRACE                                  
*                                                                               
         B     GSTCLTFD                                                         
*                                                                               
GSTEXCDN DS    0H                                                               
*                                                                               
         LA    R3,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R3          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETSTA1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
GETSTA1  DS    0H                                                               
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY INIT WORKAREA                          
*                                                                               
         MVC   STTAMOLD,MVTAGYMD   SET OLD AGY/MD  IN KEY                       
         MVC   STTMSOLD,0(R2)      SET OLD MKT/STA IN KEY                       
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',STTKEY)   FIND IN TABLE           
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETSTAER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETSTAER            SKIP IF NOT FOUND                            
*                                                                               
GSTCLTLP DS    0H                                                               
*                                                                               
         CLC   STTAMOLD,MVTAGYMD   MUST MATCH AGY/MED                           
         BNE   GETSTAER                                                         
*                                                                               
         CLC   STTMSOLD,0(R2)      MUST MATCH MKT/STA                           
         BNE   GETSTAER                                                         
*                                                                               
         CLC   STTCPOLD,=X'FFFF'   USE IF MASTER STATION                        
         BE    GSTCLTFD                                                         
*                                                                               
         CLC   STTCPOLD,WRKCLT     USE IF CLIENT MATCHES                        
         BE    GSTCLTFD                                                         
*                                                                               
GSTCLTCN DS    0H                                                               
*                                                                               
         LA    R7,STANTRYL(R7)     BUMP TO NEXT TABLE ENTRY                     
         B     GSTCLTLP                                                         
*                                                                               
GSTCLTFD DS    0H                                                               
*                                                                               
         OC    STTMSNEW,STTMSNEW   ERROR IF NO NEW MKT/STA                      
         BZ    GETSTAE1                                                         
*                                                                               
         MVC   0(5,R2),STTMSNEW    SET NEW MARKET/STATION                       
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETSTAXX                                                         
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
         MVC   P+70(5),STTSTOLD       OLD STATION                               
         MVC   P+76(3),STTCLOLD       OLD CLIENT                                
*                                                                               
         LA    R5,NEWREC           POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+80(4),STTMKNEW       NEW MARKET                                
         MVC   P+85(5),STTSTNEW       NEW STATION                               
*                                                                               
GETSTAX  DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     GETSTAXX                                                         
*                                                                               
GETSTAER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
GETSTAE1 DS   0H                   HAVE STATION TABLE ENTRY                     
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
         MVC   P+70(5),STTSTOLD       OLD STATION                               
         MVC   P+76(3),STTCLOLD       OLD CLIENT                                
*                                                                               
         LA    R5,NEWREC           POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+32,13,0,0  PRINT NEW KEY                     
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
         DROP  R3                                                               
*                                                                               
*        TABLE OF MKT/STATION EXCEPTIONS                                        
*        SAME FORMAT AS STATION TABLE                                           
*                                                                               
STAEXCPS DS    0D                  EXCEPTION STATIONS                           
         DC    XL1'92'             OLD AGENCY/MEDIA                             
         DC    XL5'00E8C68A61'     OLD MARKET/STATION                           
         DC    XL2'AC01'           OLD CLIENT - PACKED                          
         DC    CL4'0232'           OLD MARKET NUMBER                            
         DC    CL5'WHFSF'          OLD STATION                                  
         DC    CL3'LAB'            OLD CLIENT                                   
         DC    XL1'12'             NEW AGENCY/MEDIA                             
         DC    XL5'00E8C68A61'     NEW MARKET/STATION                           
         DC    CL4'0232'           NEW MARKET NUMBER                            
         DC    CL5'WHFSF'          NEW STATION                                  
*                                                                               
         DC    XL1'91'             OLD AGENCY/MEDIA                             
         DC    XL5'024FFDAD19'     OLD MARKET/STATION                           
         DC    XL2'AC01'           OLD CLIENT - PACKED                          
         DC    CL4'0591'           OLD MARKET NUMBER                            
         DC    CL5'02FSS'          OLD STATION                                  
         DC    CL3'LAB'            OLD CLIENT                                   
         DC    XL1'11'             NEW AGENCY/MEDIA                             
         DC    XL5'024FFDCF00'     NEW MARKET/STATION                           
         DC    CL4'0591'           NEW MARKET NUMBER                            
         DC    CL5'70FSS'          NEW STATION                                  
*                                                                               
         DC    XL1'91'             OLD AGENCY/MEDIA                             
         DC    XL5'01CDFDAC99'     OLD MARKET/STATION                           
         DC    XL2'AC01'           OLD CLIENT - PACKED                          
         DC    CL4'0461'           OLD MARKET NUMBER                            
         DC    CL5'01FSC'          OLD STATION                                  
         DC    CL3'LAB'            OLD CLIENT                                   
         DC    XL1'11'             NEW AGENCY/MEDIA                             
         DC    XL5'01CDFDCE80'     NEW MARKET/STATION                           
         DC    CL4'0461'           NEW MARKET NUMBER                            
         DC    CL5'69FSC'          NEW STATION                                  
*                                                                               
         DC    XL1'91'             OLD AGENCY/MEDIA                             
         DC    XL5'0141FDAC19'     OLD MARKET/STATION                           
         DC    XL2'AC01'           OLD CLIENT - PACKED                          
         DC    CL4'0321'           OLD MARKET NUMBER                            
         DC    CL5'00NES'          OLD STATION                                  
         DC    CL3'LAB'            OLD CLIENT                                   
         DC    XL1'11'             NEW AGENCY/MEDIA                             
         DC    XL5'0141FDCE00'     NEW MARKET/STATION                           
         DC    CL4'0321'           NEW MARKET NUMBER                            
         DC    CL5'68NES'          NEW STATION                                  
*                                                                               
         DC    XL1'91'             OLD AGENCY/MEDIA                             
         DC    XL5'028BFDC419'     OLD MARKET/STATION                           
         DC    XL2'AC01'           OLD CLIENT - PACKED                          
         DC    CL4'0651'           OLD MARKET NUMBER                            
         DC    CL5'48FSN'          OLD STATION                                  
         DC    CL3'LAB'            OLD CLIENT                                   
         DC    XL1'11'             NEW AGENCY/MEDIA                             
         DC    XL5'028BFDC400'     NEW MARKET/STATION                           
         DC    CL4'0651'           NEW MARKET NUMBER                            
         DC    CL5'48FSN'          NEW STATION                                  
*                                                                               
         DC    XL1'92'             OLD AGENCY/MEDIA                             
         DC    XL5'0029C98921'     OLD MARKET/STATION                           
         DC    XL2'AC01'           OLD CLIENT - PACKED                          
         DC    CL4'0041'           OLD MARKET NUMBER                            
         DC    CL5'WPYXF'          OLD STATION                                  
         DC    CL3'LAB'            OLD CLIENT                                   
         DC    XL1'12'             NEW AGENCY/MEDIA                             
         DC    XL5'0029C98921'     NEW MARKET/STATION                           
         DC    CL4'0041'           NEW MARKET NUMBER                            
         DC    CL5'WPYXF'          NEW STATION                                  
*                                                                               
         DC    XL1'92'             OLD AGENCY/MEDIA                             
         DC    XL5'0201C4AC01'     OLD MARKET/STATION                           
         DC    XL2'AC01'           OLD CLIENT - PACKED                          
         DC    CL4'0513'           OLD MARKET NUMBER                            
         DC    CL5'WBTZF'          OLD STATION                                  
         DC    CL3'LAB'            OLD CLIENT                                   
         DC    XL1'12'             NEW AGENCY/MEDIA                             
         DC    XL5'0201C4AC01'     NEW MARKET/STATION                           
         DC    CL4'0513'           NEW MARKET NUMBER                            
         DC    CL5'WBTZF'          NEW STATION                                  
*                                                                               
         DC    X'00'               EOT                                          
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
         LA    R3,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R3          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETMKT1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
GETMKT1  DS    0H                                                               
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY INIT WORKAREA                          
*                                                                               
         MVC   STTAMOLD,MVTAGYMD   SET OLD AGY/MED IN KEY                       
         MVC   STTMSOLD(2),0(R2)   SET OLD MKT     IN KEY                       
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',STTKEY)   FIND IN TABLE           
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETMKTER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETMKTER            SKIP IF NOT FOUND                            
*                                                                               
         OC    STTMSNEW,STTMSNEW   ERROR IF NO NEW MKT/STA                      
         BZ    GETMKTER                                                         
*                                                                               
         CLC   STTMSOLD(2),0(R2)   FIND MARKET MATCH                            
         BNE   GETMKTER                                                         
*                                                                               
         MVC   0(2,R2),STTMSNEW    SET NEW MARKET                               
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETMKTXX                                                         
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
*                                                                               
         LA    R5,NEWREC                                                        
         GOTO1 VHEXOUT,DMCB,(R5),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+80(4),STTMKNEW       NEW MARKET                                
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
         LA    R5,NEWREC                                                        
         GOTO1 VHEXOUT,DMCB,(R5),P+32,13,0,0  PRINT NEW KEY                     
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
         DROP  R3                                                               
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
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVTAB'                     
***********************************************************************         
*                                                                     *         
*        DATA FOR THIS CONVERSION                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVAGYOLD DC    C'PU'               OLD AGENCY - WIAPL                           
MVAGOLDP DC    X'90'               OLD AGENCY - WIAPL  - PACKED                 
*                                                                               
MVAGYNEW DC    C'WI'               NEW AGENCY - WI                              
MVAGNEWP DC    X'10'               NEW AGENCY - WI     - PACKED                 
*                                                                               
*        RECORD TYPES TO MOVE                                                   
*                                                                               
MVBUYS   DC    C'Y'                MOVE BUYS             - Y/N                  
MVGOALS  DC    C'N'                MOVE GOALS            - Y/N                  
MVNWS    DC    C'N'                MOVE BUYERS WORKSHEET - Y/N                  
*                                                                               
*        TABLE OF WHAT TO MOVE                                                  
******                                                                          
MVTAB    DS    0X                  TABLE ENTRY                                  
         DC    X'92',C'LAB',X'0000',X'000000',AL1(154)                          
         DC    X'92',C'LAB',X'0000',X'000000',AL1(155)                          
         DC    X'92',C'LAB',X'0000',X'000000',AL1(112)                          
         DC    X'92',C'MSK',X'0000',X'000000',AL1(014)                          
         DC    X'92',C'MSK',X'0000',X'000000',AL1(015)                          
         DC    X'92',C'UPS',X'0000',X'000000',AL1(165)                          
         DC    X'92',C'UPS',X'0000',X'000000',AL1(166)                          
         DC    X'92',C'UPS',X'0000',X'000000',AL1(173)                          
         DC    X'92',C'UPS',X'0000',X'000000',AL1(174)                          
         DC    X'92',C'UPS',X'0000',X'000000',AL1(179)                          
*                                                                               
         DC    X'91',C'LAB',X'0000',X'000000',AL1(123)                          
         DC    X'91',C'LAB',X'0000',X'000000',AL1(124)                          
         DC    X'91',C'UPS',X'0000',X'000000',AL1(160)                          
         DC    X'91',C'UPS',X'0000',X'000000',AL1(161)                          
*                                                                               
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(119)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(120)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(147)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(103)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(104)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(135)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(136)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(131)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(087)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(087)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(088)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(139)                          
******   DC    X'92',C'LAB',X'0000',X'000000',AL1(111)                          
******   DC    X'92',C'UPS',X'0000',X'000000',AL1(163)                          
******                                                                          
******   DC    X'91',C'LAB',X'0000',X'000000',AL1(137)                          
******   DC    X'91',C'LAB',X'0000',X'000000',AL1(107)                          
******   DC    X'91',C'LAB',X'0000',X'000000',AL1(108)                          
******   DC    X'91',C'LAB',X'0000',X'000000',AL1(127)                          
******   DC    X'91',C'LAB',X'0000',X'000000',AL1(091)                          
******   DC    X'91',C'UPS',X'0000',X'000000',AL1(155)                          
******   DC    X'91',C'UPS',X'0000',X'000000',AL1(156)                          
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - WORKD'                     
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DS    0D                                                               
*                                                                               
*        STATION FILES                                                          
*                                                                               
FSTAFROM DCB   DDNAME=FSTAFROM,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,    X        
               EODAD=DMXINDN                                                    
*                                                                               
FSTATO   DCB   DDNAME=FSTATO,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXINTC1                                                   
*                                                                               
*        BUYER   FILES                                                          
*                                                                               
FBYRFROM DCB   DDNAME=FBYRFROM,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,    X        
               EODAD=DMXIBYDN                                                   
*                                                                               
FBYRTO   DCB   DDNAME=FBYRTO,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXIBYC1                                                   
*                                                                               
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=25000                                                    
*                                                                               
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
*                                                                               
STANTRYC DS    XL(STANTRYL)        STATION TABLE ENTRY BUILD AREA               
STATABA  DS    A                   A(STATION BINSRCH TABLE)                     
*                                                                               
BYRNTRYC DS    XL(BYRNTRYL)        BUYER   TABLE ENTRY BUILD AREA               
BYRTABA  DS    A                   A(BUYER   BINSRCH TABLE)                     
*                                                                               
DUB      DS    D                                                                
HALF     DS    H                                                                
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
BSPSTA   DS    XL(BSPPRML)         STATION TABLE BINSRCH PARMS                  
         DS    0A                  ALIGNMENT                                    
BSPBYR   DS    XL(BSPPRML)         BUYER   TABLE BINSRCH PARMS                  
*                                                                               
RRLEN    DS    XL2                 OUTPUT RECORD LENGTH                         
         DS    XL2                 SPARE                                        
NEWREC   DS    XL4096              NEW RECORD BUILD AREA                        
*                                                                               
WORKLQ   EQU   *-WORKD             LENGTH OF WORKING STORAGE                    
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BSRPRMD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR BINSRCH PARAMETERES                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BSRPRMD  DSECT                                                                  
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVTABD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DATA TO MOVE                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVTABD   DSECT                                                                  
MVTAGYMD DS    XL1                 AGENCY/MEDIA                                 
MVTCLT   DS    CL3                 CLIENT                                       
MVTCLTPK DS    XL2                 CLIENT - PACKED                              
MVTPRD   DS    XL3                 PRODUCT  - ALL - NULLS MEANS ALL             
MVTEST   DS    AL1                 ESTIMATE - ALL - NULLS MEANS ALL             
MVTABL   EQU   *-MVTABD            TABLE ENTRY LENGTH                           
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - STATABD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR STATION CONVERSION TABLE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STANTRYD DSECT                                                                  
STANTRY  DS    0XL1                CONVERSION RECORD                            
STTKEY   DS    0XL1                KEY FOR TABLE                                
STTAMOLD DS    XL1                 OLD AGENCY/MEDIA                             
STTMSOLD DS    XL5                 OLD MARKET/STATION                           
STTCPOLD DS    CL2                 OLD CLIENT - PACKED                          
STTKEYLQ EQU   *-STTKEY            KEY LENGTH                                   
*                                                                               
STTMKOLD DS    CL4                 OLD MARKET NUMBER                            
STTSTOLD DS    CL5                 OLD STATION                                  
STTCLOLD DS    CL3                 OLD CLIENT                                   
STTAMNEW DS    XL1                 NEW AGENCY/MEDIA                             
STTMSNEW DS    XL5                 NEW MARKET/STATION                           
STTMKNEW DS    CL4                 NEW MARKET NUMBER                            
STTSTNEW DS    CL5                 NEW STATION                                  
STANTRYL EQU   *-STANTRY           RECORD LENGTH                                
*                                                                               
STTRMAXQ EQU   20000               MAXIMUM NUMBER OF RECORDS IN FILE            
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
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRTABD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR BUYER   CONVERSION TABLE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRNTRYD DSECT                                                                  
BYRNTRY  DS    0XL1                CONVERSION RECORD                            
BYTKEY   DS    0XL1                KEY FOR TABLE                                
BYTAMOLD DS    XL1                 OLD AGENCY/MEDIA                             
BYTBCOLD DS    XL1                 OLD INTERNAL BUYER CODE                      
BYTKEYLQ EQU   *-BYTKEY            KEY LENGTH                                   
*                                                                               
BYTBYOLD DS    CL3                 OLD BUYER ID                                 
BYTBNOLD DS    CL20                OLD BUYER NAME                               
BYTAMNEW DS    XL1                 NEW AGENCY/MEDIA                             
BYTBYNEW DS    XL3                 NEW BUYER ID                                 
BYTBCNEW DS    XL1                 NEW INTERNAL BUYER CODE                      
BYTBNNEW DS    CL20                NEW BUYER NAME                               
BYRNTRYL EQU   *-BYRNTRY           RECORD LENGTH                                
*                                                                               
BYTRMAXQ EQU   600                 MAXIMUM NUMBER OF RECORDS IN FILE            
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRFRMD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR FROM BUYER CONVERSION RECORD                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRFRMD  DSECT                                                                  
BYRFRM   DS    0XL1                CONVERSION RECORD                            
BFRKEY   DS    0XL13               BUYER RECORD KEY                             
BFRKTYP  DS    XL1'0D'             STATION RECORD TYPE                          
BFRKSUB  DS    XL1'65'             STATION RECORD SUB TYPE                      
BFRKAM   DS    CL1                 AGENCY/MEDIA                                 
BFRKBYR  DS    CL3                 BUYER ID                                     
         ORG   BFRKEY+L'BFRKEY                                                  
*                                                                               
*                                                                               
BFRBYRCD DS    XL1                 BUYER CODE                                   
BFRBYRNM DS    CL20                BUYER NAME                                   
BFRAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-BYRFRM))   SPARE                                        
BFRRECLQ EQU   *-BYRFRM            RECORD LENGTH                                
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRTOD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TO   BUYER          CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRTOD   DSECT                                                                  
BYRTO    DS    0XL1                CONVERSION RECORD                            
BTOKEY   DS    0XL13               BUYER RECORD KEY                             
BTOKTYP  DS    XL1'0D'             STATION RECORD TYPE                          
BTOKSUB  DS    XL1'65'             STATION RECORD SUB TYPE                      
BTOKAM   DS    CL1                 AGENCY/MEDIA                                 
BTOKBYR  DS    CL3                 BUYER ID                                     
         ORG   BTOKEY+L'BTOKEY                                                  
*                                                                               
*                                                                               
BTOBYRCD DS    XL1                 BUYER CODE                                   
BTOBYRNM DS    CL20                BUYER NAME                                   
BTOAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-BYRTOD))   SPARE                                        
BTORECLQ EQU   *-BYRTO             RECORD LENGTH                                
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
*                                                                               
PBLINED  DSECT                     BUYER PRINT LINE                             
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PBAMOLD  DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL1                                                              
PBOLDBYR DS    CL3                 BUYER ID       - OLD                         
         DS    CL2                                                              
PBOLDBC  DS    CL3                 BUYER CODE     - OLD - DECIMAL               
         DS    CL2                                                              
PBOLDBN  DS    CL20                BUYER NAME     - OLD                         
         DS    CL2                                                              
PBAMNEW  DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PBNEWBYR DS    CL3                 BUYER ID       - NEW                         
         DS    CL2                                                              
PBNEWBC  DS    CL3                 BUYER CODE     - NEW - DECIMAL               
         DS    CL2                                                              
PBNEWBN  DS    CL20                BUYER NAME     - NEW                         
         DS    CL2                                                              
* DSECT FOR TITLE PRINT LINE                                                    
PTLINEBD  DSECT                     BUYER PRINT LINE                            
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PBTAMOLD DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL1                                                              
PBTOLDBY DS    CL3                 BUYER ID       - OLD                         
         DS    CL2                                                              
PBTOLDBC DS    CL3                 BUYER CODE     - OLD - DECIMAL               
         DS    CL2                                                              
PBTOLDBN DS    CL20                BUYER NAME     - OLD                         
         DS    CL2                                                              
PBTAMNEW DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PBTNEWBY DS    CL3                 BUYER ID       - NEW                         
         DS    CL2                                                              
PBTNEWBC DS    CL3                 BUYER CODE     - NEW - DECIMAL               
         DS    CL2                                                              
PBTNEWBN DS    CL20                BUYER NAME     - NEW                         
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
**PAN#1  DC    CL21'021SPLDEXTWI105/14/99'                                      
         END                                                                    
*                                                                               
