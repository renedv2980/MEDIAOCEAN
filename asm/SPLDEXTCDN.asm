*          DATA SET SPLDEXTCDN AT LEVEL 019 AS OF 02/18/99                      
*PHASE SPEXTCDN                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*        TITLE 'SPLDEXT -XSPDIR/FIL LOAD/DUMP MODEL EXTERN'                     
         TITLE 'SPLDEXT -XSPDIR/FIL LOAD/DUMP MODEL EXTERN'                     
***********************************************************************         
*                                                                     *         
*        RENUMBER CANADIAN STATIONS                                   *         
*                                                                     *         
*        DO NOT DELETE                                                *         
*        SAVE FOR POSTERITY                                           *         
*                                                                     *         
*        CANADIAN STATIONS ARE REPRESENTED BY 2 BYTE NUMBERS          *         
*        THIS EXTRACT CHANGES THE NUMBERS BASED ON INPUT FILE         *         
*        THIS MODULE RESTRICTED TO SPTDIR/FIL                         *         
*                                                                     *         
*        THIS MODULE IS PART OF A SET TO COVER ALL SPOT RECORDS       *         
*                                                                     *         
*        STREPFXCDN - BUILDS LIST OF OLD AND NEW STATION NUMBERS      *         
*        SPLDEXTCDN - SPTDIR/FIL RECORDS                              *         
*        SXLDEXTCDN - XSPDIR/FIL RECORDS                              *         
*        STLDEXTCDN - TRFDIR/FIL RECORDS                              *         
*        STLDCDN    - STAFIL     RECORDS                              *         
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
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - INIT'                 
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
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - DMCTL'                
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
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - DMXINIT'              
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
*                                                                               
         OPEN  (FILEIN,(INPUT))    OPEN CONVERSION FILE                         
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ISSUE GETMAIN FOR STORAGE TO HOLD CONVERSION TABLE                     
*                                                                               
         LHI   R0,CDNRECLQ         RECORD LENGTH                                
         MHI   R0,CDNRMAXQ         * MAXIMUM NUBER OF RECORDS                   
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
         LA    RF,CDNRECLQ         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         XC    BSPNOR,BSPNOR       INIT RECORD COUNTER                          
*                                                                               
         LA    RF,CDNKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,CDNKEY-CDNREC KEY DISPLACEMENT                           
*                                                                               
         LHI   RF,CDNRMAXQ         SET # OF AVAILABLE ENTRIES                   
         ST    RF,BSPMAX                                                        
*                                                                               
*        FILL CONVERSION TABLE FROM INPUT FILE                                  
*                                                                               
         LA    R5,TEMP             ESTABLISH INPUT RECORD                       
         USING CNVRECD,R5                                                       
*                                                                               
         LA    R7,CDNRECC          ESTABLISH TABLE RECORD                       
         USING CDNRECD,R7                                                       
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1+2(20),=CL20'CONVERSION FILE'  LABEL PRINT OUT               
         MVC   MID2+1(30),=CL30'AGY AM  STATN  OLD   NEW'                       
*                                                                               
         MVI   WAGYMD,0            INIT MASTER AGY/MEDIA                        
*                                                                               
DMXINLP  DS    0H                                                               
*                                                                               
         GET   FILEIN,TEMP         READ NEXT RECORD                             
*                                                                               
*        FORMAT TABLE RECORD                                                    
*                                                                               
         MVC   CDNAGYMD,CNVAGYMD   AGENCY/MEDIA                                 
         MVC   CDNNEWNM,CNVNEWNM   NEW SEQUENCE NUMBER                          
         MVC   CDNSTA,CNVSTA       STATION                                      
         MVC   CDNAGY,CNVAGY       AGENCY                                       
         MVC   CDNOLDNM,CNVOLDNM   OLD SEQUENCE NUMBER                          
*                                                                               
         CLI   WAGYMD,0            IF FIRST TIME SAVE AGY/MEDIA                 
         BNE   *+10                                                             
         MVC   WAGYMD,CNVAGYMD                                                  
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         MVC   P+02(2),CDNAGY                   AGENCY                          
         GOTO1 VHEXOUT,DMCB,CDNAGYMD,P+5,1,0,0  AGENCY/MEDIA                    
         MVC   P+09(5),CDNSTA                   STATION                         
         GOTO1 VHEXOUT,DMCB,CDNOLDNM,P+16,2,0,0 OLD STATION NUMBER              
         GOTO1 VHEXOUT,DMCB,CDNNEWNM,P+22,2,0,0 NEW STATION NUMBER              
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',CDNREC) ADD REC TO TABLE           
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
         B     DMXIT                                                            
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - DMXREC'               
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
         CLI   0(R3),X'10'         TEST BUYREC                                  
         BH    BUY                                                              
*                                                                               
         CLI   0(R3),X'0B'         OLD INVOICE RECORD                           
         BE    INV                                                              
*                                                                               
         CLI   0(R3),X'0C'         ESTIMATE BOOK INVENTORY RECORDS              
         BE    SIR                                                              
*                                                                               
         CLC   =X'0D17',0(R3)      NTWK PROGRAM - CANADA                        
         BE    NDOV                                                             
*                                                                               
         CLC   =X'0D34',0(R3)      DARE ORDER                                   
         BE    DO                                                               
*                                                                               
         CLC   =X'0D49',0(R3)      CSO PROGRAM                                  
         BE    CSO                                                              
*                                                                               
         CLC   =X'0D4E',0(R3)      CHILD CONTRACT RECORD                        
         BE    CNT                                                              
*                                                                               
         CLC   =X'0D57',0(R3)      CHILD SPOT DEMO OVERRIDE                     
         BE    DEM                                                              
*                                                                               
         CLC   =X'0D58',0(R3)      CHILD SPOT HEAD/FOOTLINE COMMENTS            
         BE    COM                                                              
*                                                                               
         CLC   =X'0D69',0(R3)      NEW BUYERS WORKSHEET COMMENT RECORD          
         BE    NCOM                                                             
*                                                                               
         CLC   =X'0D70',0(R3)      PRD EXCLUSION RECORD                         
         BE    PXC                                                              
*                                                                               
         CLC   =X'0D71',0(R3)      STATUS RECORD                                
         BE    STK                                                              
*                                                                               
         CLC   =X'0D72',0(R3)      STATION LOCKIN HEADER                        
         BE    SLH                                                              
*                                                                               
         CLC   =X'0D76',0(R3)      CLEARED STATUS RECORD                        
         BE    CLS                                                              
*                                                                               
         CLC   =X'0D78',0(R3)      UPLOAD RECORD                                
         BE    SPUP                                                             
*                                                                               
         CLC   =X'0D79',0(R3)      INFOMERCIAL                                  
         BE    INFO                                                             
*                                                                               
         CLC   =X'0D7A',0(R3)      WIPW                                         
         BE    PW                                                               
*                                                                               
         CLC   =X'0D7B',0(R3)      DOUBLE BOOK RECORDS                          
         BE    DBL                                                              
*                                                                               
         CLC   =X'0D7E',0(R3)      CTA MAINT                                    
         BE    CTA                                                              
*                                                                               
         CLC   =X'0E01',0(R3)      STATION BILL RECORD                          
         BE    STAB                                                             
*                                                                               
         CLC   =X'0E03',0(R3)      STATION INVOICE RECORD                       
         BE    SNV                                                              
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - BUY'                  
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
         MVC   BAGYMD,BUYKAM       SAVE AGY/MED                                 
*                                                                               
         LA    R2,BUYMSTA+2        R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
BUYBYKX  DS    0H                                                               
*                                                                               
*        HANDLE VARIOUS ELEMENTS                                                
*                                                                               
         LA    R6,BDELEM           POINT TO FIRST ELEMENT                       
         MVI   PRINTSW,1           PRINT FIRST ELEMENT                          
*                                                                               
BUYELMLP DS    0H                                                               
*                                                                               
         CLI   0(R6),X'00'         DONE AT END OF RECORD                        
         BE    BUYELMDN                                                         
*                                                                               
*        MISSED EXCEPTION ELEMENT                                               
*                                                                               
BUYMSE   DS    0H                                                               
*                                                                               
         CLI   0(R6),X'19'         IF MISSED EXCEPTION ELEMENT                  
         BNE   BUYMSEX                                                          
*                                                                               
         USING MSEXELEM,R6         ESTABLISH MISSED EXCEPTION ELEMENT           
*                                                                               
         LA    R2,MSEXSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
         B     BUYELMCN                                                         
*                                                                               
BUYMSEX  DS    0H                                                               
*                                                                               
*        MAKEGOOD EXCEPTION ELEMENT                                             
*                                                                               
BUYMGE   DS    0H                                                               
*                                                                               
         CLI   0(R6),X'29'         IF MAKEGOOD EXCEPTION ELEMENT                
         BNE   BUYMGEX                                                          
*                                                                               
         USING MGEXELEM,R6         ESTABLISH MAKEGOOD EXCEPTION ELM             
*                                                                               
         LA    R2,MGEXSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
         B     BUYELMCN                                                         
*                                                                               
BUYMGEX  DS    0H                                                               
*                                                                               
*        CANADIAN NETWORK STATION ELEMENT                                       
*                                                                               
BUYNTWK  DS    0H                                                               
*                                                                               
         CLC   0(2,R6),=X'680B'    IF CANADIAN NETWORK STATION ELM              
         BNE   BUYNTWKX                                                         
*                                                                               
         USING NTWKELEM,R6         ESTABLISH CANADIAN NTWK STA ELM              
*                                                                               
         LA    R2,NTWKMKST+2       R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
         B     BUYELMCN                                                         
*                                                                               
BUYNTWKX DS    0H                                                               
*                                                                               
*        STATION CALL LETTER CHANGE ELEMENT                                     
*                                                                               
BUYSFX   DS    0H                                                               
*                                                                               
         CLI   0(R6),SFXCODEQ      IF STATION CALL LETTER CHG ELM               
         BNE   BUYSFXX                                                          
*                                                                               
         USING SFXELEM,R6          ESTABLISH STATION CALL LTTR CHG              
*                                                                               
         LA    R2,SFXSTA           R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
         B     BUYELMCN                                                         
*                                                                               
BUYSFXX DS     0H                                                               
*                                                                               
BUYELMCN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)          ELEMENT LENGTH                               
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     BUYELMLP                                                         
*                                                                               
BUYELMDN DS    0H                                                               
*                                                                               
BUYX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - INV'                  
***********************************************************************         
*                                                                     *         
*        OLD INVOICE RECORD - X'0B'                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INV      DS    0H                                                               
*                                                                               
         USING INVRECD,R3          ESTABLISH OLD INVOICE RECORD                 
*                                                                               
         LA    R2,INVKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         MVC   BAGYMD,INVKAM       SAVE AGY/MED                                 
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
INVX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - SIR'                  
***********************************************************************         
*                                                                     *         
*        ESTIMATE BOOK INVENTORY RECORD - X'0C'                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SIR      DS    0H                                                               
*                                                                               
         USING SIRRECD,R3          ESTABLISH EST BOOK INVENT RECORD             
*                                                                               
         CLI   SIRKAM,X'10'        MUST BE AGENCY MEDIA BYTE                    
         BNH   SIRX                                                             
*                                                                               
         OC    SIRKMKT,SIRKMKT     MARKET MUST BE PRESENT                       
         BZ    SIRX                                                             
*                                                                               
         LA    R2,SIRKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         MVC   BAGYMD,SIRKAM       SAVE AGY/MED                                 
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
SIRX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - NETWORK PAROGRAM - CANADA - NDOV'                  
***********************************************************************         
*                                                                     *         
*        NETWORK PROGRAM - CANADA - X'0D17'                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NDOV     DS    0H                                                               
*                                                                               
         USING DOVRECD,R3          ESTABLISH NETWORK PROGRAM - CANADA           
*                                                                               
         MVC   BAGYMD,DOVKAGMD     SAVE AGY/MED                                 
*                                                                               
*        HANDLE VARIOUS ELEMENTS                                                
*                                                                               
         LA    R6,DOVEL01          POINT TO FIRST ELEMENT                       
         MVI   PRINTSW,1           PRINT FIRST ELEMENT                          
*                                                                               
NDOVELLP DS     0H                                                              
*                                                                               
         CLI   0(R6),X'00'         DONE AT END OF RECORD                        
         BE    NDOVELDN                                                         
*                                                                               
*        STATION DEMO ELEMENT                                                   
*                                                                               
NDOVDEL  DS    0H                                                               
*                                                                               
         CLI   0(R6),X'05'         STATION DEMO ELEMENT                         
         BNE   NDOVDELX                                                         
*                                                                               
         USING DOVEL05,R6          ESTABLISH STATION DEMO ELEMENT               
*                                                                               
         CLI   DOVSTA,0            SKIP IF SPILL MARKET                         
         BE    NDOVELCN                                                         
*                                                                               
         LA    R2,DOVSTA           R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
         B     NDOVELCN                                                         
*                                                                               
NDOVDELX DS    0H                                                               
*                                                                               
NDOVELCN DS     0H                                                              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)          ELEMENT LENGTH                               
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     NDOVELLP                                                         
*                                                                               
NDOVELDN DS     0H                                                              
*                                                                               
NDOVX    DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - DO'                   
***********************************************************************         
*                                                                     *         
*        DARE ORDER RECORD - X'0D34'                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DO       DS    0H                                                               
*                                                                               
         USING DAREORDD,R3         ESTABLISH DARE ORDER RECORD                  
*                                                                               
         MVC   BAGYMD,DOKAGMD      SAVE AGY/MED                                 
*                                                                               
         LA    R2,DOKSTA           R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
DOKYX    DS    0H                                                               
*                                                                               
*        HANDLE VARIOUS ELEMENTS                                                
*                                                                               
         LA    R6,DORFRST          POINT TO FIRST ELEMENT                       
         MVI   PRINTSW,1           PRINT FIRST ELEMENT                          
*                                                                               
DOELMLP DS     0H                                                               
*                                                                               
         CLI   0(R6),X'00'         DONE AT END OF RECORD                        
         BE    DOELMDN                                                          
*                                                                               
*        PRIMARY ID ELEMENT                                                     
*                                                                               
DODOID   DS    0H                                                               
*                                                                               
         CLI   0(R6),DOIDELQ       PRIMARY ID ELEMENT                           
         BNE   DODOIDX                                                          
*                                                                               
         USING DOIDELD,R6          ESTABLISH PRIMARY ID ELEMENT                 
*                                                                               
         LA    R2,DOISTA           R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
         B     DOELMCN                                                          
*                                                                               
DODOIDX  DS    0H                                                               
*                                                                               
DOELMCN DS     0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)          ELEMENT LENGTH                               
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     DOELMLP                                                          
*                                                                               
DOELMDN DS     0H                                                               
*                                                                               
DOX      DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - CSO'                  
***********************************************************************         
*                                                                     *         
*        CSO PROGRAM RECORD - X'0D49'                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSO      DS    0H                                                               
*                                                                               
         USING CSORECD,R3          ESTABLISH CHILD SPOT PROG RECORD             
*                                                                               
         MVC   BAGYMD,CSOKAM       SAVE AGY/MED                                 
*                                                                               
         LA    R2,CSOKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
CSOKYX   DS    0H                                                               
*                                                                               
CSOX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - CNT'                  
***********************************************************************         
*                                                                     *         
*        CHILD SPOT CONTRACT RECORD - X'0D4E'                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CNT      DS    0H                                                               
*                                                                               
         USING CNTRECD,R3          ESTABLISH CHILD SPOT CONTRACT REC            
*                                                                               
         MVC   BAGYMD,CNTKAM       SAVE AGY/MED                                 
*                                                                               
         LA    R2,CNTKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
CNTKYX   DS    0H                                                               
*                                                                               
CNTX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - COM'                  
***********************************************************************         
*                                                                     *         
*        CHILD SPOT HEAD/FOOTLINE COMMENTS RECORD - X'0D58'           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COM      DS    0H                                                               
*                                                                               
         USING COMRECD,R3          ESTABLISH HEAD/FOOT COMMENT RECORD           
*                                                                               
         MVC   BAGYMD,COMKAM       SAVE AGY/MED                                 
*                                                                               
         LA    R2,COMKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
COMCKYX  DS    0H                                                               
*                                                                               
COMX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - DEM'                  
***********************************************************************         
*                                                                     *         
*        CHILD SPOT DEMO OVERRIDE RECORD - X'0D57'                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DEM      DS    0H                                                               
*                                                                               
         USING DEMRECD,R3          ESTABLISH CHILD SPOT DEMO OVRRIDE            
*                                                                               
         MVC   BAGYMD,DEMKAM       SAVE AGY/MED                                 
*                                                                               
         LA    R2,DEMKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
DEMDKYX  DS    0H                                                               
*                                                                               
DEMX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - NCOM'                 
***********************************************************************         
*                                                                     *         
*        NEW BUYERS WORKSHEET COMMENT RECORD - X'0D69'                *         
*                                                                     *         
*        DUE TO LABEL CONFLICTS WITH CHILD SPOT COMMENT RECORD        *         
*              WE HAVE TO USE HARD CODED DISPLACEMENTS                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NCOM     DS    0H                                                               
*                                                                               
         MVC   BAGYMD,2(R3)        SAVE AGY/MED                                 
*                                                                               
         LA    R2,8(R3)            R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
NCOMCKYX DS    0H                                                               
*                                                                               
NCOMX    DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - PXC'                  
***********************************************************************         
*                                                                     *         
*        PRODUCT EXCLUSION RECORD - X'0D70'                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PXC      DS    0H                                                               
*                                                                               
         USING PXCRECD,R3          ESTABLISH PRODUCT EXCLUSION RECORD           
*                                                                               
         MVC   BAGYMD,PXCKAGM      SAVE AGY/MED                                 
*                                                                               
         LA    R2,PXCKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
PXCDKYX  DS    0H                                                               
*                                                                               
PXCX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - STK'                  
***********************************************************************         
*                                                                     *         
*        STATUS RECORD - X'0D71'                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STK      DS    0H                                                               
*                                                                               
         USING STATD,R3            ESTABLISH PRODUCT EXCLUSION RECORD           
*                                                                               
         MVC   BAGYMD,STKAGMD      SAVE AGY/MED                                 
*                                                                               
         LA    R2,STKSTA           R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
STKKYX   DS    0H                                                               
*                                                                               
STKX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - SLH'                  
***********************************************************************         
*                                                                     *         
*        STATION LOCKIN HEADER - X'0D72'                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SLH      DS    0H                                                               
*                                                                               
         USING SLHRECD,R3          ESTABLISH STATION LOCKIN HEADER              
*                                                                               
         MVC   BAGYMD,SLHKAGMD     SAVE AGY/MED                                 
*                                                                               
         LA    R2,SLHKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
SLHDKYX  DS    0H                                                               
*                                                                               
SLHX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - CLS'                  
***********************************************************************         
*                                                                     *         
*        CLEARED STATUS RECORD - X'0D76'                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLS      DS    0H                                                               
*                                                                               
         USING CLRSTATD,R3         ESTABLISH CLEARED STATUS RECORD              
*                                                                               
         MVC   BAGYMD,CLSKAGMD     SAVE AGY/MED                                 
*                                                                               
         LA    R2,CLSKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
CLSKYX   DS    0H                                                               
*                                                                               
CLSX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - SPUP'                 
***********************************************************************         
*                                                                     *         
*        UPLOAD RECORD - X'0D78'                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPUP     DS    0H                                                               
*                                                                               
         USING SPUPREC,R3          ESTABLISH CLEARED STATUS RECORD              
*                                                                               
         MVC   BAGYMD,SPUPAM       SAVE AGY/MED                                 
*                                                                               
         LA    R2,SPUPSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
SPUPKYX  DS    0H                                                               
*                                                                               
*        HANDLE VARIOUS ELEMENTS                                                
*                                                                               
         LA    R6,SPUPELEM         POINT TO FIRST ELEMENT                       
         MVI   PRINTSW,1           PRINT FIRST ELEMENT                          
*                                                                               
SPUPLP DS      0H                                                               
*                                                                               
         CLI   0(R6),X'00'         DONE AT END OF RECORD                        
         BE    SPUPDN                                                           
*                                                                               
*        INSERTION ELEMENT                                                      
*                                                                               
SPUP90   DS    0H                                                               
*                                                                               
         CLI   0(R6),X'90'         INSERTION ELEMENT                            
         BNE   SPUP90X                                                          
*                                                                               
         USING DOIDELD,R6          ESTABLISH PRIMARY ID ELEMENT                 
*                                                                               
         LA    R2,SPUPSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
         B     SPUPCN                                                           
*                                                                               
SPUP90X  DS    0H                                                               
*                                                                               
SPUPCN DS      0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)          ELEMENT LENGTH                               
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     SPUPLP                                                           
*                                                                               
SPUPDN DS      0H                                                               
*                                                                               
SPUPX    DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - INF'                  
***********************************************************************         
*                                                                     *         
*        CLEARED STATUS RECORD - X'0D76'                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INFO     DS    0H                                                               
*                                                                               
         USING INFORECD,R3         ESTABLISH INFOMERCICAL RECORD                
*                                                                               
         MVC   BAGYMD,INFKAGMD     SAVE AGY/MED                                 
*                                                                               
         LA    R2,INFKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
INFOKYX  DS    0H                                                               
*                                                                               
INFOX    DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - PW'                   
***********************************************************************         
*                                                                     *         
*        PROFIT WITHIN  RECORD - X'0D7A'                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PW       DS    0H                                                               
*                                                                               
         USING PWRECD,R3           ESTABLISH PROFIT WITHIN RECORD               
*                                                                               
         MVC   BAGYMD,PWKAGMD      SAVE AGY/MED                                 
*                                                                               
         LA    R2,PWKSTA           R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
PWKYX    DS    0H                                                               
*                                                                               
PWX      DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - DBL'                  
***********************************************************************         
*                                                                     *         
*        PROFIT WITHIN  RECORD - X'0D7A'                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DBL      DS    0H                                                               
*                                                                               
         USING DBLBKREC,R3         ESTABLISH DOUBLE BOOK RECORD                 
*                                                                               
         MVC   BAGYMD,DBLKAGMD     SAVE AGY/MED                                 
*                                                                               
         LA    R2,DBLKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
DBLKYX   DS    0H                                                               
*                                                                               
DBLX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - CTA'                  
***********************************************************************         
*                                                                     *         
*        PROFIT WITHIN  RECORD - X'0D7A'                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CTA      DS    0H                                                               
*                                                                               
         USING CTARECD,R3          ESTABLISH CONTRACT RECORD                    
*                                                                               
         MVC   BAGYMD,CTAKAGMD     SAVE AGY/MED                                 
*                                                                               
*        HANDLE VARIOUS ELEMENTS                                                
*                                                                               
         LA    R6,CTAEL            POINT TO FIRST ELEMENT                       
         MVI   PRINTSW,1           PRINT FIRST ELEMENT                          
*                                                                               
CTAELMLP DS     0H                                                              
*                                                                               
         CLI   0(R6),X'00'         DONE AT END OF RECORD                        
         BE    CTAELMDN                                                         
*                                                                               
*        PRIMARY ID ELEMENT                                                     
*                                                                               
CTACTP   DS    0H                                                               
*                                                                               
         CLI   0(R6),CTPARELQ      PARTICIPANTS ELEMENT                         
         BNE   CTACTPX                                                          
*                                                                               
         USING CTPARD,R6           ESTABLISH PARTICIPANT'S ELEMENT              
*                                                                               
         LA    R2,CTPARSTA         R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
         B     CTAELMCN                                                         
*                                                                               
CTACTPX  DS    0H                                                               
*                                                                               
CTAELMCN DS     0H                                                              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)          ELEMENT LENGTH                               
         LA    R6,0(RF,R6)         BUMP TO NEXT ELEMENT                         
         B     CTAELMLP                                                         
*                                                                               
CTAELMDN DS     0H                                                              
*                                                                               
CTAX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - STAB'                 
***********************************************************************         
*                                                                     *         
*        STATION BILLING BUCKET RECORD - X'0E01'                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STAB     DS    0H                                                               
*                                                                               
         USING STABUCKD,R3         ESTABLISH STATION BILLING BUCKETS            
*                                                                               
         MVC   BAGYMD,STABKAM      SAVE AGY/MED                                 
*                                                                               
         LA    R2,STABKSTA         R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
STABKYX  DS    0H                                                               
*                                                                               
STABX    DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'SPLDEXTCDN - RENUMBER CANADIAN STATIONS - SNV'                  
***********************************************************************         
*                                                                     *         
*        STATION INVOICE RECORD - X'0E03'                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SNV      DS    0H                                                               
*                                                                               
         USING SNVKEYD,R3          ESNVLISH STATION INVOICE RECORD              
*                                                                               
         MVC   BAGYMD,SNVKAM       SAVE AGY/MED                                 
*                                                                               
         LA    R2,SNVKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
SNVKYX   DS    0H                                                               
*                                                                               
SNVX     DS    0H                                                               
         J     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - GETSTA'               
***********************************************************************         
*                                                                     *         
*        FIND STATION IN CONVERSION TABLE                             *         
*                                                                     *         
*NTRY    R2==> PACKED STATION CODE                                    *         
*        BAGYMD      AGENCY/MEDIA CODE                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETSTA   NTR1  LABEL=*                                                          
*                                                                               
         MVC   WORK(1),BAGYMD      COPY AGENCY MEDIA                            
         MVC   WORK+1(1),WAGYMD    COPY AGENCY MEDIA                            
*                                                                               
         NC    WORK(2),=X'F0F0'    KILL MEDIA                                   
*                                                                               
         CLC   WORK(1),WORK+1      MUST MATCH ON AGENCY                         
         BNE   GETSTAXX                                                         
*                                                                               
         MVC   WORK(1),BAGYMD      COPY AGENCY MEDIA                            
*                                                                               
         NI    WORK,X'0F'          KILL AGENCY                                  
*                                                                               
         CLI   WORK,X'01'          MUST BE TV                                   
         BE    *+8                                                              
         CLI   WORK,X'03'          OR      NETWORK                              
         BNE   GETSTAXX                                                         
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETSTA1                                                          
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+2,13,0,0  PRINT OLD KEY                      
         GOTO1 VHEXOUT,DMCB,(R2),P+65,2,0,0  OLD NUMBER                         
*                                                                               
         LTR   R6,R6               SKIP IF NO ELEMENT INVOLVED                  
         BZ    GETSTA1                                                          
*                                                                               
         GOTO1 VPRINTER            PRINT OLD KEY                                
*                                                                               
         SR    R7,R7                                                            
         IC    R7,1(R6)            GET ELEMENT LENGTH                           
*                                                                               
         CHI   R7,55               PRINT MAX 55 BYTES                           
         BNH   *+8                                                              
         LA    R7,55                                                            
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R6),P+4,(R7),0,0  PRINT OLD ELEMENT                
*                                                                               
         GOTO1 VPRINTER            PRINT OLD KEY                                
*                                                                               
GETSTA1  DS    0H                                                               
*                                                                               
         LA    R7,CDNRECC          ESTABLISH BINSRCH TABLE WORKAREA             
         USING CDNRECD,R7                                                       
*                                                                               
         MVC   CDNREC(CDNRECLQ),SPACES INIT WORKAREA                            
*                                                                               
         MVC   CDNOLDNM,0(R2)      SET OLD SEQ NUMBER IN KEY                    
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',CDNKEY)   FIND IN TABLE           
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETSTAER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETSTAX             SKIP IF NOT FOUND                            
*                                                                               
         MVC   0(2,R2),CDNNEWNM    SET NEW SEQUENCE NUMBER                      
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETSTAXX                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
         GOTO1 VHEXOUT,DMCB,(R2),P+72,2,0,0   NEW NUMBER                        
*                                                                               
         LTR   R6,R6               SKIP IF NO ELEMENT INVOLVED                  
         BZ    GETSTA2                                                          
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETSTA2                                                          
*                                                                               
         GOTO1 VPRINTER            PRINT NEW KEY                                
*                                                                               
         SR    R7,R7                                                            
         IC    R7,1(R6)            GET ELEMENT LENGTH                           
*                                                                               
         CHI   R7,55               PRINT MAX 55 BYTES                           
         BNH   *+8                                                              
         LA    R7,55                                                            
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R6),P+4,(R7),0,0  PRINT NEW ELEMENT                
*                                                                               
GETSTA2  DS    0H                                                               
*                                                                               
GETSTAX  DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     GETSTAXX                                                         
*                                                                               
GETSTAER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+2,13,0,0  PRINT OLD KEY                      
         GOTO1 VHEXOUT,DMCB,(R2),P+65,2,0,0  OLD NUMBER                         
*                                                                               
         LTR   R6,R6               SKIP IF NO ELEMENT INVOLVED                  
         BZ    GETSTAE1                                                         
*                                                                               
         GOTO1 VPRINTER            PRINT OLD KEY                                
*                                                                               
         SR    R7,R7                                                            
         IC    R7,1(R6)            GET ELEMENT LENGTH                           
*                                                                               
         CHI   R7,55               PRINT MAX 55 BYTES                           
         BNH   *+8                                                              
         LA    R7,55                                                            
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R6),P+4,(R7),0,0  PRINT OLD ELEMENT                
*                                                                               
         GOTO1 VPRINTER            PRINT OLD KEY                                
*                                                                               
GETSTAE1 DS    0H                                                               
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
         MVC   P+72(4),=C'****'               NEW NUMBER                        
*                                                                               
         LTR   R6,R6               SKIP IF NO ELEMENT INVOLVED                  
         BZ    GETSTAE2                                                         
*                                                                               
         GOTO1 VPRINTER            PRINT NEW KEY                                
*                                                                               
         SR    R7,R7                                                            
         IC    R7,1(R6)            GET ELEMENT LENGTH                           
*                                                                               
         CHI   R7,55               PRINT MAX 55 BYTES                           
         BNH   *+8                                                              
         LA    R7,55                                                            
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R6),P+4,(R7),0,0  PRINT NEW ELEMENT                
*                                                                               
GETSTAE2 DS    0H                                                               
*                                                                               
GETSTAEX DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
GETSTAXX DS    0H                                                               
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BNZ   *+10                                                             
         ZAP   CTR,=P'10'          RESET COUNTER                                
*                                                                               
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
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - WORKD'                
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
COUNT    DS    F                                                                
CTR      DC    PL2'10'             TRACE COUNTER                                
*                                                                               
TEMP     DS    CL(CNVRECLQ)        CONVERSION FILE INPUT AREA                   
CDNRECC  DS    XL(CDNRECLQ)        TABLE ENTRY BUILD AREA                       
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
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
         DS    0A                  ALIGNMENT                                    
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
WORKLQ   EQU   *-WORKD             LENGTH OF WORKING STORAGE                    
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - CDNRECD'              
***********************************************************************         
*                                                                     *         
*        DSECT FOR NUMBER CONVERSION TABLE - DIFFERENT ORDER THAN     *         
*              ORIGINAL FILE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CDNRECD  DSECT                                                                  
CDNREC   DS    0XL1                CONVERSION RECORD                            
CDNKEY   DS    0XL1                KEY FOR TABLE                                
CDNOLDNM DS    XL2                 OLD 2 BYTE NUMBER                            
CDNKEYLQ EQU   *-CDNKEY            KEY LENGTH                                   
*                                                                               
CDNAGYMD DS    CL1                 AGY/MEDIA                                    
CDNSTA   DS    CL5                 STATION CALL LETTERS WITH MEDIA              
CDNAGY   DS    CL2                 AGENCY CODE                                  
CDNNEWNM DS    XL2                 NEW 2 BYTE NUMBER                            
         DS    XL(80-(*-CDNRECD))  SPARE                                        
CDNRECLQ EQU   *-CDNREC            RECORD LENGTH                                
CDNRMAXQ EQU   5000                MACXIMUM NUMBER OF RECORDS IN FILE           
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - CNVRECD'              
***********************************************************************         
*                                                                     *         
*        DSECT FOR NUMBER CONVERSION FILE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CNVRECD  DSECT                                                                  
CNVREC   DS    0XL1                CONVERSION RECORD                            
CNVAGY   DS    CL2                 AGENCY CODE                                  
CNVSTA   DS    CL5                 STATION CALL LETTERS WITH MEDIA              
CNVAGYMD DS    CL1                 AGY/MEDIA                                    
CNVOLDNM DS    XL2                 OLD 2 BYTE NUMBER                            
CNVNEWNM DS    XL2                 NEW 2 BYTE NUMBER                            
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
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL5                                                              
         DS    CL2                                                              
POVER    DS    CL4                                                              
         DS    CL3                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL4                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         DS    CL4                                                              
PERR     DS    CL25                                                             
         DS    CL1                                                              
PMYREC   DS    CL46                                                             
         EJECT                                                                  
*SPGENBUY                                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*SPGENINV                                                                       
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
         EJECT                                                                  
*SPGENSIR                                                                       
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
*SPGENDRORD                                                                     
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
*SPGENCSO                                                                       
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
*SPGENINFO                                                                      
       ++INCLUDE SPGENINFO                                                      
         EJECT                                                                  
*SPGENPXC                                                                       
       ++INCLUDE SPGENPXC                                                       
         EJECT                                                                  
*SPGENSTAT                                                                      
       ++INCLUDE SPGENSTAT                                                      
         EJECT                                                                  
*SPGENSLH                                                                       
       ++INCLUDE SPGENSLH                                                       
         EJECT                                                                  
*SPGENCLRST                                                                     
       ++INCLUDE SPGENCLRST                                                     
         EJECT                                                                  
*SPGENSTAB                                                                      
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
*SPGENSNV                                                                       
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
*SPGENUPL                                                                       
       ++INCLUDE SPGENUPL                                                       
         EJECT                                                                  
*SPGENWIPW                                                                      
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
*SPGENDBLBK                                                                     
       ++INCLUDE SPGENDBLBK                                                     
         EJECT                                                                  
*SPGENCTA                                                                       
       ++INCLUDE SPGENCTA                                                       
         EJECT                                                                  
*SPGENNDOV                                                                      
       ++INCLUDE SPGENNDOV                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPLDEXTCDN02/18/99'                                      
         END                                                                    
*                                                                               
