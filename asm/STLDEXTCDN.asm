*          DATA SET STLDEXTCDN AT LEVEL 009 AS OF 08/10/00                      
*PHASE STEXTCDA STEXTCDN                                                        
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXOUT                                                                 
*        TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS'                        
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS'                        
***********************************************************************         
*                                                                     *         
*        CANADIAN STATIONS ARE REPRESENTED BY 2 BYTE NUMBERS          *         
*        THIS EXTRACT CHANGES THE NUMBERS BASED ON INPUT FILE         *         
*        THIS MODULE RESTRICTED TO TRAFFIC RECORDS                    *         
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
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - INIT'                 
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKLQ,STLDEXT                                                   
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
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - DMCTL'                
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
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - DMXINIT'              
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
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - DMXREC'               
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
         CLC   =X'0A24',0(R3)      TEST TRAFFIC INST RECAP                      
         BE    INS                                                              
*                                                                               
         CLC   =X'0A25',0(R3)      TEST TRAFFIC SHIP RECAP                      
         BE    SHP                                                              
*                                                                               
         CLC   =X'0A2E',0(R3)      TEST TRAFFIC BUY ACT                         
         BE    TBA                                                              
*                                                                               
         CLC   =X'0A2F',0(R3)      TEST LABEL LIST                              
         BE    LST                                                              
*                                                                               
         CLC   =X'0A31',0(R3)      TEST STATION LIST                            
         BE    STL                                                              
*                                                                               
         CLC   =X'0A32',0(R3)      TEST TRAFFIC BUY                             
         BE    TBY                                                              
*                                                                               
         CLC   =X'0A35',0(R3)      COMMERCIAL TEXT REC                          
         BE    CMT                                                              
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - INS'                  
***********************************************************************         
*                                                                     *         
*        TRAFFIC INSTRUCTION RECAP RECORD - X'0A24'                   *         
*                                                                     *         
*NTRY    R3==> RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INS      DS    0H                                                               
*                                                                               
         USING INSRECD,R3          ESTABLISH TRAFFIC INST RECAP REC             
*                                                                               
         LA    R2,INSKSTA          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         MVC   BAGYMD,INSKAM       SAVE AGY/MED                                 
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
INSX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - SHP'                  
***********************************************************************         
*                                                                     *         
*        TRAFFIC SHIPPING RECAP RECORD - X'0A25'                      *         
*                                                                     *         
*NTRY    R3==> RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SHP      DS    0H                                                               
*                                                                               
         USING SHPRECD,R3          ESTABLISH TRAFFIC SHIP RECAP REC             
*                                                                               
         LA    R2,SHPKSTA          R2 POINTS TO PACKED STATION CODE             
*                                                                               
         MVC   BAGYMD,SHPKAM       SAVE AGY/MED                                 
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
SHPX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - TBA'                  
***********************************************************************         
*                                                                     *         
*        TRAFFIC BUY ACTIVITY RECORD - X'0A2E'                        *         
*                                                                     *         
*NTRY    R3==> RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TBA      DS    0H                                                               
*                                                                               
         USING TBARECD,R3          ESTABLISH TRAFFIC BUY ACTIUITY REC           
*                                                                               
         LA    R2,TBAKSTA          R2 POINTS TO PACKED STATION CODE             
*                                                                               
         MVC   BAGYMD,TBAKAM       SAVE AGY/MED                                 
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
TBAX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - LST'                  
***********************************************************************         
*                                                                     *         
*        TRAFFIC STATION LIST FOR LABELS RECORD - X'0A2F'             *         
*                                                                     *         
*NTRY    R3==> RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LST      DS    0H                                                               
*                                                                               
         MVC   BAGYMD,2(R3)        SET AGY/MEDIA                                
*                                                                               
         USING LSTRECD,R3          ESTABLISH TRAFFIC LABELS LIST REC            
*                                                                               
         MVI   ELCODE,X'10'        FIND TRAFFIC LIST DATA ELEMENT               
         LR    R6,R3                                                            
         BRAS  RE,GETEL                                                         
*                                                                               
LSTLOOP  DS    0H                                                               
*                                                                               
         BNE   LSTX                DONE AT END OF RECORD                        
*                                                                               
         USING LSTDTAEL,R6         ESTABLISH LIST DATA ELEMENT                  
*                                                                               
         LA    R2,LSTSTA           POINT TO STATION                             
*                                                                               
         GOTO1 GETSTA              CHANGE SEQUENCE NUMBERS                      
*                                                                               
LSTCONT  DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           BUMP TO NEXT ELEMENT IN LIST                 
         B     LSTLOOP                                                          
*                                                                               
LSTDONE  DS    0H                                                               
*                                                                               
LSTX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - STL'                  
***********************************************************************         
*                                                                     *         
*        TRAFFIC STATION LIST RECORD - X'0A31'                        *         
*                                                                     *         
*NTRY    R3==> RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STL      DS    0H                                                               
*                                                                               
         MVC   BAGYMD,2(R3)        SET AGY/MEDIA                                
*                                                                               
         USING STLRECD,R3          ESTABLISH TRAFFIC STATION LIST REC           
*                                                                               
         MVI   ELCODE,X'10'        FIND TRAFFIC LIST DATA ELEMENT               
         LR    R6,R3                                                            
         BRAS  RE,GETEL                                                         
*                                                                               
STLLOOP  DS    0H                                                               
*                                                                               
         BNE   STLX                DONE AT END OF RECORD                        
*                                                                               
         USING STLDTAEL,R6         ESTABLISH LIST DATA ELEMENT                  
*                                                                               
         LA    R2,STLSTA           POINT TO STATION                             
*                                                                               
         GOTO1 GETSTA              CHANGE SEQUENCE NUMBERS                      
*                                                                               
STLCONT  DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           BUMP TO NEXT ELEMENT IN LIST                 
         B     STLLOOP                                                          
*                                                                               
STLDONE  DS    0H                                                               
*                                                                               
STLX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - TBY'                  
***********************************************************************         
*                                                                     *         
*        TRAFFIC BUY RECORD - X'0A32'                                 *         
*                                                                     *         
*NTRY    R3==> RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TBY      DS    0H                                                               
*                                                                               
         USING TBYRECD,R3          ESTABLISH TRAFFIC BUY RECORD                 
*                                                                               
         LA    R2,TBYKSTA          R2 POINTS TO PACKED STATION CODE             
*                                                                               
         MVC   BAGYMD,TBYKAM       SAVE AGY/MED                                 
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
TBYX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - CMT'                  
***********************************************************************         
*                                                                     *         
*        TRAFFIC COMMERCIAL TEXT RECORD - X'0A35'                     *         
*                                                                     *         
*NTRY    R3==> RECORD                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CMT      DS    0H                                                               
*                                                                               
         USING CMTRECD,R3          ESTABLISH TRAFFIC BUY ACTIUITY REC           
*                                                                               
         LA    R2,CMTKSTA          R2 POINTS TO PACKED STATION CODE             
*                                                                               
         MVC   BAGYMD,CMTKAM       SAVE AGY/MED                                 
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
CMTX     DS    0H                                                               
         B     DMXKEEP                                                          
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
         CLC   BAGYMD,WAGYMD       MUST MATCH ON AGENCY MEDIA                   
         BNE   GETSTAXX                                                         
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
         BE    GETSTAX                                                          
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETSTAX             SKIP IF NOT FOUND                            
*                                                                               
         CLC   BAGYMD,CDNAGYMD     MUST MATCH ON AGENCY MEDIA                   
         BNE   GETSTAX                                                          
*                                                                               
         MVC   0(2,R2),CDNNEWNM    SET NEW SEQUENCE NUMBER                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
         GOTO1 VHEXOUT,DMCB,(R2),P+72,2,0,0   NEW NUMBER                        
*                                                                               
         LTR   R6,R6               SKIP IF NO ELEMENT INVOLVED                  
         BZ    GETSTA2                                                          
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
GETSTAXX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - GETEL'                
***********************************************************************         
*                                                                     *         
*        GETEL MACRO                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GETEL R6,24,ELCODE                                                     
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - FILEIN'               
***********************************************************************         
*                                                                     *         
*        DCB FOR INPUT FILE CONTAINING STATIONS WITH OLD AND NEW      *         
*        NUMBERS                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
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
TEMP     DS    CL(CNVRECLQ)        CONVERSION FILE INPUT AREA                   
CDNRECC  DS    XL(CDNRECLQ)        TABLE ENTRY BUILD AREA                       
TABADDR  DS    A                   A(BINSRCH TABLE)                             
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMD   DS    CL1                                                              
WAGYMD   DS    CL1                 MASTER AGY/MEDIA                             
*                                                                               
*        PARAMETER LIST SAVE AREA                                               
*                                                                               
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
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
       ++INCLUDE SPTRTBAE                                                       
         EJECT                                                                  
       ++INCLUDE SPTRLBLS                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTAL                                                       
         EJECT                                                                  
       ++INCLUDE SPTRBUY                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMLTXT                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009STLDEXTCDN08/10/00'                                      
         END                                                                    
