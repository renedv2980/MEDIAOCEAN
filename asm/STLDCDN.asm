*          DATA SET STLDCDN    AT LEVEL 073 AS OF 01/29/99                      
*PHASE STLDCDN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*        TITLE 'STLDCDN -STATION FILE LOAD/DUMP MODEL EXTERN'                   
         TITLE 'STLDCDN -STATION FILE LOAD/DUMP MODEL EXTERN'                   
***********************************************************************         
*                                                                     *         
*        RENUMBER CANADIAN STATIONS                                   *         
*                                                                     *         
*        CANADIAN STATIONS ARE REPRESENTED BY 2 BYTE NUMBERS          *         
*        THIS EXTRACT CHANGES THE NUMBERS BASED ON INPUT FILE         *         
*        THIS MODULE RESTRICTED TO STATION FILE                       *         
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
         TITLE 'STLDCDN - RENUMBER CANADIAN STATIONS - INIT'                    
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKLQ,STLDCDN                                                   
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
         TITLE 'STLDCDN - RENUMBER CANADIAN STATIONS - DMCTL'                   
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
         TITLE 'STLDCDN - RENUMBER CANADIAN STATIONS - DMXINIT'                 
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
         MVC   MID3+1(30),=C'AGY AM  STATN  OLD   NEW'                          
*                                                                               
         XC    WAGY,WAGY           INIT MASTER AGY                              
*                                                                               
DMXINLP  DS    0H                                                               
*                                                                               
         GET   FILEIN,TEMP         READ NEXT RECORD                             
*                                                                               
*        FORMAT TABLE RECORD                                                    
*                                                                               
         MVC   CDNOLDNM,CNVOLDNM   OLD SEQUENCE NUMBER                          
         MVC   CDNAGYMD,CNVAGYMD   AGENCY/MEDIA                                 
         MVC   CDNSTA,CNVSTA       STATION                                      
         MVC   CDNAGY,CNVAGY       AGENCY                                       
         MVC   CDNNEWNM,CNVNEWNM   NEW SEQUENCE NUMBER                          
*                                                                               
         OC    WAGY,WAGY           IF FIRST TIME SAVE AGY                       
         BNE   *+10                                                             
         MVC   WAGY,CNVAGY                                                      
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         MVC   P+02(2),CDNAGY                   AGENCY                          
         MVC   P+5(2),CDNAGY                    AGENCY                          
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
         MVC   MID1+2(8),=CL8'OLD KEY'                                          
         MVC   MID1+40(8),=CL8'NEW KEY'                                         
         MVC   MID1+80(3),=C'OLD'                                               
         MVC   MID1+90(3),=C'NEW'                                               
*                                                                               
         B     DMXIT                                                            
*                                                                               
         TITLE 'STLDCDN - RENUMBER CANADIAN STATIONS - DMXREC'                  
***********************************************************************         
*                                                                     *         
*        PROCESS NEXT RECORD TO BE ADDED TO FILE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD FOR PROCESSING               
*                                                                               
*        DETERMINE RECORD TYPE                                                  
*                                                                               
         CLI   0(R3),C'X'          STATION PASSIVE FOR CANADA TV/NET            
         BE    STX                                                              
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDCDN - RENUMBER CANADIAN STATIONS - SNV'                     
***********************************************************************         
*                                                                     *         
*        STATION INVOICE RECORD - X'0E03'                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STX      DS    0H                                                               
*                                                                               
         USING STAKEY,R3           ESTABLISH STATION PASSIVE                    
*                                                                               
         LA    R2,STXKNUM          R2 POINTS TO CONDENSED STATION CODE          
*                                                                               
         MVC   BAGY,STXKAGY        SAVE AGY                                     
*                                                                               
         GOTO1 GETSTA                                                           
*                                                                               
STXX     DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
         TITLE 'STLDEXTCDN - RENUMBER CANADIAN STATIONS - GETSTA'               
***********************************************************************         
*                                                                     *         
*        FIND STATION IN CONVERSION TABLE                             *         
*                                                                     *         
*NTRY    R2==> PACKED STATION CODE                                    *         
*        BAGY        AGENCY CODE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETSTA   NTR1  LABEL=*                                                          
*                                                                               
         CLC   BAGY,WAGY           MUST MATCH ON AGENCY                         
         BNE   GETSTAXX                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+2,15,0,0  PRINT OLD KEY                      
         GOTO1 VHEXOUT,DMCB,(R2),P+80,2,0,0  OLD NUMBER                         
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
         CLC   BAGY,CDNAGY         MUST MATCH ON AGENCY                         
         BNE   GETSTAX                                                          
*                                                                               
         MVC   0(2,R2),CDNNEWNM    SET NEW SEQUENCE NUMBER                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+40,13,0,0  PRINT NEW KEY                     
         GOTO1 VHEXOUT,DMCB,(R2),P+90,2,0,0   NEW NUMBER                        
*                                                                               
GETSTAX  DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
GETSTAXX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
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
TEMP     DS    CL(CNVRECLQ)        CONVERSION FILE INPUT AREA                   
CDNRECC  DS    XL(CDNRECLQ)        TABLE ENTRY BUILD AREA                       
TABADDR  DS    A                   A(BINSRCH TABLE)                             
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGY     DS    CL2                 AGENCY SAVEAREA                              
WAGY     DS    CL2                 AGENCY SAVEAREA                              
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
*SPGENSTA                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073STLDCDN   01/29/99'                                      
         END                                                                    
*                                                                               
