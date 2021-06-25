*          DATA SET REPLDXSP4  AT LEVEL 017 AS OF 08/31/00                      
*          DATA SET REPLDXSP4  AT LEVEL 016 AS OF 12/19/96                      
*PHASE UNXREP4A                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'KATZ: KLJB-->KLJB-1 && WAOW-->WAOW-1'                           
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
UNLDXSP  CSECT                                                                  
         NMOD1 WORKX-WORKD,UNLDXSP,RR=R2                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE GOTO1 =V(PRNTBL),DMCB,=C'BYPS',AREC,C'DUMP',30,=C'1D'                  
         L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* *********************************************************************         
* THIS IS IT                                                                    
* *********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC                                         
DMXREC   DS    0H                                                               
         USING RINVREC,R6                                                       
         L     R6,AREC            POINT TO RECORD                               
         CLI   0(R6),X'12'         CHECK FOR INVENTORY                          
         BNE   DMXKEEP                                                          
         CLC   RINVKREP,=CL2'AM'   CHECK FOR KAMNY  (KATZ) AGENCY               
         BE    FXAM000                                                          
         CLC   RINVKREP,=CL2'CQ'   CHECK FOR KCONYR (KATZ) AGENCY               
         BE    FXCQ000                                                          
         B     DMXKEEP                                                          
         EJECT                                                                  
*                                                                               
** KAMNY (AM) INVENTORY RECORD **                                               
*                                                                               
FXAM000  DS    0H                                                               
         CLC   RINVKSTA,=C'KLJBT'  CHECK FOR A KLJB   STATION                   
         BNE   DMXKEEP                                                          
                                                                                
*                                                                               
*** THIS RECORD NEEDS FIXING ***                                                
*                                                                               
         DS    0H                  PRINT RECD BEFORE IT GETS PROCESSED          
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',RINVREC,C'DUMP',32,=C'1D'               
                                                                                
*                                                                               
         CLC   RINVKSTA,=C'KLJBT'  CHANGE KLJB   STATION                        
         BNE   *+14                                                             
         MVC   RINVKSTA,=C'KLJB1'   TO KLJB-1                                   
         B     FXAM039                                                          
                                                                                
         DC    H'0'                                                             
FXAM039  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  UPDATE # OF KAMNY  RECORDS                   
         LA    R0,1                                                             
         A     R0,CNTAM                                                         
         ST    R0,CNTAM                                                         
                                                                                
*                                                                               
         B     FX100                                                            
         EJECT                                                                  
*                                                                               
** KCONYR (CQ) INVENTORY RECORD **                                              
*                                                                               
FXCQ000  DS    0H                                                               
         CLC   RINVKSTA,=C'KLJBT'  CHECK FOR A KLJB   STATION                   
         BE    FXCQ010X                                                         
         CLC   RINVKSTA,=C'WAOWT'  CHECK FOR A WAOW   STATION                   
         BE    FXCQ010X                                                         
         B     DMXKEEP                                                          
FXCQ010X EQU   *                                                                
                                                                                
*                                                                               
*** THIS RECORD NEEDS FIXING ***                                                
*                                                                               
         DS    0H                  PRINT RECD BEFORE IT GETS PROCESSED          
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',RINVREC,C'DUMP',32,=C'1D'               
                                                                                
*                                                                               
         CLC   RINVKSTA,=C'KLJBT'  CHANGE KLJB   STATION                        
         BNE   *+18                                                             
         MVC   RINVKSTA,=C'KLJB1'   TO KLJB-1                                   
         LA    RF,CNTCQK                                                        
         B     FXCQ039                                                          
                                                                                
         CLC   RINVKSTA,=C'WAOWT'  CHANGE WAOW   STATION                        
         BNE   *+18                                                             
         MVC   RINVKSTA,=C'WAOW1'   TO WAOW-1                                   
         LA    RF,CNTCQW                                                        
         B     FXCQ039                                                          
                                                                                
         DC    H'0'                                                             
FXCQ039  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  RF-->COUNTER FOR SPECIFIC CQ STTN            
         LA    R0,1                                                             
         A     R0,0(RF)                                                         
         ST    R0,0(RF)                                                         
*                                                                               
         DS    0H                  UPDATE # OF KCONYR RECORDS                   
         LA    R0,1                                                             
         A     R0,CNTCQ                                                         
         ST    R0,CNTCQ                                                         
                                                                                
*                                                                               
         B     FX100                                                            
         EJECT                                                                  
*                                                                               
** UPDATE STATISTICS FOR RECORD CHANGED **                                      
*                                                                               
FX100    DS    0H                                                               
         LA    R0,1                                                             
         LR    R1,R0                                                            
         A     R1,CNTRECHG                                                      
         ST    R1,CNTRECHG         UPDATE # OF RECORDS PROCESSED                
                                                                                
         CLI   RINVKSRC,0          IF HEADER RECORD,                            
         BNE   *+16                                                             
         A     R0,CNTHDR                                                        
         ST    R0,CNTHDR            UPDATE # OF HEADERS PROCESSED               
         B     FX109                                                            
                                                                                
         CLI   RINVKSRC,C'M'       IF MARKET FACT RECORD,                       
         BNE   *+16                                                             
         A     R0,CNTMKTF                                                       
         ST    R0,CNTMKTF           UPDATE # OF MKT FCT PROCESSED               
         B     FX109                                                            
                                                                                
         CLI   RINVKSRC,C'S'       IF STATION FACT RECORD,                      
         BNE   *+16                                                             
         A     R0,CNTSTAF                                                       
         ST    R0,CNTSTAF           UPDATE # OF STA FCT PROCESSED               
         B     FX109                                                            
                                                                                
         CLI   RINVKSRC,X'FF'      IF RATIONALE RECORD,                         
         BNE   *+16                                                             
         A     R0,CNTRTNL                                                       
         ST    R0,CNTRTNL           UPDATE # OF RATIONALES PROCESSED            
         B     FX109                                                            
                                                                                
         A     R0,CNTTRK                                                        
         ST    R0,CNTTRK           UPDATE # OF TRACK RCDS PROCESSED             
         B     FX109                                                            
FX109    EQU   *                                                                
*                                                                               
         DS    0H                  PRINT RECD AFTER IT GETS PROCESSED           
         GOTO1 =V(PRNTBL),DMCB,=C'AFTR',RINVREC,C'DUMP',32,=C'1D'               
                                                                                
         MVI   SPACING+3,C'1'                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXKEEP                                                          
         DROP  R6                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   LINE,=PL2'99'       PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
                                                                                
*                                                                               
         DS    0H                  PRINT REPORT STATISTICS                      
         LA    R2,RPTTAB                                                        
*                                                                               
PRS010   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    PRSX                                                             
                                                                                
         CLC   0(L'RPTTAB,R2),SPACES                                            
         BNE   PRS020                                                           
         MVI   P,0                                                              
         GOTO1 VPRINTER                                                         
         B     PRS030                                                           
                                                                                
PRS020   DS    0H                                                               
         MVC   P+10(27),0(R2)      MOVE MESSAGE TO PRINT LINE                   
         ZICM  RF,27(R2),(3)                                                    
         LA    RF,UNLDXSP(RF)      RF-->COUNTER                                 
         L     R3,0(RF)            R3 = COUNT                                   
         EDIT  (R3),(10,P+39),ZERO=NOBLANK,COMMAS=YES                           
         GOTO1 VPRINTER                                                         
         B     PRS030                                                           
*                                                                               
PRS030   DS    0H                                                               
         LA    R2,L'RPTTAB(R2)                                                  
         B     PRS010                                                           
                                                                                
*                                                                               
PRSX     EQU   *                                                                
         B     DMXIT                                                            
         EJECT                                                                  
* *******************************************************************           
* VARIABLE LIST                                                                 
* *******************************************************************           
DATADISP DC    H'0024'                                                          
                                                                                
                                                                                
CNTAM    DC    F'0'                COUNTS # OF KAMNY RECORDS                    
CNTCQK   DC    F'0'                COUNTS # OF KCONYR/KLJB RECORDS              
CNTCQW   DC    F'0'                COUNTS # OF KCONYR/WAOW RECORDS              
CNTCQ    DC    F'0'                COUNTS # OF KCONYR RECORDS                   
CNTHDR   DC    F'0'                COUNTS # OF HEADERS                          
CNTMKTF  DC    F'0'                COUNTS # OF MARKET FACTS                     
CNTSTAF  DC    F'0'                COUNTS # OF STATION FACTS                    
CNTTRK   DC    F'0'                COUNTS # OF TRACK RECORDS                    
CNTRTNL  DC    F'0'                COUNTS # OF RATIONALES                       
CNTRECHG DC    F'0'                COUNTS # OF RECORDS CHANGED                  
*                                                                               
         SPACE                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
RPTTAB   DS    0XL(27+2)                                                        
         DC     CL27'# OF KAMNY RECORDS  CHANGED',AL2(CNTAM-UNLDXSP)            
         DC     CL(L'RPTTAB)' '                                                 
         DC     CL27'# OF KCONYR/KLJB    CHANGED',AL2(CNTCQK-UNLDXSP)           
         DC     CL27'# OF KCONYR/WAOW    CHANGED',AL2(CNTCQW-UNLDXSP)           
         DC     CL27'# OF KCONYR RECORDS CHANGED',AL2(CNTCQ-UNLDXSP)            
         DC     CL(L'RPTTAB)' '                                                 
         DC     CL27'# OF INV HEADERS    CHANGED',AL2(CNTHDR-UNLDXSP)           
         DC     CL27'# OF MARKET FACTS   CHANGED',AL2(CNTMKTF-UNLDXSP)          
         DC     CL27'# OF STATION FACTS  CHANGED',AL2(CNTSTAF-UNLDXSP)          
         DC     CL27'# OF INV TRACKS     CHANGED',AL2(CNTTRK-UNLDXSP)           
         DC     CL27'# OF RATIONALES     CHANGED',AL2(CNTRTNL-UNLDXSP)          
         DC     CL(L'RPTTAB)' '                                                 
         DC     CL27'TOTAL # OF RECORDS  CHANGED',AL2(CNTRECHG-UNLDXSP)         
         DC    X'00'                                                            
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
BYTE     DS    CL1                                                              
WORK     DS    CL100                                                            
WORK2    DS    CL100                                                            
HALF     DS    H                                                                
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE REGENINVA                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017REPLDXSP4 08/31/00'                                      
         END                                                                    
