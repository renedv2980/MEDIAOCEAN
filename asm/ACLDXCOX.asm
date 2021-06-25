*          DATA SET ACLDXCOX   AT LEVEL 044 AS OF 11/22/99                      
*PHASE ACLDXCOX                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACRECTYP                                                               
         TITLE 'DUMP A RECORD FROM DUMP TAPE'                                   
***********************************************************************         
*                                                                               
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
*                                                                               
***********************************************************************         
ACLDXCOP CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**COX**,R7,R8                                                  
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         ST    R1,APARM                                                         
         MVC   PLIST,0(R1)                                                      
         B     DMXCTL                                                           
*                                                                               
DMXCTL   CLI   PLIST,X'00'                                                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'                                                      
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INITIALIZE                                             *         
***********************************************************************         
*                                                                               
DMXINIT  DS    0H                                                               
         ZAP   TOTREC,=P'0'                                                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS RECORD                                         *         
*              NOTE: RETURNED FROM ACRECTYP   0(R1) RECTYPE           *         
*                                             1(R1) COMPANY CODE      *         
*                                             2(R1) DISP TO COMP CODE *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R4                                                       
DMXREC   DS    0H                                                               
         L     R4,AREC             IDENTIFY RECORD TYPE                         
         GOTO1 RECTYP,DMCB,(C'D',TRNRECD)                                       
         CLI   1(R1),X'6F'                                                      
         BNE   DMXKEEP                                                          
         MVC   COMPDSP,2(R1)       SAVE DISP TO COMPANY                         
         MVC   RECDTYP,0(R1)       AND RECORD TYPE                              
         CLI   RECDTYP,ACRTACTL                                                 
         BNE   DMXKEEP                                                          
         USING TRNRECD,R4                                                       
DMXREC05 L     R4,AREC                                                          
         CLI   TRNKCPY,X'6F'       BE VERY CAREFUL                              
         BNE   DMXKEEP                                                          
         CLC   TRNKUNT(14),=CL14'1R8A02TP001856'                                
         BE    DMXREC30                                                         
         CLC   TRNKUNT(14),=CL14'1R8C1AHF002098'                                
         BE    DMXREC30                                                         
         CLC   TRNKUNT(14),=CL14'1R8A02ZZ002009'                                
         BE    DMXREC30                                                         
         CLC   TRNKUNT(14),=CL14'1R8ACAEP001500'                                
         BE    DMXREC30                                                         
         CLC   TRNKUNT(14),=CL14'1R8A02TP001857'                                
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC30 DS    0H                                                               
         BAS   RE,DMPGET           PRINT IT                                     
         AP    TOTREC,=P'1'                                                     
         B     DMXKEEP             GO TO TAPE                                   
         DROP  R4                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
*              PROCESS RECORD                                         *         
*              NOTE: RETURNED FROM ACRECTYP   0(R1) RECTYPE           *         
*                                             1(R1) COMPANY CODE      *         
*                                             2(R1) DISP TO COMP CODE *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R4                                                       
DMXREC   DS    0H                                                               
         L     R4,AREC             IDENTIFY RECORD TYPE                         
         GOTO1 RECTYP,DMCB,(C'D',TRNRECD)                                       
         CLI   1(R1),X'6E'         BSNY                                         
         BNE   DMXKEEP                                                          
         MVC   COMPDSP,2(R1)       SAVE DISP TO COMPANY                         
         MVC   RECDTYP,0(R1)       AND RECORD TYPE                              
         CLI   RECDTYP,ACRTTIM     TMS ONLY                                     
         BNE   DMXKEEP                                                          
         L     R4,AREC                                                          
         USING TIMRECD,R4                                                       
         CLI   TIMKCPY,X'6E'       BE VERY CAREFUL                              
         BNE   DMXKEEP                                                          
         CLC   TIMKOFF,=C'15'      BE VERY VERY CAREFUL                         
         BNE   DMXKEEP                                                          
         CLC   TIMKCUNT(4),=C'1C15'                                             
         BNE   DMXKEEP                                                          
         CLC   TIMKREF,=C'*TIME*'                                               
         BNE   DMXKEEP                                                          
         MVI   ACTIVITY,C'N'                                                    
         USING TIMELD,R6                                                        
         LA    R6,TIMRFST                                                       
DMXREC10 CLI   0(R6),0                                                          
         BE    DMXREC40                                                         
         CLI   0(R6),TIMELQ        X'8B' ELEMENT                                
         BNE   DMXREC20                                                         
         CLI   TIMETYP,TIMEINP     INPUT DETAIL ELEMENT                         
         BE    DMXREC30                                                         
DMXREC20 SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DMXREC10                                                         
*                                                                               
DMXREC30 CLC   TIMACC(2),=C'SJ'    ONLY CONSIDER SJ                             
         BNE   DMXREC20                                                         
         LA    R2,CLOT                                                          
DMXREC34 CLI   0(R2),X'FF'         END OF TABLE                                 
         BE    DMXREC20                                                         
         CLC   TIMACC+2(3),0(R2)   MATCH CLIENT                                 
         BE    DMXREC36                                                         
         LA    R2,L'CLOT(R2)                                                    
         B     DMXREC34                                                         
*                                                                               
DMXREC36 CLC   TIMOFF,=C'15'       IT'S ALREADY GOOD                            
         BE    DMXREC20                                                         
         CLC   TIMOFF,=C'11'       NEED TO CHANGE 11,22,5W                      
         BE    DMXREC37                                                         
         CLC   TIMOFF,=C'22'                                                    
         BE    DMXREC37                                                         
         CLC   TIMOFF,=C'5W'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DMXREC37 CLI   ACTIVITY,C'N'       FIRST TIME DUMP IT                           
         MVI   ACTIVITY,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,DMPGET           PRINT IT                                     
         MVC   TIMOFF,=C'15'       MOVE OFFICE 15                               
         B     DMXREC20            NEXT ELEMENT                                 
*                                                                               
DMXREC40 DS    0H                                                               
         CLI   ACTIVITY,C'Y'                                                    
         BNE   DMXKEEP                                                          
         BAS   RE,DMPPUT           PRINT CHANGES                                
         AP    TOTREC,=P'1'                                                     
         B     DMXKEEP             GO TO TAPE                                   
         DROP  R4                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*              PRINT TOTALS                                           *         
***********************************************************************         
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P(12),=C'CHANGES     '                                           
         EDIT  (P8,TOTREC),(14,P+14)                                            
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLIENT OFFICE TABLE                                    *         
***********************************************************************         
*                                                                               
CLOT     DS    0CL5                                                             
         DC    C'KNG',C'15'                                                     
         DC    C'KN6',C'15'                                                     
         DC    C'TKN',C'15'                                                     
         DC    C'TK6',C'15'                                                     
         DC    C'WB6',C'15'                                                     
         DC    C'WCA',C'15'                                                     
         DC    C'WCL',C'15'                                                     
         DC    C'WC6',C'15'                                                     
         DC    C'WD6',C'15'                                                     
         DC    C'WEF',C'15'                                                     
         DC    C'WEL',C'15'                                                     
         DC    C'WE6',C'15'                                                     
         DC    C'WFS',C'15'                                                     
         DC    C'WFW',C'15'                                                     
         DC    C'WF6',C'15'                                                     
         DC    C'WH6',C'15'                                                     
         DC    C'WH7',C'15'                                                     
         DC    C'WH8',C'15'                                                     
         DC    C'WIC',C'15'                                                     
         DC    C'WII',C'15'                                                     
         DC    C'WI6',C'15'                                                     
         DC    C'WJ7',C'15'                                                     
         DC    C'WLM',C'15'                                                     
         DC    C'WL6',C'15'                                                     
         DC    C'WMS',C'15'                                                     
         DC    C'WM6',C'15'                                                     
         DC    C'WNB',C'15'                                                     
         DC    C'WNE',C'15'                                                     
         DC    C'WNY',C'15'                                                     
         DC    C'WN6',C'15'                                                     
         DC    C'WN7',C'15'                                                     
         DC    C'WN8',C'15'                                                     
         DC    C'WSC',C'15'                                                     
         DC    C'WSD',C'15'                                                     
         DC    C'WSP',C'15'                                                     
         DC    C'WSW',C'15'                                                     
         DC    C'WS6',C'15'                                                     
         DC    C'WTN',C'15'                                                     
         DC    C'WTX',C'15'                                                     
         DC    C'WT6',C'15'                                                     
         DC    C'WV6',C'15'                                                     
         DC    C'WW6',C'15'                                                     
         DC    C'WW7',C'15'                                                     
         DC    C'WW8',C'15'                                                     
         DC    C'WEN',C'15'                                                     
         DC    C'WEC',C'15'                                                     
         DC    C'WJ8',C'15'                                                     
         DC    C'WDM',C'15'                                                     
         DC    X'FF'                                                            
CLOEND   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              DITTO OUTPUT RECORDS                                   *         
***********************************************************************         
*                                                                               
DMPGET   NTR1  ,                                                                
         LA    R7,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1  ,                                                                
         LA    R7,=C'PUT'                                                       
*                                                                               
DUMP     L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                    
                                                                                
DUMPX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
*        EXIT CONDITIONS                                              *         
***********************************************************************         
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     EXIT                                                             
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     EXIT                                                             
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     EXIT                                                             
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     EXIT                                                             
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
          GETEL R2,DATADISP,ELCODE                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
*              EQUATES                                                *         
***********************************************************************         
*                                                                               
SPACE    EQU   X'40'                                                            
         EJECT                                                                  
***********************************************************************         
*              WORK AREA                                              *         
***********************************************************************         
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
HELLO    DC    V(HELLO)                                                         
RECTYP   DC    V(ACRECTYP)                                                      
*                                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                   VPRINTER                                     
VCPRINT  DS    A                   VCPRINT                                      
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
         DS    CL8                                                              
WORK     DS    CL20                                                             
         DS    CL8                                                              
BYTE     DS    C                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
RECDTYP  DS    XL1                 RECORD TYPE (SEE EQUATES)                    
RECCOMP  DS    XL1                 COMPANY CODE                                 
COMPDSP  DS    XL1                 DISPLACEMENT TO COMPANY CODE                 
*                                                                               
*                                                                               
ACTIVITY DC    C'N'                                                             
DATADISP DC    H'56'                                                            
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'2000'                                                        
TOTREC   DC    PL8'0'                                                           
ELM1     DS    CL255                                                            
         EJECT                                                                  
***********************************************************************         
*        OTHER INCLUDES                                               *         
***********************************************************************         
*                                                                               
* DDDPRINT                                                                      
* ACGENFILE                                                                     
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACLDXCOX  11/22/99'                                      
         END                                                                    
