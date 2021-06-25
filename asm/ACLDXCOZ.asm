*          DATA SET ACLDXCOZ   AT LEVEL 014 AS OF 02/01/01                      
*PHASE ACLDXCOZ                                                                 
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
ACLDXCOZ CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**COZ**,R7,R8                                                  
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
         MVC   COMPDSP,2(R1)       SAVE DISP TO COMPANY                         
         MVC   RECDTYP,0(R1)       AND RECORD TYPE                              
         CLI   RECDTYP,ACRTACTL    ACCOUNT RECORDS ONLY                         
         BE    *+12                                                             
         CLI   RECDTYP,ACRTACTH                                                 
         BNE   DMXKEEP                                                          
         L     R4,AREC                                                          
         USING ACTRECD,R4                                                       
         CLC   ACTKUNT(2),=C'SJ'                                                
         BNE   DMXKEEP                                                          
         CLC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),SPACES                          
         BNE   DMXKEEP                                                          
         USING PBAELD,R6                                                        
         SR    R1,R1                                                            
         LA    R6,ACTRFST                                                       
DMXR20   CLI   0(R6),0             EOR?                                         
         BE    DMXKEEP                                                          
         CLI   PBAEL,PBAELQ        POOL ELEMENT                                 
         BE    DMXR30                                                           
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DMXR20                                                           
*                                                                               
DMXR30   AP    TOTREC,=P'1'                                                     
         BAS   RE,DMPGET                                                        
         B     DMXKEEP             GO TO TAPE                                   
         DROP  R4,R6                                                            
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
MAXDUMP  DC    PL4'1000'                                                        
TOTREC   DC    PL8'0'                                                           
WRKREF   DC    CL6' '                                                           
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
**PAN#1  DC    CL21'014ACLDXCOZ  02/01/01'                                      
         END                                                                    
