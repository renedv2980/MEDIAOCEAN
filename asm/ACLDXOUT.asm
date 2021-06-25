*          DATA SET ACLDXOUT   AT LEVEL 004 AS OF 01/29/97                      
*PHASE ACLDXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACRECTYP                                                               
         TITLE 'CREATE OUT TAPE FOR SELECTED COMPANIES'                         
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
ACLDXOUT CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DRP**,R7,R8                                                  
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
         OPEN  (TXOUT,(OUTPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS RECORD                                         *         
***********************************************************************         
*                                                                               
         USING ACTRECD,R4                                                       
DMXREC   DS    0H                                                               
         L     R4,AREC             IDENTIFY RECORD TYPE                         
         GOTO1 RECTYP,DMCB,(C'D',ACTRECD)                                       
         CLI   1(R1),X'6E'         COPY BSNY TO TAPE                            
         BE    DMXREC3                                                          
         CLI   1(R1),X'D8'         COPY BSNEW TAPE                              
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC3  LR    R3,R4               WRITE TO TAPE                                
         SH    R3,=H'4'                                                         
         PUT   TXOUT,(R3)                                                       
         B     DMXPURGE            PURGE FROM INPUT                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              PRINT TOTALS                                           *         
***********************************************************************         
DMXEOF   DS    0H                                                               
         CLOSE (TXOUT)                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DITTO OUTPUT RECORDS                                   *         
***********************************************************************         
*                                                                               
DMPPUT   NTR1  ,                                                                
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         LA    R7,=C'PUT'                                                       
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
                                                                                
DUMP     LH    R8,HALF                                                          
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
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'5000'                                                        
*                                                                               
TXOUT     DCB  DDNAME=TXOUT,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,LRECL=2048,BLKSIZE=32760                                
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
**PAN#1  DC    CL21'004ACLDXOUT  01/29/97'                                      
         END                                                                    
