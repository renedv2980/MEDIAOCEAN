*          DATA SET ACLDXY20   AT LEVEL 016 AS OF 07/22/98                      
*PHASE ACLDXY20                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACRECTYP                                                               
         TITLE 'EXTRACT DATA FOR Y2000 TEST'                                    
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
                                                                                
ACLDXY20 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*Y20**,R7,R8                                                   
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
                                                                                
DMXINIT  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PROCESS RECORD                                         *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
DMXREC   DS    0H                                                               
         L     R2,AREC             IDENTIFY RECORD TYPE                         
         GOTO1 ACRECTYP,DMCB,(C'D',ACTRECD)                                     
         MVC   RECTYP,0(R1)        RECORD TYPE                                  
         MVC   CURCOMP,1(R1)       CURRENT COMPANY                              
         MVC   COMPDSP,2(R1)       SAVE DISPLACEMENT TO COMPANY                 
*                                                                               
*        CLI   CURCOMP,X'F5'       TEST BBDO                                    
*        BNE   DMXPURGE                                                         
         CLI   CURCOMP,X'F5'       TEST BBDO                                    
         BNE   DMXPURGE                                                         
*                                                                               
         CLI   RECTYP,ACRTTRN      DELETE TRANSACTIONS                          
         BE    DMXPURGE                                                         
         CLI   RECTYP,ACRTTIM      AND TIME                                     
         BE    DMXPURGE                                                         
*                                                                               
         CLI   RECTYP,ACRTOTHR     TEST SPECIAL RECORDS                         
         BL    DMXR1                                                            
         CLI   RECTYP,ACRTNBT      DROP BATCH                                   
         BE    DMXPURGE                                                         
         CLI   RECTYP,ACRTSCM      DROP COMMENT                                 
         BE    DMXPURGE                                                         
         CLI   0(R2),X'2F'         MEDIA DETAILS                                
         BE    DMXPURGE                                                         
         CLI   0(R2),X'3E'         DROP SPECIAL TIME/COST RECORDS               
         BE    DMXPURGE                                                         
         CLI   0(R2),X'3F'         DROP STORED REQUEST                          
         BE    DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXR1    CLI   RECTYP,ACRTACTL                                                  
         BH    DMXPURGE            DROP CONTRA & TRANSACTIONS                   
         BL    DMXKEEP             KEEP U/L   ACCOUNT HIGH                      
         SR    R0,R0                                                            
         LA    R3,ACTRFST                                                       
*                                                                               
DMXR2    CLI   0(R3),ABLELQ        FIND BALANCE ELEMENT                         
         BE    DMXR3                                                            
         CLI   0(R3),0                                                          
         BE    DMXR5                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DMXR2                                                            
*                                                                               
         USING ABLELD,R3                                                        
DMXR3    ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
*                                                                               
DMXR5    CLC   ACTKUNT(2),=C'SJ'                                                
         BNE   DMXKEEP                                                          
         CLC   ACTKACT(3),=C'FX '                                               
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*              PRINT TOTALS                                           *         
***********************************************************************         
                                                                                
DMXEOF   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DITTO OUTPUT RECORDS                                   *         
***********************************************************************         
                                                                                
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
         EJECT                                                                  
***********************************************************************         
*              WORK AREA                                              *         
***********************************************************************         
                                                                                
PRNTBL   DC    V(PRNTBL)                                                        
PRINT    DC    V(PRINT)                                                         
HELLO    DC    V(HELLO)                                                         
ACRECTYP DC    V(ACRECTYP)                                                      
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
COMPDSP  DS    XL1                 DISP TO COMPANY                              
RECTYP   DS    XL1                 RECORD TYPE                                  
CURCOMP  DS    XL1                 CURRENT COMPANY                              
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
*                                                                               
*                                                                               
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'500'                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OTHER INCLUDES                                               *         
***********************************************************************         
                                                                                
* DDDPRINT                                                                      
* ACGENFILE                                                                     
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACLDXY20  07/22/98'                                      
         END                                                                    
