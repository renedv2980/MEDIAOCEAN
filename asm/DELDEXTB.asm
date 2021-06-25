*          DATA SET DELDEXTB   AT LEVEL 009 AS OF 05/01/02                      
*PHASE DELDEXTB,+0                                                              
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'DELDEXT - EXTERN FOR BIRCH HISPANIC'                            
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   BC    0,DMXREC0                                                        
         OI    *-3,X'F0'                                                        
* THIS CODE ONE TIME ONLY - BUT INIT GETS IN TOO SOON                           
         L     R4,VLDDEFN          POINT TO LOAD DEFINITION                     
         USING LDDEFND,R4                                                       
         L     R5,LDDDTFDA         POINT TO DIR ACC DTF                         
         MVC   FILNAME,22(R5)      FILE NAME IS AT DTF+22                       
         MVC   SUBFILE,28(R5)      EXTRACT SUB-FILE CODE (IE A OR N)            
         DROP  R4                                                               
*                                                                               
DMXREC0  L     R3,AREC             POINT TO RECORD                              
         L     R1,RECIN            UPDATE RECORDS IN COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,RECIN                                                         
*                                                                               
         LA    R2,ARBRECS          POINT R2 AT ARB ACCUMS                       
         CLI   2(R3),C'A'          TEST FOR ARB                                 
         BE    DMXREC1                                                          
         LA    R2,NSIRECS                                                       
         CLI   2(R3),C'N'          TEST FOR NSI                                 
         BE    DMXREC1                                                          
         LA    R2,BBMRECS                                                       
         CLI   2(R3),C'M'          TEST FOR BBM RADIO                           
         BE    DMXREC1                                                          
         LA    R2,ORECS            NEITHER ARB OR NSI OR BBM                    
         SPACE 1                                                                
DMXREC1  DS    0H                                                               
         L     R1,0(R2)                                                         
         LA    R1,1(R1)            UPDATE ACCUMS                                
         ST    R1,0(R2)                                                         
*                                                                               
         CLI   0(R3),C'U'          TEST FOR UNIVERSE                            
         BE    DMXPURGE                                                         
         L     R1,RECKEEP          INCREMENT RECORD KEEP COUNT                  
         LA    R1,1(R1)                                                         
         ST    R1,RECKEEP                                                       
         B     DMXKEEP                                                          
         SPACE 1                                                                
* DUMP RECORD WITH WRONG SOURCE - WRITE TO CONSOLE AND BLOW UP                  
*                                                                               
DMXREC2  DS    0H                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(16),=C'RECORD SOURCE IS'                                  
         MVC   WORK+18(1),2(R3)                                                 
         MVI   WORK,18                                                          
         BAS   RE,DUMPREC                                                       
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(32),=C'**WARNING** WRONG INPUT TAPE FOR'                    
         MVC   WORK+33(7),FILNAME                                               
         GOTO1 LOGIO,DMCB,1,(41,WORK),RR=RELO                                   
         DC    H'0'                                                             
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+10(7),FILNAME                                                  
         MVC   P+18(14),=C'SUMMARY TOTALS'                                      
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(21),P+10                                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,BUCKETS          COUNTER                                      
         LA    R3,BUCKTAB          POINTER TO BUCKETS                           
DMXEOF1  MVC   P+10(20),4(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            BUCKET VALUE                                 
         EDIT  (R2),(10,P+32)                                                   
         GOTO1 VPRINTER                                                         
         LA    R3,L'BUCKTAB(R3)                                                 
         BCT   R4,DMXEOF1                                                       
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
         MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
         LH    R5,HALF                                                          
         CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
         BNE   *+8                                                              
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         GOTO1 PRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D',     X        
               RR=RELO                                                          
         B     DMXIT                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
FILNAME  DC    CL7' '                                                           
SUBFILE  DC    C' '                                                             
         SPACE 2                                                                
* ROUTINE TABLE                                                                 
*                                                                               
LOGIO    DC    V(LOGIO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
         SPACE 2                                                                
* BUCKET TABLE                                                                  
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
RECIN    DC    F'0',CL20'RECORDS IN'                                            
ARBRECS  DC    F'0',CL20'ARB RECORDS'                                           
NSIRECS  DC    F'0',CL20'BIR RECORDS'                                           
BBMRECS  DC    F'0',CL20'BBM RECORDS'                                           
ORECS    DC    F'0',CL20'OTHER RECORDS'                                         
RECKEEP  DC    F'0',CL20'RECORDS KEPT'                                          
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
RELO     DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL60                                                             
ELCODE   DS    C                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DELDEXTB  05/01/02'                                      
         END                                                                    
