*          DATA SET DELDEXTG   AT LEVEL 018 AS OF 01/10/94                      
*PHASE DELDEXTG,+0                                                              
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'DELDEXT - LOAD/DUMP EXTERNAL ROUTINE FOR DEMO FILES'            
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
         LA    R2,NSIRECS                                                       
         CLI   2(R3),C'N'          NSI                                          
         BE    DMXREC1                                                          
         LA    R2,ARBRECS                                                       
         CLI   2(R3),C'A'          ARB                                          
         BE    DMXREC1                                                          
         LA    R2,ORECS            NEITHER ARB OR NSI OR BBM                    
         SPACE 1                                                                
DMXREC1  DS    0H                                                               
         CLI   0(R3),DSECDEQU      STATION EQUIV RECDS ONLY                     
         BNE   DMXOK                                                            
         CLI   1(R3),C'R'          RADIO ONLY                                   
         BNE   DMXOK                                                            
         CLI   2(R3),C'A'          ARB ONLY                                     
         BNE   DMXOK                                                            
         CLI   3(R3),X'01'         THIS SHOULD SEPARATE THE BAD                 
         BH    DMXNOTOK             RECORDS FROM THE GOOD RECORDS               
                                                                                
*                                                                               
DMXOK    L     R1,0(R2)                                                         
         LA    R1,1(R1)            UPDATE ACCUMS                                
         ST    R1,0(R2)                                                         
         L     R1,RECKEEP          INCREMENT RECORD KEEP COUNT                  
         LA    R1,1(R1)                                                         
         ST    R1,RECKEEP                                                       
         B     DMXKEEP                                                          
*                                                                               
DMXNOTOK L     R1,0(R2)                                                         
         LA    R1,1(R1)            UPDATE ACCUMS                                
         ST    R1,0(R2)                                                         
         L     R1,RECPURGE         INCREMENT RECORD PURGE COUNT                 
         LA    R1,1(R1)                                                         
         ST    R1,RECPURGE                                                      
         MVC   P(25),0(R3)         PRINT RECORD TO BE PURGED                    
         GOTO1 VPRINTER                                                         
         B     DMXPURGE                                                         
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
NSIRECS  DC    F'0',CL20'NSI RECORDS'                                           
BBMRECS  DC    F'0',CL20'BBM RECORDS'                                           
ORECS    DC    F'0',CL20'OTHER RECORDS'                                         
RECKEEP  DC    F'0',CL20'RECORDS KEPT'                                          
RECPURGE DC    F'0',CL20'RECORDS PURGED'                                        
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
         PRINT ON                                                               
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
**PAN#1  DC    CL21'018DELDEXTG  01/10/94'                                      
         END                                                                    
