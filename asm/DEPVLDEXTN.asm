*          DATA SET DEPVLDEXTN AT LEVEL 081 AS OF 05/01/02                      
*PHASE DEPVLEXA PVLDEXTN                                                        
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PVLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
PVLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*PVLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
PVXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    PVINIT              INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    PVXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    PVXEOF              END-OF-FILE                                  
         B     PVXIT                                                            
*                                                                               
PVXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVC   WORK,SPACES         KEEP RECORD                                  
         MVC   WORK+1(23),0(R3)                                                 
         MVI   WORK,23                                                          
         BAS   RE,DUMPREC                                                       
         B     PVXIT                                                            
*                                                                               
PVXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     PVXIT                                                            
*                                                                               
PVINIT   LA    RE,COUNTS                                                        
         L     RF,=F'15000'                                                     
         XCEF                                                                   
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
*DELETE ALL NOV/98 NHTI FILE RECDS                                              
**********************************************************************          
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         MVC   20(2,R3),=X'FFFF'   YES - SET RECLEN TO -1                       
         CLI   0(R3),C'N'                                                       
         BE    NREC                                                             
         B     PVXPURGE                                                         
*                                                                               
         USING PNKEY,R3                                                         
NREC     CLC   PNCODE,=C'NNN'                                                   
         BNE   PVXPURGE                                                         
         CLI   PNIND,0                                                          
         BNE   PVXPURGE                                                         
         CLC   PNBOOK,=X'5F05'     TEST WITH MAR98 FIRST                        
         BL    PVXPURGE                                                         
         CLI   PNBOOK+1,X'00'      BYPASS 00 BOOKS                              
         BE    PVXPURGE                                                         
         CLI   PNSTAT+4,C'N'       NAD                                          
         BE    *+12                                                             
         CLI   PNSTAT+4,C'M'       NAD-SYN                                      
         BNE   PVXPURGE                                                         
*                                                                               
         CLC   PNSTAT(3),=C'HUT'   HUT RECDS DON'T HAVE WK NIBBLE SET           
         BNE   NREC20                                                           
         CLI   PNDW,X'00'          USE M-F PTR                                  
         BNE   PVXPURGE                                                         
         MVI   PNDW,X'5F'                                                       
         MVC   20(2,R3),=X'FFFF'   YES - SET RECLEN TO -1                       
         B     PVXKEEP                                                          
*                                                                               
NREC20   CLI   PNDW,X'0F'          GEN FRI PTRS FROM M-F PTRS                   
         BNE   PVXPURGE                                                         
         MVI   PNDW,X'5F'          CREATE FRIDAY                                
         MVC   20(2,R3),=X'FFFF'   YES - SET RECLEN TO -1                       
         B     PVXKEEP                                                          
         EJECT                                                                  
***********************************************************************         
* END-OF-FILE PROCESSING                                                        
***********************************************************************         
*                                                                               
PVXEOF   DS    0H                                                               
         L     R1,APARM                                                         
         MVI   0(R1),X'FF'         PURGE RECD                                   
         L     R3,AREC             POINT TO RECORD                              
         MVC   20(2,R3),=X'FFFF'   YES - SET RECLEN TO -1                       
         MVI   PLIST,X'FF'         SET EOF                                      
         B     PVXIT                                                            
*                                                                               
         MVC   P(21),=CL21'***RECORDS SAVED***'                                 
         GOTO1 VPRINTER                                                         
         LA    R2,COUNTS                                                        
PVXEOF1  OC    0(5,R2),0(R2)                                                    
         BZ    PVXIT                                                            
         SR    R9,R9                                                            
         ICM   R9,15,5(R2)                                                      
         MVC   P(3),0(R2)                                                       
         EDIT  (R9),(8,P+10)                                                    
         ZIC   R9,3(R2)                                                         
         EDIT  (R9),(2,P+4)                                                     
         ZIC   R9,4(R2)                                                         
         EDIT  (R9),(2,P+6)                                                     
*        GOTO1 VPRINTER                                                         
         LA    R2,9(R2)                                                         
         B     PVXEOF1                                                          
         EJECT                                                                  
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
*        MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
*        LH    R5,HALF                                                          
*        CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
*        BNE   *+8                                                              
*                                                                               
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
*        GOTO1 VPRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D'              
         XIT1                                                                   
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
LOGIO    DC    V(LOGIO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
KEEPS    DC    F'0'                                                             
HEAD     DC    C'**RECORD**'                                                    
COUNTS   DS    15000C                                                           
         EJECT                                                                  
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
         SPACE 1                                                                
VPRNTBL  DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081DEPVLDEXTN05/01/02'                                      
         END                                                                    
