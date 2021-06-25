*          DATA SET ACLDXLIDT  AT LEVEL 013 AS OF 05/01/02                      
*PHASE ACLDXLDT,*                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACRECTYP                                                               
         TITLE 'CHANGE LIST REC (PERMANENT) EXPIRY DATE FOR YR 2000'            
*                                                                               
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
         SPACE 2                                                                
         PRINT NOGEN                                                            
ACLDLDT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDELS                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*************************************************************                   
* CONTROL FLOW LOGIC                                                            
*************************************************************                   
         SPACE 1                                                                
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
**************************************************************                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
**************************************************************                  
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
*        ZAP   RECCNT,=P'0'                                                     
         B     DMXIT                                                            
         EJECT                                                                  
**************************************************************                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
**************************************************************                  
         SPACE 1                                                                
         USING LSTRECD,R3                                                       
DMXREC   L     R3,AREC                                                          
         GOTO1 RECTYP,DMCB,(C'D',LSTRECD)                                       
         CLI   0(R1),ACRTLST                                                    
         BNE   DMXKEEP                                                          
         LR    R4,R3               SAVE ADDRESS OF RECORD                       
*                                                                               
         USING LITELD,R4                                                        
         MVI   ELCODE,LITELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   LITEDAT,=X'991231'                                               
         BNE   DMXKEEP                                                          
         MVC   LITEDAT,=X'FFFFFF'                                               
         AP    RECCNT,=P'1'                                                     
         BAS   RE,DMPGET                                                        
         B     DMXKEEP                                                          
*                                                                               
DMXREC5  MVC   P,SPACES                                                         
*        GOTO1 HEXOUT,DMCB,R3,P,L'LSTKEY,=C'TOG'                                
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
**************************************************************                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
**************************************************************                  
         SPACE 1                                                                
DMXRET   DS    0H                                                               
         EJECT                                                                  
**************************************************************                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
**************************************************************                  
         SPACE 1                                                                
DMXEOF   DS    0H                                                               
         MVC   P+10(16),=C'RECORDS CHANGED:'                                    
         EDIT (P8,RECCNT),(10,P+27),ZERO=NOBLANK                                
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
******************************************************                          
*    PRINT RECORD                                                               
******************************************************                          
         SPACE 1                                                                
DMPGET   NTR1  ,                                                                
         CP    RECCNT,=P'10'                                                    
         BH    DMPX                                                             
         L     R9,AREC                                                          
         LA    R7,=C'GET'                                                       
         MVC   HALF,LSTRLEN-LSTRECD(R9)                                         
         LH    R8,HALF                                                          
         GOTO1 VPRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                   
DMPX     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
VPRNTBL  DC    V(PRNTBL)                                                        
RECTYP   DC    V(ACRECTYP)                                                      
HEXOUT   DC    V(HEXOUT)                                                        
PRNTRECS DC    PL4'0'                                                           
DATADISP DC    H'56'                                                            
RECCNT   DC    PL8'0'              RECORD COUNT                                 
TOTDOL   DS    F                                                                
         EJECT                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL20                FOR EDIT                                     
ELCODE   DS    CL1                                                              
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* ACOPTEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACOPTEQUS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACLDXLIDT 05/01/02'                                      
         END                                                                    
