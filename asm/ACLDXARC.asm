*          DATA SET ACLDXARC   AT LEVEL 018 AS OF 05/01/02                      
*PHASE ACLDARC,*                                                                
*INCLUDE ACRECTYP                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'ACLD EXTERNAL - UN ARCHIVE ITEMS'                               
         SPACE 2                                                                
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
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
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
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
ACCF     OPEN  (RCVTAPE,(OUTPUT))                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   L     R2,AREC                                                          
         USING SESRECD,R2                                                       
         CLC   SESKEY(2),=XL2'2C3C'                                             
         BNE   DMXKEEP                                                          
*                                                                               
         AP    SESRECS,=P'1'                                                    
*                                                                               
         TM    SESRSTA,X'40'       ARCHIVED SESSION REC                         
         BNO   DMXKEEP                                                          
*                                                                               
         AP    ARCRECS,=P'1'                                                    
         BAS   RE,DMPGET                                                        
         NI    SESRSTA,X'80'                                                    
         BAS   RE,DMPPUT                                                        
*                                                                               
         B     DMXKEEP             KEEP ON TAPE                                 
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         CLOSE (RCVTAPE)                                                        
         ZAP   LINE,=P'99'                                                      
         MVI   P,C' '                                                           
         GOTO1 VPRINTER                                                         
         MVC   P(11),=C'UNARCHIEVED '                                           
         UNPK  P+13(6),ARCRECS                                                  
         OI    P+18,X'F0'                                                       
         MVC   P+30(11),=C'OUT OF      '                                        
         UNPK  P+43(6),SESRECS                                                  
         OI    P+48,X'F0'                                                       
         GOTO1 VPRINTER                                                         
         MVI   P,C' '                                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DMPGET   NTR1  ,                                                                
*        AP    DUMPCNT,=P'1'                                                    
*        ZAP   DUB,DUMPCNT                                                      
*        DP    DUB,EVERY                                                        
*        CP    DUB+4(4),=P'0'                                                   
*        BNE   DUMPX                                                            
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         LA    R7,=C'GET'                                                       
         MVC   HALF,SESRLEN                                                     
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1  ,                                                                
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
*        ZAP   DUB,DUMPCNT                                                      
*        DP    DUB,EVERY                                                        
*        CP    DUB+4(4),=P'0'                                                   
*        BNE   DUMPX                                                            
         LA    R7,=C'PUT'                                                       
         MVC   HALF,SESRLEN                                                     
         SPACE 1                                                                
DUMP     LH    R8,HALF                                                          
         GOTO1 VPRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                   
         SPACE 1                                                                
DUMPX    XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
VRECTYP  DC    V(ACRECTYP)                                                      
VPRNTBL  DC    V(PRNTBL)                                                        
VACCEMU  DC    V(DMACCEMU)                                                      
SESRECS  DC    PL4'0'                                                           
ARCRECS  DC    PL4'0'                                                           
ADDRECS  DC    PL4'0'                                                           
ERRRECS  DC    PL4'0'                                                           
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'01'                                                          
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'1000'                                                        
SAVENAME DS    XL64                                                             
         EJECT                                                                  
*              DCB HISTORY TAPE                                                 
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
RECTYPE  DS    XL1                                                              
SV4      DS    XL4                                                              
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
IOF      DS    F                                                                
IO       DS    2000C                                                            
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
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACLDXARC  05/01/02'                                      
         END                                                                    
