*          DATA SET ACLDHST    AT LEVEL 001 AS OF 02/04/93                      
*PHASE ACLDHST,*                                                                
*INCLUDE ACRECTYP                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'ACLD EXTERNAL - TO CREATE TAPE FOR PEELED ITEMS'                
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
* P7=A(CARDS)                                                                   
* P8=A(PEELDATE)                                                                
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,CLEAR=YES                                    
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
ACCF     OPEN  (HSTTAPE,(OUTPUT))                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   L     R2,AREC                                                          
         USING TRNRECD,R2                                                       
         GOTO1 VRECTYP,DMCB,(C'D',TRNRECD)                                      
         MVC   RECTYPE,0(R1)                                                    
         CLI   RECTYPE,ACRTTRN     TEST TRANSACTION                             
         BNE   DMXKEEP                                                          
         LA    R3,TRNRFST                                                       
         CLI   0(R3),TRNELQ       IS THERE A TRANSACTION ELEMENT                
         BNE   DMXKEEP                                                          
         SR    R0,R0                                                            
*                                                                               
DMXREC3  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             END OF RECORD                                
         BE    DMXKEEP                                                          
         USING TRSELD,R3                                                        
         CLI   TRSEL,TRSELQ        STATUS ELEMENT                               
         BNE   DMXREC3                                                          
         OC    TRSPDAT,TRSPDAT     HAS IT BEEN PEELED                           
         BZ    DMXKEEP                                                          
         L     RE,APEELDTE         A(PEELDATE)                                  
         OC    0(2,RE),0(RE)                                                    
         BZ    DMXKEEP             NO PEELED DATE                               
         CLC   TRSPDAT,0(RE)       TEST DATE TRANSACTION PEELED                 
         BH    DMXKEEP                                                          
*                                                                               
         USING ACCRECD,R2          R2=A(NEW INPUT RECORD)                       
         LA    R4,IO                                                            
         USING ACKEYD,R4           R4=A(OLD OUTPUT RECORD)                      
         MVC   ACSTATUS,ACCRSTA                                                 
         XC    ACDTUSED,ACDTUSED                                                
         XC    ACDTPEEL,ACDTPEEL                                                
         MVC   ACKEYACC(L'ACCKEY),ACCKEY                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,ACCRLEN        R1=NEW FILE RECORD LENGTH                    
         SH    R1,NEWSYS                                                        
         CH    R1,OLDMAX           TEST DATA LENGTH TOO BIG                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         STH   R1,SAVELEN                                                       
         LA    R0,ACRECORD                                                      
         LA    RE,ACCRFST                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE DATA TO OLD RECORD                      
         LR    R1,R0               R1=A(EOR)                                    
         XC    0(2,R1),0(R1)       MAKE SURE EOR IS ZEROED                      
         LH    R1,SAVELEN                                                       
         AH    R1,OLDSYS                                                        
         STCM  R1,3,ACLENGTH       SET OLD RECORD LENGTH                        
         MVC   ACDTUSED,TRSUDAT    SET USED & PEELED DATES                      
         MVC   ACDTPEEL,TRSPDAT                                                 
*                                                                               
         LA    R1,28(R1)           ADD RECOVERY HEADER                          
         STCM  R1,3,IOLEN                                                       
         LA    R5,IOLEN                                                         
         LA    R8,HSTTAPE                                                       
         PUT   (R8),(R5)                                                        
         AP    PELRECS,=P'1'                                                    
         B     DMXPURGE            PURGE OLD RECORD                             
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         CLOSE (HSTTAPE)                                                        
         ZAP   LINE,=P'99'                                                      
         MVI   P,C' '                                                           
         GOTO1 VPRINTER                                                         
         MVC   P(11),=C'PEELED      '                                           
         UNPK  P+13(6),PELRECS                                                  
         OI    P+18,X'F0'                                                       
         MVC   P+30(11),=C'ERRORS      '                                        
         UNPK  P+43(6),ERRRECS                                                  
         OI    P+48,X'F0'                                                       
         GOTO1 VPRINTER                                                         
         MVI   P,C' '                                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DMPGET   NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   DUMPX                                                            
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         LA    R7,=C'GET'                                                       
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1  ,                                                                
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   DUMPX                                                            
         LA    R7,=C'PUT'                                                       
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         SPACE 1                                                                
DUMP     LH    R8,HALF                                                          
         GOTO1 VPRNTBL,DMCB,(3,(R7)),AREC,C'DUMP',(R8),=C'2D'                   
         SPACE 1                                                                
DUMPX    XIT1  ,                                                                
         EJECT                                                                  
VRECTYP  DC    V(ACRECTYP)                                                      
VPRNTBL  DC    V(PRNTBL)                                                        
*                                                                               
NEWMAX   DC    Y(2000-(ACCRFST-ACCRECD))                                        
OLDMAX   DC    Y(2000-(ACRECORD-ACKEYD))                                        
NEWSYS   DC    Y(ACCRFST-ACCRECD)                                               
OLDSYS   DC    Y(ACRECORD-ACKEYD)                                               
*                                                                               
PELRECS  DC    PL4'0'                                                           
ERRRECS  DC    PL4'0'                                                           
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'05'                                                          
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'100'                                                         
*                                                                               
IOLEN    DC    F'0'                LENGTH                                       
IORCV    DC    X'6103',22X'00'     RECOVERY HEADER(ACCOUNT - ADD)               
IO       DS    2000C                                                            
         SPACE 2                                                                
*              DCB HISTORY TAPE                                                 
*                                                                               
HSTTAPE  DCB   DDNAME=HSTTAPE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=8200,BLKSIZE=8204                                 
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
SAVELEN  DS    H                                                                
RECTYPE  DS    XL1                                                              
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL32                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VCARDS   DS    A                                                                
APEELDTE DS    A                                                                
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
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
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
**PAN#1  DC    CL21'001ACLDHST   02/04/93'                                      
         END                                                                    
