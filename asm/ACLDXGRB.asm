*          DATA SET ACLDXGRB   AT LEVEL 002 AS OF 10/17/97                      
*PHASE ACLDXGRB,*                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACRECTYP                                                               
*                                                                               
         TITLE 'REDO GROUP BILLING RECORD DATES FOR YEAR 2000'                  
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALIZE                           
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
         PRINT NOGEN                                                            
ACLDXGRB CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDELS                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
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
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
FILENAME DC    CL7'       '                                                     
*                                                                               
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
         USING GRBRECD,R4                                                       
DMXREC   L     R4,AREC                                                          
         GOTO1 =V(ACRECTYP),DMCB,(C'D',GRBRECD)                                 
         CLI   0(R1),ACRTGRB       GROUP BILLING RECORD?                        
         BNE   DMXKEEP             NOPE                                         
*                                                                               
*        BAS   RE,DMPGET           DUMP RECORD AS IT COME IN                    
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,=X'999999'                                                  
         SR    RF,RF                                                            
         ICM   RF,7,GRBKBILD                                                    
         SR    RE,RF               BRING DATE BACK TO PL3                       
*                                                                               
         LNR   RE,RE                                                            
         STCM  RE,7,GRBKBILD       STORE IT BACK AS 2'S COMPLEMENT              
*                                                                               
*        BAS   RE,DMPPUT           PRINT NEW RECORD                             
*                                                                               
         AP    CHGCNT,=P'1'        ADD ONE TO CHANGE COUNTER                    
         B     DMXKEEP             KEEP WITH THE NEW KEY                        
*                                                                               
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P(15),=C'RECORDS CHANGED'                                        
         EDIT  CHGCNT,(11,P+18)                                                 
         GOTO1 VPRINTER                                                         
         MVI   P,C' '                                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DELETE AN ELEMENT                                            *         
*                                                                     *         
*              P1   BYTE 0    ELEMENT CODE                            *         
*                   BYTE 1-3  A(RECORD)                               *         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT               *         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                      *         
*        GOTO1 DELEL,DMCB,('PTRELQ',AREC),0                                     
*---------------------------------------------------------------------*         
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 =V(HELLO),DMCB,(C'D',=C'ACCMST  '),((R4),(R2)),((R5),(R3X        
               ))                                                               
         B     EXIT                                                             
         SPACE 5                                                                
*---------------------------------------------------------------------*         
*        ADD AN ELEMENT                                               *         
*                                                                     *         
*              P1   A(RECORD)                                         *         
*              P2   A(ELEMENT)                                        *         
*---------------------------------------------------------------------*         
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 =V(HELLO),DMCB,(C'P',=C'ACCMST  '),(R2),(R3)                     
         B     EXIT                                                             
         SPACE 5                                                                
*---------------------------------------------------------------------*         
*        FIND AN ELEMENT                                              *         
*                                                                     *         
*              P1   BYTE 0    ELEMENT CODE                            *         
*                   BYTE 1-3  A(RECORD)                               *         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT               *         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                      *         
*---------------------------------------------------------------------*         
FNDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 =V(HELLO),DMCB,(C'G',=C'ACCBIG  '),((R4),(R2)),((R5),(R3X        
               ))                                                               
         B     EXIT                                                             
         SPACE 5                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
DMPGET   NTR1                                                                   
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         LA    R7,=C'GET '                                                      
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         LA    R7,=C'PUT '                                                      
         B     DUMP                                                             
*                                                                               
DUMP     LH    R8,HALF                                                          
         GOTO1 VPRNTBL,DMCB,(4,(R7)),(R9),C'DUMP',(R8),=C'2D'                   
*                                                                               
DUMPX    XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
VPRNTBL  DC    V(PRNTBL)                                                        
CHGCNT   DC    PL4'0'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'200'                                                         
DATADISP DC    H'56'                                                            
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
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
WORK     DS    CL64                                                             
ELCODE   DS    CL1                                                              
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
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACLDXGRB  10/17/97'                                      
         END                                                                    
