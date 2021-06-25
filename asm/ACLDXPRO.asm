*          DATA SET ACLDXPRO   AT LEVEL 019 AS OF 08/06/96                      
*PHASE ACLDXPRO,*                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRORATA                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETFACT                                                                
         TITLE 'FIX ELEMENTS BASED ON PRORATA DATA'                             
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
ACLDXPRO CSECT                                                                  
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
         USING TRNRECD,R3                                                       
DMXREC   L     R3,AREC                                                          
*                                                                               
         USING TRNELD,R2                                                        
         LA    R2,TRNRFST                                                       
         CLI   TRNEL,TRNELQ                                                     
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   TRNKUNT(2),=C'SJ'                                                
         BNE   DMXKEEP                                                          
*                                                                               
         USING TRXELD,R4                                                        
         LR    R4,R3                                                            
         MVI   ELCODE,TRXELQ       GET EXTRA STATUS ELEMENT                     
         BAS   RE,GETEL            FIND ANY 75'S?                               
         BNE   DMXKEEP             NO, NOTHING TO LOOK FOR                      
         TM    TRXSTA2,TRXSBILP    ANYTHING ALLOCATED?                          
         BZ    DMXKEEP             NOPE, DONE                                   
*                                                                               
         GOTO1 VPRORAT,DMCB,(X'40',(R3)),0,COMFACS,0,PRBLOCK,PRBLOCK2           
*                                                                               
         USING PRBLOCKD,R5                                                      
         LA    R5,PRBLOCK                                                       
         CP    PP$AALLO,=P'0'      ANYTHING ALLOCATED?                          
         BNE   DMXKEEP             YES, ALL IS WELL                             
*                                  NO, TURN OFF BIT                             
         NI    TRXSTA2,X'FF'-TRXSBILP                                           
         NI    TRNRSTA2,X'FF'-TRNSBILP                                          
*                                                                               
         AP    CONCNT,=P'1'        YES, ADD TO ERROR COUNT                      
         BAS   RE,DMPPUT           PRINT THE RECORD                             
         B     DMXKEEP             GET NEXT RECORD                              
*                                                                               
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P(16),=C'ELEMENTS CHANGED'                                       
         EDIT  CONCNT,(11,P+20)                                                 
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
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
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
         EJECT                                                                  
DMPGET   NTR1                                                                   
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         LA    R7,=C'GET '                                                      
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         L     R9,AREC                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
         LA    R7,=C'PUT '                                                      
         B     DUMP                                                             
*                                                                               
DMPERR   NTR1  ,                                                                
         L     R9,AREC                                                          
         LA    R7,=C'ERR '                                                      
         B     DUMP                                                             
*                                                                               
DUMP     MVC   HALF,ACCRLEN-ACCRECD(R9)                                         
         LH    R8,HALF                                                          
         GOTO1 VPRNTBL,DMCB,(4,(R7)),(R9),C'DUMP',(R8),=C'2D'                   
*                                                                               
DUMPX    XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
VPRNTBL  DC    V(PRNTBL)                                                        
VPRORAT  DC    V(PRORATA)                                                       
CONCNT   DC    PL4'0'                                                           
ERRCNT   DC    PL4'0'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'200'                                                         
DATADISP DC    H'56'                                                            
SAVENAME DS    XL64                                                             
*                                                                               
         DS    0D                                                               
PRBLOCK  DC    (PRBLOCKL)X'00'                                                  
PRBLOCK2 DC    2048X'00'                                                        
*                                                                               
         EJECT                                                                  
COMFACS  DS    0F                                                               
       ++INCLUDE DDCOMFACSC                                                     
*                                                                               
         EJECT                                                                  
         ENTRY MASTC                                                            
MASTC    DS    0F                                                               
       ++INCLUDE DDMASTC                                                        
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
WORKX    EQU   *                                                                
         EJECT                                                                  
         PRINT OFF                                                              
PRBLOCKD DSECT                                                                  
       ++INCLUDE ACPRORATAD                                                     
PRBLOCKL EQU   *-PRBLOCKD                                                       
         PRINT ON                                                               
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
**PAN#1  DC    CL21'019ACLDXPRO  08/06/96'                                      
         END                                                                    
