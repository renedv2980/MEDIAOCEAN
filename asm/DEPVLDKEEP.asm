*          DATA SET DEPVLDKEEP AT LEVEL 066 AS OF 08/23/00                      
*PHASE DEPVLKPA PVLDKEEP                                                        
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
         SPACE 2                                                                
PVXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     PVXIT                                                            
         SPACE 2                                                                
PVXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     PVXIT                                                            
         SPACE 2                                                                
PVINIT   LA    RE,COUNTS                                                        
         L     RF,=F'15000'                                                     
         XCEF                                                                   
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1 1                                                                
         EJECT                                                                  
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLI   0(R3),C'P'                                                       
         BE    PREC                                                             
         CLI   0(R3),C'Q'                                                       
         BE    QREC                                                             
         CLI   0(R3),C'N'                                                       
         BE    NREC                                                             
         B     PVXPURGE                                                         
         SPACE                                                                  
         USING PRKEY,R3                                                         
PREC     CLI   PRCODE,C'P'                                                      
         BNE   PVXPURGE                                                         
         CLI   PRMEDIA,C'T'                                                     
         BNE   PVXPURGE                                                         
         CLI   PRSRC,C'N'                                                       
         BNE   PVXPURGE                                                         
         MVC   DUB(3),PRKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PRBOOK     SAVE BOOK                                    
         CLC   PRBOOK,KEEPBOOK                                                  
         BE    PVXREC51                                                         
         B     PVXPURGE                                                         
         SPACE                                                                  
         USING PMKEY,R3                                                         
QREC     CLI   PMCODE,C'Q'                                                      
         BNE   PVXPURGE                                                         
         CLI   PMMEDIA,C'T'                                                     
         BNE   PVXPURGE                                                         
         CLI   PMSRC,C'N'                                                       
         BNE   PVXPURGE                                                         
         MVC   DUB(3),PMKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PMBOOK     SAVE BOOK                                    
         CLC   PMBOOK,KEEPBOOK                                                  
         BE    PVXREC51            ADD TO TABLE AND KEEP RECD                   
         B     PVXPURGE                                                         
         SPACE                                                                  
         USING PNKEY,R3                                                         
NREC     CLI   PNMEDIA,C'T'                                                     
         BNE   PVXPURGE                                                         
         CLI   PNSRC,C'N'                                                       
         BNE   PVXPURGE                                                         
         MVC   DUB(3),PNKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PNBOOK     SAVE BOOK                                    
         CLC   PNBOOK,KEEPBOOK                                                  
         BE    PVXREC51              ADD TO TABLE AND KEEP RECD                 
         B     PVXPURGE                                                         
         SPACE                                                                  
*                                                                               
         USING PRKEY,R3                                                         
PVXREC51 LA    RE,COUNTS                                                        
PVXREC5A OC    0(5,RE),0(RE)       OPEN SLOT                                    
         BZ    PVXREC5B                                                         
         CLC   DUB(5),0(RE)        FIND THE SLOT                                
         BE    PVXREC5C                                                         
         LA    RE,9(RE)                                                         
         B     PVXREC5A                                                         
PVXREC5B MVC   0(5,RE),DUB         SET KEY IN TABLE                             
PVXREC5C SR    RF,RF               ADD UP RECORDS                               
         ICM   RF,15,5(RE)                                                      
         A     RF,=F'1'                                                         
         STCM  RF,15,5(RE)                                                      
         B     PVXKEEP                                                          
         DROP  R3                                                               
         EJECT                                                                  
* END-OF-FILE PROCESSING                                                        
*                                                                               
PVXEOF   MVC   P(21),=CL21'***RECORDS SAVED***'                                 
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
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
KEEPBOOK DC    AL1(90,10)                                                       
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
**PAN#1  DC    CL21'066DEPVLDKEEP08/23/00'                                      
         END                                                                    
