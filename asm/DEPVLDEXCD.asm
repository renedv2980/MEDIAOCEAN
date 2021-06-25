*          DATA SET DEPVLDEXCD AT LEVEL 046 AS OF 08/23/00                      
*PHASE DEPVLE6A PVLDEXT6                                                        
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
         B     PVXKEEP                                                          
         SPACE                                                                  
         USING PRKEY,R3                                                         
PREC     CLI   PRMEDIA,C'N'                                                     
         BNE   PCKHISP                                                          
         MVC   DUB(3),PRKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PRBOOK     SAVE BOOK                                    
         CLC   PRBOOK,=X'6024'                                                  
         BNE   PVXKEEP                                                          
         CLC   PRSTAT(5),=C'CAT D'                                              
         BE    PVXREC51                                                         
         B     PVXKEEP                                                          
         SPACE                                                                  
PCKHISP  CLI   PRMEDIA,C'W'                                                     
         BNE   PVXKEEP                                                          
         B     PVXKEEP                                                          
         MVC   DUB(3),PRKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PRBOOK     SAVE BOOK                                    
         CLC   PRBOOK,=X'5620'                                                  
         BL    PVXREC51                                                         
         CLC   PRBOOK,=X'5F0A'                                                  
         BH    PVXREC51                                                         
         B     PVXKEEP                                                          
         SPACE                                                                  
         USING PMKEY,R3                                                         
QREC     CLI   PMMEDIA,C'N'                                                     
         BNE   QCKHISP                                                          
         MVC   DUB(3),PMKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PMBOOK     SAVE BOOK                                    
         CLI   PMSRC,C'N'                                                       
         BNE   PVXKEEP                                                          
         CLC   PMBOOK,=X'6024'                                                  
         BNE   PVXKEEP                                                          
         CLC   PMSTAT(5),=C'CAT D'                                              
         BE    PVXREC51                                                         
         B     PVXKEEP                                                          
         SPACE                                                                  
QCKHISP  CLI   PMMEDIA,C'W'                                                     
         BNE   PVXKEEP                                                          
         B     PVXKEEP                                                          
         MVC   DUB(3),PMKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PMBOOK     SAVE BOOK                                    
         CLC   PMBOOK,=X'5F0A'                                                  
         BH    PVXREC51                                                         
         B     PVXKEEP                                                          
         SPACE                                                                  
         USING PNKEY,R3                                                         
NREC     CLI   PNMEDIA,C'N'                                                     
         BNE   PVXKEEP                                                          
         MVC   DUB(3),PNKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PNBOOK     SAVE BOOK                                    
         CLI   PNSRC,C'N'                                                       
         BNE   PVXKEEP                                                          
         CLC   PNBOOK,=X'6024'                                                  
         BNE   PVXKEEP                                                          
         CLC   PNSTAT,=C'CAT D'                                                 
         BE    PVXREC51                                                         
         B     PVXKEEP                                                          
         SPACE                                                                  
PVXREC2  L     R1,KEEPS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,KEEPS                                                         
         SR    R0,R0                                                            
         D     R0,=F'20'           DUMP 1 OF EVERY 20 RECORDS                   
         LTR   R0,R0               TEST FOR REMAINDER                           
         BNZ   PVXREC4             YES                                          
         MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
         LH    R5,HALF                                                          
         CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
         BNE   *+8                                                              
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         LA    R2,L'HEAD                                                        
         MVC   WORK(L'HEAD),HEAD                                                
         GOTO1 VPRNTBL,DMCB,((R2),WORK),(R3),C'DUMP',(R5),=C'2D'                
         SPACE                                                                  
PVXREC4  B     PVXKEEP                                                          
         EJECT                                                                  
         USING PRKEY,R3                                                         
PVXREC5  MVC   DUB(3),PRKEY        SAVE CODE/MEDIA/SOURCE                       
         MVC   DUB+3(2),PRBOOK     SAVE BOOK                                    
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
         B     PVXPURGE                                                         
         DROP  R3                                                               
         EJECT                                                                  
* END-OF-FILE PROCESSING                                                        
*                                                                               
PVXEOF   MVC   P(21),=C'***RECORDS DELETED***'                                  
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
         GOTO1 VPRINTER                                                         
         LA    R2,9(R2)                                                         
         B     PVXEOF1                                                          
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
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
**PAN#1  DC    CL21'046DEPVLDEXCD08/23/00'                                      
         END                                                                    
