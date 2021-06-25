*          DATA SET SPLDEXTAN2 AT LEVEL 094 AS OF 01/21/00                      
*PHASE SPEXTANN,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - FIX BUYS WITH WRONG BDSEDAY'                          
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
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         LA    R1,ESTTAB                                                        
         MVI   0(R1),X'FF'                                                      
         ST    R1,NEXTEST                                                       
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         TM    15(R3),X'80'                                                     
         BO    DMXKEEP                                                          
*                                                                               
*        CHECK CLT RECORD - SAVE CLIST                                          
*                                                                               
         CLI   0(R3),0                                                          
         BNE   DMX100                                                           
*                                                                               
         USING SPGENCLTD,R3                                                     
         OC    4(9,R3),4(R3)       IS IT A CLT RECORD                           
         BNZ   DMX10                                                            
         LA    R1,SVCLIST          THEN SAVE CLIST                              
         MVC   0(220,R1),CLIST+000                                              
         MVC   220(220,R1),CLIST+220                                            
         MVC   440(220,R1),CLIST+440                                            
         MVC   660(220,R1),CLIST+660                                            
         B     DMXKEEP                                                          
         DROP  R3                                                               
*                                                                               
*        CHECK EST RECORD - FOR OOW                                             
*                                                                               
         USING SPGENESTD,R3                                                     
DMX10    OC    EKEYEST,EKEYEST     IS THERE AN ESTIMATE                         
         BZ    DMXKEEP                                                          
         OC    EKEYEST+1(5),EKEYEST+1    NOT A BILLING REC                      
         BNZ   DMXKEEP                                                          
         CLI   EOWSDAY,0           SKIP IF NOT OOW                              
         BE    DMXKEEP                                                          
*        CLC   EEND,=C'990101'     IS ESTIMATE LATER THAN 1999 AT ALL           
*        BNH   DMXKEEP                                                          
*                                                                               
         LA    R1,SVCLIST                                                       
DMX20    CLC   EKEYPRD,0(R1)                                                    
         BE    DMX25                                                            
         LA    R1,4(R1)                                                         
         OC    0(4,R1),0(R1)       ANY MORE                                     
         BNZ   DMX20                                                            
         MVC   P(13),=C'PRODUCT ERROR'                                          
         GOTO1 =V(HEXOUT),DMCB,0(R3),P+14,13                                    
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMX25    L     R2,NEXTEST                                                       
         MVC   0(3,R2),EKEYAM      SAVE A/M,CLT                                 
         MVC   3(1,R2),3(R1)       BINARY PRD                                   
         MVC   4(1,R2),EKEYEST     ESTIMATE                                     
         MVC   5(1,R2),EOWSDAY     ROTATOR WEEK START DAY                       
*                                                                               
***      GOTO1 =V(HEXOUT),DMCB,0(R2),P+10,6                                     
***      GOTO1 VPRINTER                                                         
*                                                                               
         LA    R2,6(R2)                                                         
         MVI   0(R2),X'FF'                                                      
         ST    R2,NEXTEST                                                       
         B     DMXKEEP                                                          
         DROP  R3                                                               
*                                                                               
*        CHECK BUY RECORD                                                       
*                                                                               
         USING SPGENBUYD,R3                                                     
DMX100   CLI   0(R3),X'10'         BUY REC?                                     
         BNH   DMXKEEP                                                          
*                                                                               
*        CLC   BDEND,=X'630101'    DOES BUY ENTER 1999 AT ALL                   
*        BNH   DMXKEEP                                                          
*                                                                               
         CLI   BDINPUT,2           END DATE ENTERED                             
         BNE   DMXKEEP             NO - DON'T NEED TO FIX                       
*                                                                               
         MVI   ELCODE,X'99'        NWS TRANSFER BUY?                            
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP             NO SKIP                                      
*                                                                               
         MVI   ESTOWSDY,0                                                       
         LA    R1,ESTTAB           CHECK IF OUT OF WEEK ROTATOR                 
DMX110   CLI   0(R1),X'FF'                                                      
         BE    DMX200              NO ROTATOR                                   
         CLC   0(4,R1),BUYKAM      SAME A/M,CLT,PRD                             
         BNE   DMX120                                                           
         CLC   BUYKEST,4(R1)       SAME EST                                     
         BE    DMX130                                                           
DMX120   LA    R1,6(R1)                                                         
         B     DMX110                                                           
*                                                                               
DMX130   MVC   ESTOWSDY,5(R1)      SAVE EOWSDAY                                 
*                                                                               
DMX200   DS    0H                                                               
*                                                                               
         BAS   RE,CALCSEDY         FIX SEDAY                                    
*                                                                               
***      BAS   RE,BUYPRNT                                                       
         AP    NUMBUYS,=P'1'                                                    
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
BUYPRNT  NTR1                                                                   
         CLI   ESTOWSDY,0            PRINT OUT ALL OOW                          
         BNE   BP10                                                             
         CP    NUMBUYS,=P'25'                                                   
         BH    BPX                                                              
BP10     LA    R4,=CL20'BUY RECORD'                                             
         CLI   ESTOWSDY,0                                                       
         BE    BP20                                                             
         LA    R4,=CL20'BUY RECORD-OOW'                                         
BP20     SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
BPX      XIT1                                                                   
*                                                                               
RECPRNT  NTR1                                                                   
         LA    R4,=CL20'HEADER REC'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        FIX BDSEDAY                                                            
*                                                                               
CALCSEDY NTR1                                                                   
         USING SPGENBUYD,R2                                                     
         LR    R2,R3               POINT R2 TO BUY                              
*                                                                               
         XR    R4,R4               GET BITWISE REP OF DAYS BOUGHT               
         ICM   R4,8,BDDAY                                                       
*                                                                               
         CLI   ESTOWSDY,0          OUT OF WEEK ROTATOR?                         
         BE    CSEDY10             NO                                           
         XR    R1,R1               YES, WILL NEED TO APPEND ANOTHER             
         IC    R1,BDDAY              BDDDAY TO FIRST ONE                        
         SLL   R1,1                                                             
         STC   R1,APBYTE                                                        
         ICM   R4,4,APBYTE                                                      
         XR    R1,R1                                                            
         IC    R1,ESTOWSDY                                                      
         SLL   R4,0(R1)                                                         
         SRL   R4,25               CLEAR EVERYTHING BUT HOB                     
         SLL   R4,24                                                            
*                                                                               
CSEDY10  LA    RE,1                R4 SHOULD HAVE CORRECT BDDAY                 
         CLI   ESTOWSDY,0                                                       
         BE    *+8                                                              
         IC    RE,ESTOWSDY                                                      
         LA    R1,7                                                             
CSEDY15  SLA   R4,1                                                             
         BO    CSEDY20             ON OVERFLOW                                  
         AHI   RE,1                                                             
         BCT   R1,CSEDY15                                                       
         DC    H'0'                                                             
*                                                                               
CSEDY20  CHI   RE,7                WE HAVE OUR START DAY                        
         BNH   *+8                                                              
         SHI   RE,7                                                             
         LR    RF,RE                                                            
         SLL   RE,4                                                             
         STC   RE,APBYTE                                                        
         LR    RE,RF                                                            
         AHI   RE,1                NEXT DAY NUMBER                              
         BCTR  R1,0                ONE LESS DAY TO SHIFT                        
*                                                                               
CSEDY22  SLA   R4,1                                                             
         BZ    CSEDY30             NO MORE BITS                                 
         BO    CSEDY26                                                          
CSEDY24  AHI   RE,1                                                             
         BCT   R1,CSEDY22                                                       
         B     CSEDY30                                                          
*                                                                               
CSEDY26  LR    RF,RE               LAST DAY WITH BIT ON                         
         B     CSEDY24                                                          
*                                                                               
CSEDY30  CHI   RF,7                                                             
         BNH   *+8                                                              
         SHI   RF,7                                                             
         STC   RF,BDSEDAY                                                       
         OC    BDSEDAY,APBYTE                                                   
CSEDYXIT XIT1                                                                   
         DROP  R2                                                               
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF BUYS ='                                       
         EDIT  (P10,NUMBUYS),(15,P+25)                                          
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
NEXTEST  DC    F'0'                                                             
LASTEST  DC    F'0'                                                             
DATADISP DC    H'0024'                                                          
NUMBUYS  DC    PL10'0'                                                          
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
**                                                                              
SVCLIST  DS    CL880                                                            
ESTTAB   DS    3000CL6                                                          
**                                                                              
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
APBYTE   DS    XL1                                                              
ESTOWSDY DS    CL1                                                              
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORK2    DS    CL64                                                             
NUMRECS  DS    PL6                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*SPGENBUY                                                                       
SPGENBUYD      DSECT                                                            
       ++INCLUDE SPGENBUY                                                       
*SPGENEST                                                                       
SPGENESTD      DSECT                                                            
       ++INCLUDE SPGENEST                                                       
*SPGENCLT                                                                       
SPGENCLTD      DSECT                                                            
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094SPLDEXTAN201/21/00'                                      
         END                                                                    
