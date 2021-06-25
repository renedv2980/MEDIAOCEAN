*          DATA SET DEPVLDEXM2 AT LEVEL 089 AS OF 05/01/02                      
*PHASE PVLDEXTJ                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
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
PVXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     PVXIT                                                            
*                                                                               
PVXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
*                                                                               
         MVC   WORK,SPACES         KEEP RECORD                                  
         MVC   WORK+1(23),0(R3)                                                 
         MVI   WORK,23                                                          
*        BAS   RE,DUMPREC                                                       
*                                                                               
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
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PJKEY,R3                                                         
         CLC   0(3,R3),=C'JCN'                                                  
         BNE   PVXPURGE                                                         
         CLC   PJSTAT,=C'6217C'                                                 
         BNE   PVX7235                                                          
         OC    PJBOOK,PJBOOK                                                    
         BZ    PVXKEEP             BOOK <> 0 ONLY                               
         CLC   PJBOOK,=X'6422'                                                  
         BE    PVXKEEP                                                          
         CLC   PJBOOK,=X'6421'                                                  
         BE    PVXKEEP                                                          
         CLC   PJBOOK,=X'6420'                                                  
         BE    PVXKEEP                                                          
         B     PVXPURGE                                                         
*                                                                               
PVX7235  CLC   PJSTAT,=C'7235C'                                                 
         BNE   PVX7133             DELETE ALL J'S                               
         OC    PJBOOK,PJBOOK                                                    
         BZ    PVXKEEP             BOOK <> 0 ONLY                               
         CLC   PJBOOK,=X'6417'                                                  
         BE    PVXKEEP                                                          
         CLC   PJBOOK,=X'6419'                                                  
         BE    PVXKEEP                                                          
         CLC   PJBOOK,=X'641A'                                                  
         BE    PVXKEEP                                                          
         CLC   PJBOOK,=X'641B'                                                  
         BE    PVXKEEP                                                          
         B     PVXPURGE                                                         
*                                                                               
PVX7133  CLC   PJSTAT,=C'7133C'                                                 
         BNE   PVX6221                                                          
         OC    PJBOOK,PJBOOK                                                    
         BZ    PVXKEEP             BOOK <> 0 ONLY                               
         CLC   PJBOOK,=X'641A'                                                  
         BE    PVXKEEP                                                          
         B     PVXPURGE                                                         
*                                                                               
PVX6221  CLC   PJSTAT,=C'6221C'                                                 
         BNE   PVX5830                                                          
         OC    PJBOOK,PJBOOK                                                    
         BZ    PVXKEEP             BOOK <> 0 ONLY                               
         CLC   PJBOOK,=X'641A'                                                  
         BE    PVXKEEP                                                          
         B     PVXPURGE                                                         
*                                                                               
PVX5830  CLC   PJSTAT,=C'5830C'                                                 
         BNE   PVX6390                                                          
         OC    PJBOOK,PJBOOK                                                    
         BZ    PVXKEEP             BOOK <> 0 ONLY                               
         CLC   PJBOOK,=X'641B'                                                  
         BE    PVXKEEP                                                          
         B     PVXPURGE                                                         
*                                                                               
PVX6390  CLC   PJSTAT,=C'6390C'                                                 
         BNE   PVX                                                              
         OC    PJBOOK,PJBOOK                                                    
         BZ    PVXKEEP             BOOK <> 0 ONLY                               
         CLC   PJBOOK,=X'641B'                                                  
         BE    PVXKEEP                                                          
         B     PVXPURGE                                                         
*                                                                               
PVX      B     PVXPURGE                                                         
*                                                                               
**********************************************************************          
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
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
*        MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
*        LH    R5,HALF                                                          
*        CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
*        BNE   *+8                                                              
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         GOTO1 VPRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D'              
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
SVKEY    DS    CL18                                                             
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
**PAN#1  DC    CL21'089DEPVLDEXM205/01/02'                                      
         END                                                                    
