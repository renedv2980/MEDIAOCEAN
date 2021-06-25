*          DATA SET STEXTRE    AT LEVEL 014 AS OF 02/24/00                      
*          DATA SET STEXTNE    AT LEVEL 011 AS OF 10/31/95                      
*PHASE STEXTRE,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* SEARCH FOR ALL RECORDS WITH DUPLICATE ELEMENTS                                
* 02/24/95                                                                      
         SPACE                                                                  
*                                                                               
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
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
         MVI   BYTE,0                                                           
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
         SPACE                                                                  
         CLI   0(R3),X'0A'         REVISION RECORD                              
         BE    DMXREC20                                                         
         SPACE                                                                  
         LA    R4,=CL20'NON-TRAFFIC REC'                                        
         SPACE                                                                  
         AP    TOTNOT,=P'1'                                                     
         SPACE                                                                  
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE                                                                  
DMXREC20 DS    0H                                                               
         SPACE                                                                  
         LA    R4,24(,R3)                                                       
         SPACE                                                                  
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)                                                      
         LA    RF,0(R1,R3)                                                      
         BCTR  RF,0                                                             
         MVI   0(RF),0                                                          
         SPACE                                                                  
DMXREC30 DS    0H                                                               
         CLI   0(R4),X'F1'                                                      
         BE    DMXREC40                                                         
         CLI   0(R4),0             END OF REC                                   
         BNE   DMXREC34                                                         
         AP    TOTMF1,=P'1'                                                     
         LA    R4,=CL20'NO F1 ACT ELEM'                                         
         B     PRT                                                              
DMXREC34 DS    0H                                                               
         LR    R2,R4               SAVE LAST ELEM ADDR                          
         SPACE                                                                  
         SR    R7,R7                                                            
         IC    R7,1(R4)                                                         
         AR    R4,R7                                                            
         SPACE                                                                  
         CLC   0(2,R2),0(R4)                                                    
         BNE   DMXREC30                                                         
         SPACE                                                                  
         LR    R6,R7                                                            
         BCTR  R7,0                                                             
         SPACE                                                                  
         EX    R7,CKELEM                                                        
         SPACE                                                                  
         BNE   DMXREC30                                                         
         SPACE                                                                  
         AP    TOTEQE,=P'1'                                                     
         MVI   BYTE,1                                                           
         MVC   P+1(5),=C'ELM 1'                                                 
         GOTO1 =V(HEXOUT),DMCB,(R2),P+7,(R6),0,0                                
         GOTO1 VPRINTER                                                         
         MVC   P+1(5),=C'ELM 2'                                                 
         GOTO1 =V(HEXOUT),DMCB,(R4),P+7,(R6),0,0                                
         GOTO1 VPRINTER                                                         
         B     DMXREC30                                                         
         SPACE                                                                  
CKELEM   CLC   0(0,R2),0(R4)                                                    
         SPACE                                                                  
DMXREC40 DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END OF REC                                   
         BNE   DMXREC50                                                         
         CLI   BYTE,0                                                           
         BE    DMXKEEP                                                          
         LA    R4,=CL20'DUP EQ ELMS'                                            
         B     PRT                                                              
         SPACE                                                                  
DMXREC50 DS    0H                                                               
         LA    R4,=CL20'BAD F1 REC'                                             
         SPACE                                                                  
         AP    TOTBF1,=P'1'                                                     
         CLI   BYTE,0                                                           
         BE    PRT                                                              
         LA    R4,=CL20'BAD F1/EQ ELM'                                          
         SPACE                                                                  
PRT      DS    0H                                                               
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         LA    R2,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(28),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+33)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,33(,R3)                                                       
         BCT   R2,DMXEOF10                                                      
         SPACE                                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
TOTRD    DC    PL5'0',CL28'TOT RECS READ'                                       
TOTNOT   DC    PL5'0',CL28'NOT TRAF RECS'                                       
TOTEQE   DC    PL5'0',CL28'DUP/EQ ELEMS'                                        
TOTMF1   DC    PL5'0',CL28'MISS F1 RECS'                                        
TOTBF1   DC    PL5'0',CL28'BAD F1 RECS'                                         
TOTCTRS  EQU   (*-TOTRD)/33                                                     
          SPACE                                                                 
WORK     DS    CL64                                                             
         SPACE                                                                  
RTITLE   DC    CL100'FIND DUPLICATE ELEMENTS IN TRAFFIC RECS'                   
         LTORG                                                                  
          SPACE                                                                 
         SPACE 2                                                                
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
BYTE     DS    C                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
STARECD   DSECT                                                                 
       ++INCLUDE SPGENSTA                                                       
ADDRECD   DSECT                                                                 
       ++INCLUDE SPGENADD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014STEXTRE   02/24/00'                                      
         END                                                                    
