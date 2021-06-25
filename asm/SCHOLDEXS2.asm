*          DATA SET SCHOLDEXS2 AT LEVEL 101 AS OF 06/05/00                      
*PHASE SCHOEXTJ,*                                                               
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
         CLC   PJCODE(8),=C'JNNPPPPS'    J-KEYS                                 
         BNE   PVXPURGE                                                         
*                                                                               
         LA    R2,KEYTAB           LOOKING THROUGH HARD CODE TABLE OF           
QREC05   CLI   0(R2),X'FF'         17 KEYS.                                     
         BE    PVXPURGE                                                         
         CLC   0(18,R3),0(R2)      PURGING MATCHS                               
         BE    QREC10                                                           
         LA    R2,L'KEYTAB(R2)                                                  
         B     QREC05                                                           
*                                                                               
QREC10   MVC   PJINTNUM,18(R2)     REPLACE THE NUMBER                           
         B     PVXKEEP                                                          
*                                                                               
PVXEOF   DS    0H                                                               
         L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'         PURGE RECD AND EXIT                          
         B     PVXIT                                                            
*                                                                               
*        MVC   P(21),=CL21'***RECORDS SAVED***'                                 
*        GOTO1 VPRINTER                                                         
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
KEYTAB   DS    0CL20                                                            
         DC    X'D1D5D5D7D7D7D7E2000000000007005904B1',X'09C5'                  
         DC    X'D1D5D5D7D7D7D7E2000000000008177604B2',X'09C6'                  
         DC    X'D1D5D5D7D7D7D7E2000000000008225804B3',X'09C7'                  
         DC    X'D1D5D5D7D7D7D7E2000000000008230904B4',X'09C8'                  
         DC    X'D1D5D5D7D7D7D7E2000000000009311504B5',X'09C9'                  
         DC    X'D1D5D5D7D7D7D7E2000000000009868004B6',X'09CA'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010255404B7',X'09CB'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010255504B8',X'09CC'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010308604B9',X'09CD'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010308804BA',X'09CE'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010309004BB',X'09CF'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010309304BC',X'09D0'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010309504BD',X'09D1'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010319904BE',X'09D2'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010320304BF',X'09D3'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010320504C0',X'09D4'                  
         DC    X'D1D5D5D7D7D7D7E2000000000010320604C1',X'09D5'                  
         DC    X'FF'                                                            
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
**PAN#1  DC    CL21'101SCHOLDEXS206/05/00'                                      
         END                                                                    
