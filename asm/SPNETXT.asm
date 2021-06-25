*          DATA SET SPNETXT    AT LEVEL 002 AS OF 11/20/92                      
*          DATA SET SPEXTNET   AT LEVEL 008 AS OF 09/29/88                      
*PHASE SPEXNET,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE LOADER                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
******** THIS PROGRAM CHANGES C'-' TO C'0' ON PROGRTAM RECS                     
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 0,DMLDEXT                                                        
         B     DMXCTL                                                           
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
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
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         LA    R1,NTAB                                                          
         ST    R1,ANET                                                          
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLC   0(2,R3),=X'0D20'    IS IT PROGRAM RECORD                         
         BNE   DMXKEEP                                                          
         MVC   BYTE,2(R3)          IS IT NETWORK                                
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,3                                                           
         BNE   DMXKEEP                                                          
         USING NPGRECD,R3                                                       
         CLI   NPGKPROG+2,C'-'                                                  
         BNE   DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'BFOR',0(R3),C'DUMP',30,=C'1D'                 
         MVI   NPGKPROG+2,C'0'                                                  
         GOTO1 =V(PRNTBL),DMCB,=C'AFTR',0(R3),C'DUMP',30,=C'1D'                 
         B     DMXKEEP                                                          
*                                                                               
         GETEL (R4),24,ELCODE                                                   
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   MVC   P(20),=C'TOTAL RECORDS READ ='                                   
         EDIT  (P8,RECCNT),(10,P+28)                                            
         ZAP   LINE,=PL2'99'                                                    
         GOTO1 VPRINTER                                                         
         MVC   P(23),=C'TOTAL RECORDS CHANGED ='                                
         EDIT  (P8,CHACNT),(10,P+28)                                            
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
PRINTER  DC    V(PRINTER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
HEXOUT   DC    V(HEXOUT)                                                        
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
BYTE     DS    CL1                                                              
*                                                                               
RECCNT   DC    PL8'0'                                                           
CHACNT   DC    PL8'0'                                                           
*                                                                               
ELCODE   DS    XL1                                                              
*                                                                               
AELEM    DS    A                                                                
ANET     DS    A                                                                
NTAB     DC    1000XL7'00'                                                      
NTABX    EQU   *                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE SPGENPROG                                                      
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPNETXT   11/20/92'                                      
         END                                                                    
