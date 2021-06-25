*          DATA SET SPLDEXTAN4 AT LEVEL 091 AS OF 10/19/00                      
*PHASE SPEXTAN4,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - DELETE ALL COKEAT GOALS FOR 2000 ESTIMATES'           
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
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
         CLI   0(R3),X'02'         GOAL RECORD                                  
         BNE   DMXKEEP             NO, KEEP IT                                  
         CLI   1(R3),X'B1'         COKEAT MEDIA T                               
         BE    DMXR10                                                           
         CLI   1(R3),X'B2'         COKEAT MEDIA R                               
         BNE   DMXKEEP             NO, KEEP IT                                  
*                                                                               
DMXR10   LA    R4,ESTTABLE         DELETE 1998 ESTIMATES                        
DMXR20   CLI   0(R4),X'FF'                                                      
         BE    DMXKEEP                                                          
         CLC   7(1,R3),1(R4)                                                    
         BE    DMXR30                                                           
         LA    R4,2(R4)                                                         
         B     DMXR20                                                           
*                                                                               
DMXR30   DS    0H                                                               
*        BAS   RE,GOALPRNT                                                      
         AP    NUMRECS,=P'1'                                                    
         B     DMXPURGE                                                         
*                                                                               
GOALPRNT NTR1                                                                   
         GOTO1 =V(HEXOUT),DMCB,0(R3),P+6,13                                     
         EDIT  (B1,7(R3)),(3,P+2),FILL=0                                        
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
RECPRNT  NTR1                                                                   
         LA    R4,=CL20'GOAL RECORD'                                            
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'HEADER REC'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF RECS ='                                       
         EDIT  (P6,NUMRECS),(12,P+25)                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DATADISP DC    H'0024'                                                          
*                                                                               
ESTTABLE DC    H'006'                                                           
         DC    H'008'                                                           
         DC    H'014'                                                           
         DC    H'019'                                                           
         DC    H'024'                                                           
         DC    H'027'                                                           
         DC    H'039'                                                           
         DC    H'084'                                                           
         DC    H'101'                                                           
         DC    H'102'                                                           
         DC    H'103'                                                           
         DC    H'104'                                                           
         DC    H'111'                                                           
         DC    H'112'                                                           
         DC    H'113'                                                           
         DC    H'114'                                                           
         DC    H'121'                                                           
         DC    H'122'                                                           
         DC    H'123'                                                           
         DC    H'124'                                                           
         DC    H'129'                                                           
         DC    H'130'                                                           
         DC    H'131'                                                           
         DC    H'132'                                                           
         DC    H'133'                                                           
         DC    H'134'                                                           
         DC    H'135'                                                           
         DC    H'136'                                                           
         DC    H'137'                                                           
         DC    H'138'                                                           
         DC    H'139'                                                           
         DC    H'140'                                                           
         DC    H'142'                                                           
         DC    H'143'                                                           
         DC    H'144'                                                           
         DC    H'145'                                                           
         DC    H'146'                                                           
         DC    H'147'                                                           
         DC    H'148'                                                           
         DC    H'149'                                                           
         DC    H'150'                                                           
         DC    H'151'                                                           
         DC    H'152'                                                           
         DC    H'153'                                                           
         DC    H'154'                                                           
         DC    H'155'                                                           
         DC    H'156'                                                           
         DC    H'157'                                                           
         DC    H'158'                                                           
         DC    H'159'                                                           
         DC    H'160'                                                           
         DC    H'161'                                                           
         DC    H'162'                                                           
         DC    H'163'                                                           
         DC    H'164'                                                           
         DC    H'165'                                                           
         DC    H'166'                                                           
         DC    H'167'                                                           
         DC    H'168'                                                           
         DC    H'169'                                                           
         DC    H'170'                                                           
         DC    H'171'                                                           
         DC    X'FF'                                                            
*                                                                               
NUMRECS  DC    PL6'0'                                                           
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
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
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
NUMBUYS  DS    PL6                                                              
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091SPLDEXTAN410/19/00'                                      
         END                                                                    
