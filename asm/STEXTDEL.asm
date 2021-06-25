*          DATA SET STEXTDEL   AT LEVEL 012 AS OF 09/22/00                      
*PHASE STEXTDEL                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE XSORT                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* SEP 20, 2000                                                                  
* DEL CML FROM NET4 SPOTC MSNY H7/02                                            
* DEL CML RECS FROM SPOTF MSNY H7/03                                            
*                                                                               
* FOR CLTS AIG, AP. OT, EKN, HU, IM0, KK, SAP, NV, OND, PPP, YP, WMD            
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
         LA    R0,CLTABCT                                                       
         LA    R5,CLTAB                                                         
DMXI010  GOTO1 =V(CLPACK),DMCB,(R5),3(R5)                                       
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,8(,R5)                                                        
         BCT   R0,DMXI010                                                       
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   L     R3,AREC                                                          
         AP    TOTRD,=P'1'                                                      
DMX000   NOP   DMXKEEP                                                          
         CLC   =X'0A21',0(R3)     ONLY COMMLS                                   
         BH    DMXKEEP                                                          
         BE    DMX010                                                           
         SPACE                                                                  
         OI    DMX000+1,X'F0'      ALL DONE                                     
         B     DMXKEEP                                                          
         SPACE                                                                  
DMX010   DS    0H                                                               
         MVC   BYTE,2(R3)                                                       
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'20'          THIS MSNY                                    
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         LA    R0,CLTABCT                                                       
         LA    R5,CLTAB                                                         
DMX012   DS    0H                                                               
         CLC   3(2,R5),3(R3)       THIS CLIENT NEEDED                           
         BE    DMX014                                                           
         LA    R5,8(,R5)                                                        
         BCT   R0,DMX012                                                        
         B     DMXKEEP                                                          
DMX014   DS    0H                                                               
         CLC   =X'0A21',0(R3)     ONLY COMMLS                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CMLKCML-CMLKEY(,R3),CMLKCML-CMLKEY(R3)                           
         BNZ   DMX018                                                           
         AP    TOTSEQ,=P'1'                                                     
         B     DMX030                                                           
DMX018   DS    0H                                                               
                                                                                
         CLC   CMLKCML-CMLKEY(,R3),=C'99999999'                                 
         BNE   DMX020                                                           
         AP    TOTPRD,=P'1'                                                     
         B     DMX030                                                           
         SPACE                                                                  
DMX020   DS    0H                                                               
         AP    5(3,R5),=P'1'                                                    
         AP    TOTCML,=P'1'                                                     
DMX024   DS    0H                                                               
         SPACE                                                                  
         CLC   20(2,R3),=C'H7'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
DMX030   DS    0H                                                               
         MVC   P+3(3),=C'DEL'                                                   
         MVC   P+9(4),=C'CLT='                                                  
         MVC   P+13(3),0(R5)                                                    
         MVC   P+20(4),=C'KEY='                                                 
         MVC   P+24(13),0(R3)                                                   
         GOTO1 =V(HEXOUT),DMCB,(R3),P+40,13,0                                   
         GOTO1 VPRINTER                                                         
         NOP   DMXPURGE                                                         
         MVI   *-3,X'F0'                                                        
         LA    R6,=CL20'DELETED REC FOR MSNY'                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXPURGE                                                         
         SPACE                                                                  
DMXEOF   DS    0H                                                               
         LA    R3,TOTRD                                                         
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,TOTSEQ                                                        
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,TOTPRD                                                        
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,TOTCML                                                        
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R3,TOTXT                                                         
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         LA    R4,CLTABCT                                                       
         LA    R5,CLTAB                                                         
DMXE010  DS    0H                                                               
         MVC   P+5(7),=C'CLIENT='                                               
         MVC   P+12(3),0(R5)                                                    
         EDIT  (P3,5(R5)),(10,P+20),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R5,8(,R5)                                                        
         BCT   R4,DMXE010                                                       
         B     DMXIT                                                            
         SPACE 2                                                                
TOTRD    DC    PL4'0',CL28'TOTAL RECS READ ='                                   
TOTSEQ   DC    PL4'0',CL28'TOTAL SEQ DELETED='                                  
TOTPRD   DC    PL4'0',CL28'TOTAL PRD DELETED='                                  
TOTCML   DC    PL4'0',CL28'TOTAL CMMLS DELETED='                                
TOTXT    DC    PL4'0',CL28'TOTAL TEXT DELETED='                                 
SVAGY    DS    XL3                                                              
FULL     DS    F                                                                
BADAGYCT DC    PL4'0'                                                           
         DS    0D                                                               
WORK     DS    CL64                                                             
CLTAB    DC    CL5'AIG',PL3'0'                                                  
         DC    CL5'AP ',PL3'0'                                                  
         DC    CL5'EKN',PL3'0'                                                  
         DC    CL5'HU ',PL3'0'                                                  
         DC    CL5'IM0',PL3'0'                                                  
         DC    CL5'KK ',PL3'0'                                                  
         DC    CL5'NV ',PL3'0'                                                  
         DC    CL5'OND',PL3'0'                                                  
         DC    CL5'OT ',PL3'0'                                                  
         DC    CL5'PPP',PL3'0'                                                  
         DC    CL5'SAP',PL3'0'                                                  
         DC    CL5'WMD',PL3'0'                                                  
         DC    CL5'YP ',PL3'0'                                                  
CLTABCT  EQU   (*-CLTAB)/8                                                      
BYTE     DS    CL1                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
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
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE SPTRCMML                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012STEXTDEL  09/22/00'                                      
         END                                                                    
