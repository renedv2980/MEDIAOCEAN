*          DATA SET SPEXTCML   AT LEVEL 011 AS OF 07/20/99                      
*          DATA SET SPEXTUNT   AT LEVEL 008 AS OF 12/03/98                      
*          DATA SET SPEXTREG   AT LEVEL 025 AS OF 01/05/89                      
*PHASE SPEXTCML,*                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE XSORT                                                                  
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* JUL 20, 1999                                                                  
* FIX BAD CML RECS WITH NULL DATES                                              
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
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   L     R3,AREC                                                          
         AP    TOTRD,=P'1'                                                      
         CLC   =X'0A21',0(R3)     ONLY COMMLS                                   
         BNE   DMXKEEP                                                          
         SPACE                                                                  
* BYPASS CMML SEQ RECS                                                          
         SPACE                                                                  
         CLC   CMLKCML-CMLKEY(,R3),=C'99999999'                                 
         BE    DMX010                                                           
         SPACE                                                                  
         OC    CMLKCML-CMLKEY(,R3),CMLKCML-CMLKEY(R3)                           
         BNZ   DMX020                                                           
         SPACE                                                                  
DMX010   DS    0H                                                               
         AP    TOTSEQ,=P'1'                                                     
         B     DMXKEEP                                                          
         SPACE                                                                  
DMX020   DS    0H                                                               
         AP    TOTCML,=P'1'                                                     
         LA    R4,24(,R3)          FIND FIRST ELEM                              
         CLI   0(R4),X'10'         THIS TRAFFIC ELEM                            
         BE    *+6                  YES CHECK IT OUT                            
         DC    H'0'                MUST HAVE ELEMENT                            
         SPACE                                                                  
* LOOK FOR COMMLS WITH NO DATE                                                  
         SPACE                                                                  
         OC    CMLRLSE-CMLDTAEL(3,R4),CMLRLSE-CMLDTAEL(R4)                      
         BNZ   DMXKEEP                                                          
*        BNZ   DMX100                                                           
         SPACE                                                                  
* IF ONE MISSING BOTH SHOULD BE MISSING                                         
         SPACE                                                                  
         OC    CMLRCL-CMLDTAEL(3,R4),CMLRCL-CMLDTAEL(R4)                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
* IF BOTH MISSING BOTH MUST BE EASI (I HOPE)                                    
         SPACE                                                                  
         TM    CMLSTAT-CMLDTAEL(R4),X'20'                                       
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   CMLRLSE-CMLDTAEL(6,R4),=X'500101FFFFFF'                          
         AP    BADCMLCT,=P'1'                                                   
         NOP   DMXKEEP                                                          
         OI    *-3,X'F0'                                                        
         LA    R6,=CL20'EZ CMML WITH NO DATE'                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE                                                                  
DMX100   DS    0H                                                               
         LA    R6,=CL20'EZ CMML WITH NO DATE'                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R6)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
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
         LA    R3,TOTCML                                                        
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         LA    R3,BADCMLCT                                                      
         MVC   P+5(28),4(R3)                                                    
         EDIT  (P4,0(R3)),(10,P+33),COMMAS=YES                                  
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
TOTRD    DC    PL4'0',CL28'TOTAL RECS READ ='                                   
TOTSEQ   DC    PL4'0',CL28'TOTAL SEQ READ='                                     
TOTCML   DC    PL4'0',CL28'TOTAL CMMLS READ='                                   
BADCMLCT DC    PL4'0',CL28'BAD CMML DATE='                                      
SVAGY    DS    XL3                                                              
FULL     DS    F                                                                
BADAGYCT DC    PL4'0'                                                           
WORK     DS    CL64                                                             
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
**PAN#1  DC    CL21'011SPEXTCML  07/20/99'                                      
         END                                                                    
