*          DATA SET SPLDEXTFB3 AT LEVEL 134 AS OF 01/28/00                      
*PHASE SPFEB30                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'SPLDEXTFB3 - CHECK FOR FEB30/00 PROBLEMS AND OTHERS'            
*                                                                               
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
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         L     R8,12(R1)           DMLDDEFN                                     
         USING LDDEFND,R8                                                       
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
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
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
DMXIT    DS    0H                                                               
         XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R5,AREC                                                          
         CLI   0(R5),X'10'                                                      
         BNH   DMXKEEP                                                          
         XC    BDATE,BDATE                                                      
         LA    R6,24(R5)           FIRST ELEMENT                                
DMXR10   CLI   0(R6),0                                                          
         BE    DMXKEEP                                                          
         CLI   0(R6),X'06'                                                      
         BE    DMXR20                                                           
         CLI   0(R6),X'0B'                                                      
         BE    DMXR20                                                           
DMXR15   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DMXR10                                                           
*                                                                               
         USING REGELEM,R6                                                       
DMXR20   CLC   RDATE,BDATE         DATE SHOULD NEVER BE < PREVIOUS DATE         
         BE    DMXR15                                                           
         BL    DMXR30                                                           
         GOTO1 LDATCON,DMCB,(2,RDATE),(10,TEMPDATE)                             
         GOTO1 LDATVAL,DMCB,(0,TEMPDATE),SIXBYDAT                               
         OC    DMCB(4),DMCB                                                     
         BZ    DMXR30                                                           
         MVC   BDATE,RDATE                                                      
         B     DMXR15                                                           
*                                                                               
DMXR30   BAS   RE,PRNTREC                                                       
         B     DMXKEEP                                                          
***************                                                                 
* PRINT THE RECORD                                                              
***************                                                                 
PRNTREC  NTR1                                                                   
         MVC   P(28),=C'****************************'                           
         GOTO1 VPRINTER                                                         
         GOTO1 =V(HEXOUT),DMCB,(R5),P,24                                        
         GOTO1 VPRINTER                                                         
         LA    R4,24(R5)                                                        
PRNTRC10 CLI   0(R4),0                                                          
         BE    EXIT                                                             
         BAS   RE,PRINTEL                                                       
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PRNTRC10                                                         
***************                                                                 
* PRINT THE ELEMENT                                                             
***************                                                                 
PRINTEL  NTR1                                                                   
         ZIC   R5,1(R4)                                                         
         MVC   P(3),=C'+++'                                                     
         CR    R4,R6                                                            
         BNE   *+10                                                             
         MVC   P(4),=C'===>'                                                    
PRTEL10  CHI   R5,16                                                            
         BNH   PRTEL20                                                          
         GOTO1 =V(HEXOUT),DMCB,0(R4),P+10,16                                    
         GOTO1 VPRINTER                                                         
         AHI   R4,16                                                            
         SHI   R5,16                                                            
         B     PRTEL10                                                          
*                                                                               
PRTEL20  ST    R5,DMCB+8                                                        
         GOTO1 =V(HEXOUT),DMCB,0(R4),P+10                                       
         GOTO1 VPRINTER                                                         
PRTELX   B     EXIT                                                             
*                                                                               
DMXEOF   DS    0H                                                               
EXIT     XIT1                                                                   
*                                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
NEXTEST  DS    F                                                                
SVEOR    DS    F                                                                
COUNT    DS    F                   NUMBER OF DARE RECORDS DELETED               
BDATE    DS    XL2                                                              
TEMPDATE DS    CL10                MM/DD/YY                                     
SIXBYDAT DS    CL6                 YYMMDD                                       
BYTE     DS    X                                                                
DONE     DS    C                                                                
ELCODE   DS    X                                                                
         DS    0D                                                               
ELEM     DS    XL100                                                            
CPKEY    DS    XL13                CLIENT PASSIVE KEY FOR DARE ORDERS           
*                                                                               
IOLEN    DS    XL2                                                              
IONUL    DC    2X'00'                                                           
IO       DS    XL30                                                             
         LTORG                                                                  
SVCLIST  DS    CL880                                                            
ESTTAB   DS    10000XL8                                                         
ESTTABX  DS    0X                                                               
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VMSUNPK  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ADSNBFFR DS    A                                                                
RECDATE  DS    CL6                                                              
ESTDATE  DS    CL6                                                              
RANGE    DS    CL6                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE SPGENBUY                                                       
         PRINT OFF                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'134SPLDEXTFB301/28/00'                                      
         END                                                                    
