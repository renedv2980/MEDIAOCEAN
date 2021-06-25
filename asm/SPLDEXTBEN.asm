*          DATA SET SPLDEXTBEN AT LEVEL 154 AS OF 05/27/98                      
*PHASE SPEXTBEN                                                                 
SPEXTCBL TITLE 'DELETE ALL 0D0D RECS FROM SPTFILES'                             
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
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
*                                                                               
         L     R9,VLDDEFN                                                       
         USING LDDEFND,R9                                                       
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
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
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         CLC   =X'0D0D',0(R3)                                                   
         BE    DEL                                                              
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*=================================================================              
* BUY RECORDS   R3=A(RECORD)                                                    
*=================================================================              
         SPACE 1                                                                
DEL      DS    0H                                                               
*                                                                               
         GOTO1 LHEXOUT,DMCB,0(R3),P+2,30,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         AP    NUMRECS,=P'1'                                                    
         B     DMXPURGE                                                         
*                                                                               
         EJECT                                                                  
*==================================================================             
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF RECS ='                                       
         EDIT  (P6,NUMRECS),(12,P+25)                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
*                                                                               
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         LTORG                                                                  
*                                                                               
NUMRECS  DC    PL6'0'                                                           
         SPACE                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
NETNUM   DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
BYTE     DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
TABADDR  DS    A                                                                
DUB      DS    D                                                                
KEY      DS    CL10                                                             
BAGYMED  DS    CL1                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE SPCBLLST                                                       
         SPACE 2                                                                
WORKD    DSECT                                                                  
*UB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
ELCODE   DS    X                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL5                                                              
         DS    CL2                                                              
POVER    DS    CL4                                                              
         DS    CL3                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL4                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         DS    CL4                                                              
PERR     DS    CL25                                                             
         DS    CL1                                                              
PMYREC   DS    CL46                                                             
         PRINT OFF                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'154SPLDEXTBEN05/27/98'                                      
         END                                                                    
