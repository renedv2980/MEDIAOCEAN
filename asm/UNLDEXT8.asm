*          DATA SET UNLDEXT8   AT LEVEL 103 AS OF 01/22/03                      
*PHASE UNLDEXT8,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'UNLDEXT8 - TURN ON BIT IN OLD STYLE PAID'                       
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
UNLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*UNLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
UNXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         ST    R5,RELO                                                          
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BNE   *+14                                                             
         XC    COUNT,COUNT                                                      
         B     UNXIT               INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    UNXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    UNXEOF              END-OF-FILE                                  
         B     UNXIT                                                            
         SPACE 2                                                                
UNXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     UNXIT                                                            
         SPACE 2                                                                
UNXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     UNXIT                                                            
         SPACE 2                                                                
UNXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                                          
*******************************************************************             
UNXREC   DS    0H                                                               
         L     R3,AREC                                                          
         USING NURECD,R3                                                        
*                                                                               
         XC    SVREC,SVREC                                                      
*                                                                               
         CLI   0(R3),X'04'         UNIT                                         
         BNE   UNXKEEP                                                          
         CLI   NUKAM,X'F3'         STARCOM?                                     
         BNE   UNXKEEP                                                          
         DROP  R3                                                               
*                                                                               
*                                                                               
         MVC   SVREC,0(RF)                                                      
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
*                                                                               
         GOTO1 VPRNTBL,DMCB,=C'REC',AREC,C'DUMP',200,=C'1D'                     
         MVC   P(11),=C'EARNED COST'                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         B     UNXKEEP                                                          
*                                                                               
* END-OF-FILE PROCESSING                                                        
*                                                                               
UNXEOF   DS    0H                                                               
         MVC   P(10),=C'# OF RECS:'                                             
         EDIT  COUNT,(10,P+20)                                                  
         GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
*                                                                               
         GETEL R3,27,ELCODE                                                     
         LTORG                                                                  
*                                                                               
COUNT    DS    F                                                                
SVREC    DS    XL20                                                             
*                                                                               
         DS    0F                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
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
MFULL    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    CL1                                                              
BYTE2    DS    CL2                                                              
ELCODE   DS    CL1                                                              
NEW      DS    CL20                                                             
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPTRNREV                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'103UNLDEXT8  01/22/03'                                      
         END                                                                    
