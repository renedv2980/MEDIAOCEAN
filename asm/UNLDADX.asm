*          DATA SET UNLDADX    AT LEVEL 019 AS OF 01/17/97                      
*          DATA SET UNLDEXT    AT LEVEL 096 AS OF 05/15/91                      
*PHASE UNLDADX,+0                                                               
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELEN                                                                  
         TITLE 'UNLDADX - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
         BE    UNXIT               INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    UNXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    UNXEOF              END-OF-FILE                                  
         B     UNXIT                                                            
         SPACE 2                                                                
UNXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     UNXIT                                                            
         SPACE 2                                                                
UNXPURGE GOTO1 VPRNTBL,DMCB,=C'KEEP',AREC,C'DUMP',30,=C'1D'                     
         L     R1,COUNT            PURGE RECORD EXIT                            
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
         L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     UNXIT                                                            
         SPACE 2                                                                
UNXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC - PURGE ADFORCE CLOSED OUT RECS                          
*******************************************************************             
         SPACE 1                                                                
UNXREC   DS    0H                                                               
         USING NURECD,R3                                                        
         L     R3,AREC                                                          
         CLI   0(R3),X'04'                                                      
         BNE   UNX300                                                           
*                                                                               
         CLI   1(R3),X'13'         IS IT WILA                                   
         BNE   UNXKEEP                                                          
         CLC   2(2,R3),=XL2'80EC'  IS IT AHM                                    
         BNE   UNXKEEP                                                          
*                                                                               
         LA    R4,MKTTAB                                                        
         LA    RF,4                                                             
*                                                                               
UNX20    CLC   NUKNET,0(R4)                                                     
         BE    UNX40                                                            
         LA    R4,10(R4)                                                        
         BCT   RF,UNX20                                                         
         B     UNXKEEP                                                          
*                                                                               
UNX40    GOTO1 VPRNTBL,DMCB,=C'BEFO',AREC,C'DUMP',150,=C'1D'                    
         MVC   NUKNET,4(R4)                                                     
         MVC   NUMARKET,8(R4)                                                   
         GOTO1 VPRNTBL,DMCB,=C'AFTE',AREC,C'DUMP',150,=C'1D'                    
         B     UNXKEEP                                                          
*                                                                               
MKTTAB   DC    CL4'ARTS',CL4'AEN ',XL2'0064'                                    
         DC    CL4'CNB ',CL4'CNBC',XL2'0066'                                    
         DC    CL4'ESP ',CL4'ESPN',XL2'006B'                                    
         DC    CL4'HIST',CL4'THC ',XL2'0098'                                    
*                                                                               
*                                                                               
*                                                                               
*                                                                               
UNX300   CLI   0(R3),X'02'        IS IT PACKAGE                                 
         BNE   UNXKEEP                                                          
         DROP  R3                                                               
         USING NPRECD,R3                                                        
         CLI   NPKAM,X'13'         IS IT KANY                                   
         BNE   UNXKEEP                                                          
         CLC   NPKCLT,=XL2'80EC'                                                
         BNE   UNXKEEP                                                          
*                                                                               
         LA    R4,MKTTAB                                                        
         LA    RF,4                                                             
*                                                                               
UNX320   CLC   NPKNET,0(R4)                                                     
         BE    UNX340                                                           
         LA    R4,10(R4)                                                        
         BCT   RF,UNX320                                                        
         B     UNXKEEP                                                          
*                                                                               
UNX340   GOTO1 VPRNTBL,DMCB,=C'BEFPACK',AREC,C'DUMP',30,=C'1D'                  
         MVC   NPKNET,4(R4)                                                     
         GOTO1 VPRNTBL,DMCB,=C'AFTPACK',AREC,C'DUMP',30,=C'1D'                  
         B     UNXKEEP                                                          
*                                                                               
PACKTAB  DC    CL4'ABC ',XL1'1E',XL1'14'                                        
         DC    CL4'ABC ',XL1'1E',XL1'1E'                                        
         DC    CL4'CBS ',XL1'1E',XL1'14'                                        
         DC    CL4'CBS ',XL1'1E',XL1'1E'                                        
         DC    CL4'NBC ',XL1'1E',XL1'0D'                                        
         DC    CL4'NBC ',XL1'1E',XL1'0E'                                        
         DC    CL4'NBC ',XL1'1E',XL1'14'                                        
         DC    CL4'NBC ',XL1'1E',XL1'15'                                        
         DC    CL4'NBC ',XL1'1E',XL1'1E'                                        
*                                                                               
         DC    CL4'ARTS',XL1'1F',XL1'1E'                                        
         DC    CL4'ARTS',XL1'1F',XL1'28'                                        
         DC    CL4'CNB ',XL1'1F',XL1'1E'                                        
         DC    CL4'CNB ',XL1'1F',XL1'28'                                        
         DC    CL4'ESP ',XL1'1F',XL1'14'                                        
         DC    CL4'HIST',XL1'1F',XL1'1E'                                        
         DC    CL4'HIST',XL1'1F',XL1'28'                                        
         DC    CL4'TNT ',XL1'1F',XL1'1E'                                        
         DC    CL4'TNT ',XL1'1F',XL1'28'                                        
         DC    CL4'USA ',XL1'1F',XL1'15'                                        
*                                                                               
         DC    CL4'AVID',XL1'21',XL1'0A'                                        
         DC    CL4'UVID',XL1'21',XL1'0A'                                        
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
* END-OF-FILE PROCESSING                                                        
*                                                                               
UNXEOF   DS    0H                                                               
*                                                                               
         MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         MVC   P+10(26),=C'SUMMARY OF RECORDS PURGED '                          
         L     R4,COUNT                                                         
         EDIT  (R4),(10,P+37)                                                   
         GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
         EJECT                                                                  
         GETEL R6,27,ELCODE                                                     
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
COPREC   DC    C'R',X'82',C'S',X'81',C'P',X'82',C'T',X'42',C'H',X'42'           
         DC    C'U',X'43',C'V',X'40'                                            
         DC    X'00'                                                            
NSPREC   DC    C'R',X'81',C'S',X'81',C'P',X'81',C'T',X'43',C'H',X'43'           
         DC    C'U',X'43',C'V',X'40'                                            
         DC    X'00'                                                            
         DS    0F                                                               
COUNT    DC    F'0'                                                             
         SPACE 1                                                                
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
POTYP    DS    CL1                                                              
ELCODE   DS    CL1                                                              
NEW      DS    CL20                                                             
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPLAN                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019UNLDADX   01/17/97'                                      
         END                                                                    
