*          DATA SET DEPVLDDAYL AT LEVEL 093 AS OF 05/01/02                      
*PHASE DEPVLDYA MAYAEXTN                                                        
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PVLDEXT - DELETE NTI DAILY RECS PRE 10/99'                      
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
         NMOD1 WORKX-WORKD,*PVLDEXT,RR=R6                                       
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
         AR    RE,R6                                                            
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
         L     R3,AREC                                                          
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(23),0(R3)                                                 
         MVI   WORK,23                                                          
         BAS   RE,DUMPREC                                                       
         B     PVXIT                                                            
*                                                                               
PVINIT   DS    0H                                                               
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
*DELETE ALL NIT DAILY RECORDS PRIOR TO 8/30/99  (9936=6324)                     
**********************************************************************          
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PMKEY,R3                                                         
         CLC   PMCODE(3),=C'QNN'                                                
         BH    PVXEOF                                                           
         BNE   PVXNREC                                                          
         CLI   PMSTAT+4,C'D'       DAILY RECORDS ONLY                           
         BNE   PVXKEEP                                                          
         CLC   PMBOOK,=X'6300'     KEEP 9900 RECDS                              
         BE    PVXKEEP                                                          
         CLC   PMBOOK,=X'6324'     OCT15/99                                     
         BL    PVXPURGE                                                         
         B     PVXKEEP                                                          
         DROP  R3                                                               
*                                                                               
         USING PNKEY,R3                                                         
PVXNREC  DS    0H                                                               
         CLC   PNCODE(3),=C'NNN'                                                
         BNE   PVXPREC                                                          
         CLC   PNBOOK,=X'6300'                                                  
         BE    PVXKEEP                                                          
         CLI   PNSTAT+4,C'D'                                                    
         BNE   PVXKEEP                                                          
         CLC   PNBOOK,=X'6324'                                                  
         BL    PVXPURGE                                                         
         B     PVXKEEP                                                          
         DROP  R3                                                               
*                                                                               
         USING PRKEY,R3                                                         
PVXPREC  DS    0H                                                               
         CLC   PRCODE(3),=C'PNN'                                                
         BNE   PVXKEEP                                                          
         CLC   PRBOOK,=X'6300'                                                  
         BE    PVXKEEP                                                          
         CLI   PRSTAT+4,C'D'                                                    
         BNE   PVXKEEP                                                          
         CLC   PRBOOK,=X'6324'                                                  
         BL    PVXPURGE                                                         
         B     PVXKEEP                                                          
*                                                                               
         EJECT                                                                  
* END-OF-FILE PROCESSING                                                        
*                                                                               
PVXDONE  DS    0H                                                               
         L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'         PURGE RECD AND EXIT                          
*                                                                               
PVXEOF   DS    0H                                                               
         B     PVXIT                                                            
         EJECT                                                                  
*                                                                               
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
         L     R1,PRTCNT                                                        
         A     R1,=F'1'                                                         
         ST    R1,PRTCNT                                                        
         CLC   SVTYP,WORK+1                                                     
         BNE   DUMPR5                                                           
         MVC   SVTYP,WORK+1                                                     
         C     R1,=F'10'                                                        
         BH    DUMPRECX                                                         
         B     DUMPR10                                                          
*                                                                               
DUMPR5   XC    PRTCNT,PRTCNT       CLEAR COUNT FOR THIS TYPE OF RECD            
         MVC   SVTYP,WORK+1                                                     
*                                                                               
DUMPR10  LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         GOTO1 VPRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D'              
*                                                                               
DUMPRECX XIT1                                                                   
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PRTCNT   DC    F'0'                PRINT LOOP COUNTER                           
SVTYP    DC    C' '                                                             
*                                                                               
LOGIO    DC    V(LOGIO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
KEEPS    DC    F'0'                                                             
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
**PAN#1  DC    CL21'093DEPVLDDAYL05/01/02'                                      
         END                                                                    
