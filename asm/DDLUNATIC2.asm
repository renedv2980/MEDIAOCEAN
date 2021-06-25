*          DATA SET DDLUNATIC2 AT LEVEL 018 AS OF 08/13/00                      
*PHASE LUNATICT LUNATIC2                                                        
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'DDLUNATIC2 -- TESTING LUNATIC'                                  
LUNATIC2 CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,LUNATIC2,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         LR    R3,RC                                                            
         SH    R3,=H'4'                                                         
         L     R3,0(R3)                                                         
         L     R3,0(R3)            A(R1 PARAMETERS FROM ATTACH)                 
         USING PARMD,R3                                                         
*                                                                               
         LA    R0,4                                                             
         LNR   R0,R0               R0 = -4                                      
         SVC   247                 SO LUNATIC2 IS NOT SWAPPABLE                 
*                                                                               
         LA    R1,PMAINECB         BUILD ECBLIST                                
         STCM  R1,7,ECBLST+1                                                    
         LA    R1,PSTOPECB                                                      
         STCM  R1,7,ECBLST+5                                                    
*                                                                               
         GOTO1 =V(DMENQDEQ),DMCB,(C'N',=C'TDDSQDQ')                             
*                                                                               
         USING DTFPHD,R2                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ1'                             
         L     R2,DMCB+12          A(DTF)                                       
         MVC   DTFFID,=C'TPRTQ1 '                                               
         GOTO1 (RF),(R1),,=C'PRTQ2'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQ2 '                                               
         GOTO1 (RF),(R1),,=C'PRTQ3'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQ3 '                                               
         GOTO1 (RF),(R1),,=C'PRTQ4'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQ4 '                                               
         GOTO1 (RF),(R1),,=C'PRTQ5'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQ5 '                                               
         GOTO1 (RF),(R1),,=C'PRTQ6'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQ6 '                                               
         GOTO1 (RF),(R1),,=C'PRTQ7'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQ7 '                                               
         GOTO1 (RF),(R1),,=C'PRTQ8'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQ8 '                                               
         GOTO1 (RF),(R1),,=C'PRTQ9'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQ9 '                                               
         GOTO1 (RF),(R1),,=C'PRTQA'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQA '                                               
         GOTO1 (RF),(R1),,=C'PRTQB'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQB '                                               
         GOTO1 (RF),(R1),,=C'PRTQC'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQC '                                               
         GOTO1 (RF),(R1),,=C'PRTQD'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQD '                                               
         GOTO1 (RF),(R1),,=C'PRTQE'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQE '                                               
         GOTO1 (RF),(R1),,=C'PRTQF'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQF '                                               
         GOTO1 (RF),(R1),,=C'PRTQG'                                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'TPRTQG '                                               
         DROP  R2                                                               
         EJECT                                                                  
LOOP     WAIT  1,ECBLIST=ECBLST    WAIT FOR POST FROM MAIN PROGRAM              
*                                                                               
         TM    PSTOPECB,X'40'      STOP?                                        
         BZ    DONTSTOP            NO                                           
*                                                                               
         XBASE                                                                  
*                                                                               
DONTSTOP TM    PMAINECB,X'40'      MAIN TASK POSTED READY?                      
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         XC    PMAINECB,PMAINECB                                                
*                                                                               
         CLC   =C'HERE COMES A REPORT',PRECORD                                  
         BNE   LUN10                                                            
         XC    PRECORD,PRECORD                                                  
         MVC   PRECORD(18),=C'GIVE ME THE REPORT'                               
         B     GOODBYE                                                          
*                                                                               
LUN10    CLC   =C'*HDR*',PRECORD                                                
         BNE   LUN20                                                            
*                                                                               
         LA    RE,PRECORD+5                                                     
         XC    P,P                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,P                                                             
         USING PQPLD,R2                                                         
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         OI    QLFLAG,QLFLRALL                                                  
         MVC   QLINDEX,QLINDEX-PQPLD(RE)                                        
         MVC   QLBATTR,QLBATTR-PQPLD(RE)                                        
         NI    QLTYPE,X'FF'-PQTYRSND  TURN OFF 'SEND PENDING' FLAG              
         MVC   QLFATTR,QLFATTR-PQPLD(RE)                                        
         TM    QLATTB,QLATJOBI     TRANSMITTING JCL?                            
         BZ    *+14                NO                                           
         MVC   QLRNUM,QLREPNO-PQPLD(RE)                                         
         B     LUN15                                                            
         TM    QLATTB,QLATJOBO     TRANSMITTING SOON JOB OUTPUT?                
         BZ    LUN15                                                            
         OI    QLFLAG,QLFLCIDA                                                  
         MVC   QLREPRCI,PCIADDR    CONTROL INTERVAL NUMBER                      
*                                                                               
LUN15    GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),PRTQUE,0,P,PQBUFF               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT OPEN PQ REPORT                     
*                                                                               
         XC    PREFNUM,PREFNUM                                                  
         TM    QLATTB,QLATJOBI     TRANSMITTING JCL?                            
         BZ    LUN17               NO                                           
         MVC   SAVERCI,QLREPRCI    NEW CONTROL INTERVAL NUMBER                  
         MVC   SAVEREF,QLREPRNO    NEW REPORT REFERENCE NUMBER                  
         MVC   PREFNUM,SAVEREF                                                  
*                                                                               
LUN17    XC    PRECORD,PRECORD                                                  
         MVC   PRECORD(20),=C'THE REPORT IS OPENED'                             
         B     GOODBYE                                                          
         DROP  R2                                                               
*                                                                               
LUN20    CLC   =C'*REC*',PRECORD                                                
         BNE   LUN30                                                            
*                                                                               
         XC    P,P                 PRINT LINE FOR PRTQUE CALLS                  
         MVC   P,PRECORD+5                                                      
         CLC   =C'DIRECT=',P+1                                                  
         BNE   LUN25                                                            
         GOTO1 =V(HEXOUT),DMCB,SAVEREF,P+48,2,=C'TOG'                           
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HEXOUT),DMCB,SAVERCI,P+52,2,=C'TOG'                           
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LUN25    GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),PRTQUE,0,P,PQBUFF               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT WRITE PQ LINE                      
         XC    PRECORD,PRECORD                                                  
         MVC   PRECORD(16),=C'WE GOT A PQ LINE'                                 
         B     GOODBYE                                                          
*                                                                               
LUN30    CLC   =C'*END*',PRECORD                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   P,X'FF'             CLOSE REPORT                                 
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),PRTQUE,0,P,PQBUFF               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT CLOSE PQ REPORT                    
         XC    PRECORD,PRECORD                                                  
         MVC   PRECORD(13),=C'REPORT CLOSED'                                    
         B     GOODBYE                                                          
*                                                                               
GOODBYE  POST  PSUBECB             TELL MAIN PROGRAM WE ARE DONE                
*                                                                               
         B     LOOP                WAIT                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
SSB      DC    F'0'                                                             
UTL      DC    F'0',X'0A'                                                       
PRTQUE   DC    C'PRTQU'                                                         
SAVERCI  DS    XL2                                                              
SAVEREF  DS    XL2                                                              
         DS    XL4                                                              
P        DS    XL200                                                            
*                                                                               
         DS    0F                                                               
ECBLST   DC    X'00',AL3(0)        A(PMAINECB)                                  
         DC    X'80',AL3(0)        A(PSTOPECB)                                  
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*PQBUFF*'                                                      
PQBUFF   DS    14336X              PQ BUFFER                                    
         SPACE 3                                                                
PARMD    DSECT                                                                  
PMAINECB DS    F                                                                
PSUBECB  DS    F                                                                
PSTOPECB DS    F                                                                
PREFNUM  DS    H                                                                
PCIADDR  DS    H                                                                
PRECORD  DS    XL200                                                            
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
       ++INCLUDE DMDTFPH                                                        
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018DDLUNATIC208/13/00'                                      
         END                                                                    
