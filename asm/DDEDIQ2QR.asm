*          DATA SET DDEDIQ2QR  AT LEVEL 008 AS OF 10/01/99                      
*PHASE EDIQ2QRA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
         TITLE 'DDEDIQ2QR -- WRITE TRANSFERRED REPORTS TO PRINT QUEUE'          
EDIQ2QR  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*EDIQ2QR,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         LR    R3,RC                                                            
         SH    R3,=H'4'                                                         
         L     R3,0(R3)                                                         
         L     R3,0(R3)            A(R1 PARAMETERS FROM ATTACH)                 
         USING Q2QPARMD,R3                                                      
*                                                                               
         LA    R1,QMAINECB         BUILD ECBLIST                                
         STCM  R1,7,ECBLST+1                                                    
         LA    R1,QSTOPECB                                                      
         STCM  R1,7,ECBLST+5                                                    
*                                                                               
         GOTO1 =V(DMENQDEQ),DMCB,(C'N',=C'RDDSQDQ')                             
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'GLIST'),=C'PRTQUE',IO,0,PQBUFF            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,IO+32                                                         
         ZIC   RF,0(R1)            NUMBER OF PRINT QUEUES                       
*                                                                               
         C     RF,=F'16'                                                        
         BE    *+6                                                              
         DC    H'0'                MUST BE EXACTLY 16 PRINT QUEUES              
*                                                                               
         USING DTFPHD,R2                                                        
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ1'                             
         L     R2,DMCB+12          A(DTF)                                       
         MVC   DTFFID,=C'RPRTQ1 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ2'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQ2 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ3'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQ3 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ4'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQ4 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ5'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQ5 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ6'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQ6 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ7'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQ7 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ8'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQ8 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQ9'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQ9 '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQA'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQA '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQB'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQB '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQC'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQC '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQD'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQD '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQE'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQE '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQF'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQF '                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DTFAD',=C'PRTQG'                             
         L     R2,DMCB+12                                                       
         MVC   DTFFID,=C'RPRTQG '                                               
         DROP  R2                                                               
         EJECT                                                                  
LOOP     WAIT  1,ECBLIST=ECBLST    WAIT FOR POST FROM MAIN PROGRAM              
*                                                                               
         TM    QSTOPECB,X'40'      STOP?                                        
         BZ    DONTSTOP            NO                                           
*                                                                               
         XBASE                                                                  
*                                                                               
DONTSTOP TM    QMAINECB,X'40'      MAIN TASK POSTED READY?                      
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         XC    QMAINECB,QMAINECB                                                
*                                                                               
         OPEN  (Q2QRPT,INPUT)      OPEN TEMPORARY FILE                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL OPEN OF TEMPORARY FILE          
*                                                                               
         XC    P,P                 PRINT LINE FOR PRTQUE CALLS                  
         LA    RF,P                                                             
         USING PQPLD,RF                                                         
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSRCID,QUSERID                                                  
         MVC   QLSUBID,QSUBID                                                   
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'24'                                                   
         MVC   QLDESC,QDESCRIP                                                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,P,PQBUFF           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT OPEN PQ REPORT                     
*                                                                               
GETLINE  GET   Q2QRPT,P            READ A PQLINE                                
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,P,PQBUFF           
         CLI   DMCB+8,0                                                         
         BE    GETLINE                                                          
         DC    H'0'                COULD NOT WRITE PQ LINE                      
*                                                                               
ENDGET   CLOSE (Q2QRPT)            CLOSE TEMPORARY FILE                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL CLOSE OF TEMPORARY FILE         
*                                                                               
         MVI   P,X'FF'             CLOSE REPORT                                 
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,P,PQBUFF           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT CLOSE PQ REPORT                    
*                                                                               
         POST  QSUBECB             TELL MAIN PROGRAM WE ARE DONE                
*                                                                               
         B     LOOP                WAIT                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
SSB      DC    F'0'                                                             
UTL      DC    F'0',X'0A'                                                       
P        DS    XL200                                                            
         SPACE 3                                                                
Q2QRPT   DCB   DDNAME=Q2QRPT,MACRF=GM,DSORG=PS,RECFM=FBM,LRECL=133,    +        
               BLKSIZE=23142,EODAD=ENDGET                                       
         SPACE 3                                                                
         DS    0F                                                               
ECBLST   DC    X'00',AL3(0)        A(QMAINECB)                                  
         DC    X'80',AL3(0)        A(QSTOPECB)                                  
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DS    1000X               IO AREA FOR GLIST                            
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*PQBUFF*'                                                      
PQBUFF   DS    14336X              PQ BUFFER                                    
         EJECT                                                                  
       ++INCLUDE DDEDICTWRK                                                     
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008DDEDIQ2QR 10/01/99'                                      
         END                                                                    
