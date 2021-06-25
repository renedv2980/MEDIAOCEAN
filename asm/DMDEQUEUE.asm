*          DATA SET DMDEQUEUE  AT LEVEL 002 AS OF 08/16/13                      
*PHASE DEQUEUEA                                                                 
*INCLUDE DMDMGRL                                                                
         TITLE 'DEQUEUE - DEQUEUE RESOURCE GIVEN BY EXEC PARM='                 
         PRINT NOGEN                                                            
DEQUEUE  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,*DEQUEUE,WORK=A(DEQWORK)                                       
*                                                                               
DEQ0     ST    R1,APARM            SAVE MVS SUPV INFO                           
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    DEQ2                                                             
         LA    R1,2(R1)            R1=A(PARM DATA)                              
*                                                                               
DEQ0A    CHI   R2,3                TEST IF DSPACE AND DDSIO INPUT               
         BNH   DEQ1                                                             
         CLI   3(R1),C'#'          THIS CHAR IS VALID IN PARM=TEXT              
         BE    DEQ0B                                                            
         LHI   R2,3                IGNORE EXTRA CHARACTERS                      
         B     DEQ1                                                             
*                                                                               
DEQ0B    CHI   R2,6                PARM=RRR#XY WHERE Y IS DDSIO VALUE           
         BNE   DEQ0C                                                            
         L     RE,=V(DDSIO)                                                     
         MVC   5(1,RE),5(R1)       DDSIO=DDSIOY                                 
         AHI   R2,-1                                                            
*                                                                               
DEQ0C    CHI   R2,5                PARM=RRR#X WHERE X IS DSPACE VALUE           
         BNE   DEQ0D                                                            
         L     RE,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RE),4(R1) DSPACE=X                             
         AHI   R2,-1                                                            
*                                                                               
DEQ0D    CHI   R2,4                PARM=RRR#                                    
         BNE   DEQ1                                                             
         AHI   R2,-1                                                            
*                                                                               
DEQ1     CHI   R2,3                PARM=CPU                                     
         BNE   DEQ1B                                                            
         CLC   0(3,R1),=C'CPU'                                                  
         BNE   DEQ1B                                                            
         MVC   RESOURCE,=CL8'CPU'  ALL RESOUCES ENQ BY THIS CPU                 
         B     DEQ2                                                             
*                                                                               
DEQ1B    CHI   R2,3                PARM=ADR                                     
         BNE   DEQ1C                                                            
         CLC   0(3,R1),=C'ADR'                                                  
         BNE   DEQ1C                                                            
         MVC   RESOURCE,=CL8'ALL'  ALL RESOURCES ENQ BY THIS ADRSPC             
         B     DEQ2                                                             
*                                                                               
DEQ1C    CHI   R2,3                PARM=ALL                                     
         BNE   DEQ1D                                                            
         CLC   0(3,R1),=C'ALL'                                                  
         BNE   DEQ1D                                                            
         MVC   RESOURCE,=CL8'ALL'  ALL RESOURCES ENQ BY THIS ADRSPC             
         B     DEQ2                                                             
*                                                                               
DEQ1D    LA    RF,2                RC=2 IF INVALID RESOURCE                     
         B     DEQX                                                             
*                                                                               
DEQ2     XC    ENQP1(12),ENQP1     BUILD ENQDEQ PARAM LIST                      
         LA    R2,RESOURCE                                                      
         ST    R2,ENQP1                                                         
         MVI   ENQP1,C'D'                                                       
         LA    RE,=C'ENQDEQ'                                                    
         ST    RE,ENQP0                                                         
         GOTO1 =V(DATAMGR),ENQP0                                                
         SR    RF,RF                                                            
         TM    ENQP2,X'80'         TEST DISK ERROR                              
         BZ    *+12                                                             
         LA    RF,8                                                             
         B     DEQ2X                                                            
         TM    ENQP2,X'40'         TEST ENQREC NOT FOUND                        
         BZ    *+12                                                             
         LA    RF,4                                                             
         B     DEQ2X                                                            
DEQ2X    EQU   *                                                                
*                                                                               
DEQ3     XC    ENQP1(12),ENQP1     BUILD ENQCTL PARAM LIST                      
         LA    R2,RESOURCE                                                      
         ST    R2,ENQP1                                                         
         MVI   ENQP1,C'D'                                                       
         LA    RE,=C'ENQCTL'                                                    
         ST    RE,ENQP0                                                         
         GOTO1 =V(DATAMGR),ENQP0                                                
         SR    RF,RF                                                            
         TM    ENQP2,X'80'         TEST DISK ERROR                              
         BZ    *+12                                                             
         LA    RF,8                                                             
         B     DEQ3X                                                            
         TM    ENQP2,X'40'         TEST ENQREC NOT FOUND                        
         BZ    *+12                                                             
         LA    RF,4                                                             
         B     DEQ3X                                                            
DEQ3X    EQU   *                                                                
*                                                                               
DEQX     XBASE RC=(RF)                                                          
         EJECT                                                                  
DUB      DC    D'0'                                                             
FULL     DC    F'0'                                                             
APARM    DC    F'0'                                                             
DMCB     DC    6F'0'                                                            
RESOURCE DC    CL8'ALL'            DEFAULT IS ALL FOR ADDRESS SPACE             
*                                                                               
         DC    C'**PARM**'                                                      
ENQP0    DC    F'0'                USED FOR ENTRY VIA DMDMGR                    
ENQP1    DC    F'0'                                                             
ENQP2    DC    F'0'                                                             
ENQP3    DC    F'0'                                                             
ENQP4    DC    F'0'                                                             
ENQP5    DC    F'0'                                                             
ENQP6    DC    F'0'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'*WRKWRK*'                                                      
DEQWORK  DC    4000D'0'                                                         
*                                                                               
         DC    C'*UTLUTL*'                                                      
UTL      DC    F'0',X'01',251X'00'                                              
*                                                                               
         DC    C'*SSBSSB*'                                                      
SSB      DC    H'0',X'FF',1021X'00'                                             
*                                                                               
*FASSBOFF                                                                       
SSOOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DMDEQUEUE 08/16/13'                                      
         END                                                                    
