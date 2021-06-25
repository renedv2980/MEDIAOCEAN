*          DATA SET PPREPG102  AT LEVEL 025 AS OF 01/13/14                      
*PHASE PPG102A                                                                  
*INCLUDE GENIOS                                                                 
*INCLUDE IJGFOZZZ                                                               
         TITLE 'PPG102 - INSERTION ORDER TA GENERATOR'                          
* CHANGE LOG                                                                    
*                                                                               
*   BPLA  12/13    CHANGES FOR 2 CHARACTER SYSTEMS                              
*                  USES SYSNAID INSTEAD OF FILENUM                              
PPG102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPG102                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPG1WRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BNE   EXIT                                                             
*                                                                               
         XC    LAST72,LAST72                                                    
         XC    LAST72O,LAST72O                                                  
         XC    L72PROF,L72PROF                                                  
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   IOSW,C'N'                                                        
         OI    DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVC   WORK(2),RCDATE+6                                                 
         MVC   WORK+2(2),RCDATE                                                 
         MVC   WORK+4(2),RCDATE+3                                               
         LH    R3,=H'-30'                                                       
         GOTO1 ADDAY,DMCB,WORK,DUB,(R3)                                         
         GOTO1 DATCON,DMCB,DUB,(3,DATLIM)                                       
         CLI   QOPT2,C'N'                                                       
         BE    RP1                                                              
***                                                                             
*** NOTE SYSNAID IS THE 2 CHARACTER FILE NUMBER                                 
***                                                                             
*&&DO                                                                           
         MVC   PP49RQN+22+6(2),SYSNAID       SET FILE NUMBER                    
         MVC   PP49RQI+22+6(2),SYSNAID                                          
         LA    R1,PP49RQN                                                       
         OPENR (1)                                                              
*&&                                                                             
*&&OS                                                                           
         MVC   PP49RQN+40+6(2),SYSNAID       SET FILE NUMBER                    
         MVC   PP49RQI+40+6(2),SYSNAID                                          
         OPEN  (PP49RQN,(OUTPUT))                                               
*&&                                                                             
*                                                                               
RP1      DS    0H                                                               
         CLI   QOPT2,C' '                                                       
         BNE   RP1B                                                             
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
         CLI   DMCB,5              TEST FRI,SAT,SUN                             
         BNL   *+8                                                              
         MVI   QOPT2,C'E'          NO - ERASE 49RQ                              
RP1B     DS    0H                                                               
         ZAP   RQCNT,=P'0'                                                      
         ZAP   TRQCNT,=P'0'                                                     
         CLI   QOPT2,C'E'          ERASE OPTION - NO FILE READ                  
         BNE   RP1D                                                             
*                                                                               
         MVC   P(37),=C'**ERASE MODE - FILE PP49RQN  ERASED**'                  
         MVC   P+26(2),SYSNAID                                                  
         BAS   RE,RPRT                                                          
         B     RP51                                                             
*                                                                               
RP1D     DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         CLI   QAGENCY,C' '                                                     
         BE    *+10                                                             
         MVC   KEY(2),QAGENCY                                                   
RP2      DS    0H                                                               
         GOTO1 HIGH                                                             
         B     *+8                                                              
RP4      DS    0H                                                               
         GOTO1 SEQ                                                              
         CLI   KEY,X'FF'                                                        
         BE    RP30                                                             
         CLI   KEY+3,1                                                          
         BE    RP30                AGY HDR                                      
         BL    RP4                                                              
         CLI   KEY+3,X'21'                                                      
         BL    RP20                                                             
         BH    RP22                                                             
*                                  BUY POINTER                                  
         CLC   KEY+16(3),DATLIM                                                 
         BL    RP4                      INS DATE PAST                           
*                                                                               
         MVC   SAVKEYS,KEY       SAVE KEYS FOR RESTORE OF SEQ READ              
*                                                                               
         LA    R7,PBUYREC                                                       
         ST    R7,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         L     R1,DATCON                                                        
         L     R2,GETDAY                                                        
         L     R3,ADDAY                                                         
         L     R4,DATAMGR                                                       
         L     R5,GETPROF                                                       
         STM   R1,R5,WORK                                                       
         RELOC (R3)                                                             
         GOTO1 =V(GENIOS),DMCB,(7,PBUYREC),PAGYREC,REQCRDH,WORK,RR=R3           
*                                                                               
         CLI   DMCB+4,0            TEST NEED TO RESTORE SEQ                     
         BE    RP8                                                              
         MVC   KEY(64),SAVKEYS                                                  
         GOTO1 HIGH                                                             
*                                                                               
RP8      DS    0H                                                               
         CLI   REQCRD,C' '         NO IO                                        
         BNH   RP10                                                             
*                                                                               
*                          MUST READ 72 PROFIULE AND POSSIBLY                   
*                          SET FORM TYPE                                        
*                                                                               
         BAS   RE,GET72OP                                                       
         MVC   REQOUT(6),LAST72O       6 CHARS                                  
*                                                                               
         AP    RQCNT,=P'1'                                                      
         CLI   QOPT1,C'Y'                                                       
         BNE   RP9                                                              
         MVC   P+1(80),REQCRD                                                   
         MVC   PSECOND(7),=C'HEADER='                                           
         MVC   PSECOND+7(26),REQCRDH                                            
         MVC   PSECOND+40(11),=C'HEADER HEX='                                   
         GOTO1 HEXOUT,DMCB,REQCRDH,PSECOND+54,26,=C'N'                          
         BAS   RE,RPRT                                                          
*                                                                               
RP9      DS    0H                                                               
         CLI   QOPT2,C'N'                                                       
         BE    RP10                                                             
         LA    R1,PP49RQN                                                       
         LA    R0,REQCRDH                                                       
         PUT   (1),(0)                                                          
*                                                                               
RP10     DS    0H                                                               
         B     RP4                                                              
*                                                                               
RP20     DS    0H                                                               
         MVI   KEY+3,X'21'                                                      
         B     RP2                                                              
*                                                                               
RP22     DS    0H                                                               
         MVI   KEY+3,X'FF'                                                      
         B     RP2                                                              
*                                                                               
RP30     DS    0H                                                               
         CLI   IOSW,C'Y'                                                        
         BNE   RP34                                                             
         BAS   RE,RPRT                                                          
         MVC   P+1(2),PAGYKAGY                                                  
         MVC   P+4(1),PAGYKMED                                                  
         MVC   P+7(33),PAGYNAME                                                 
         EDIT  (P4,RQCNT),(5,P+44)                                              
         OI    P+44+4,C'0'                                                      
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         AP    TRQCNT,RQCNT                                                     
         ZAP   RQCNT,=P'0'                                                      
*                                                                               
RP34     DS    0H                                                               
         CLI   KEY,X'FF'                                                        
         BE    RP50                                                             
         CLI   QAGENCY,C' '                                                     
         BE    RP34B                                                            
         CLC   KEY(2),QAGENCY                                                   
         BH    EXIT                                                             
RP34B    DS    0H                                                               
         LA    R7,PAGYREC                                                       
         ST    R7,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVI   IOSW,C'N'                                                        
         CLI   PAGYPROF+22,C'N'                                                 
         BE    RP22                                                             
         CLI   PAGYPROF+22,C'C'                                                 
         BE    RP35                                                             
         CLC   PAGYPROF+22(2),=C'00'                                            
         BH    RP35                                                             
         CLC   PAGYPROF+24(2),=C'00'                                            
         BNH   RP22                                                             
*                                                                               
RP35     DS    0H                                                               
         MVI   IOSW,C'Y'                                                        
         B     RP20                                                             
*                                                                               
RP50     DS    0H                                                               
         MVC   P+1(12),=C'TOTAL IO''S ='                                        
         EDIT  (P4,TRQCNT),(6,P+14)                                             
         BAS   RE,RPRT                                                          
*                                                                               
         CLI   QOPT2,C'N'                                                       
         BE    RP52                                                             
RP51     DS    0H                                                               
*&&DO                                                                           
         LA    R1,PP49RQN                                                       
         CLOSER (1)                                                             
*&&                                                                             
*&&OS                                                                           
         CLOSE (PP49RQN,)                                                       
*&&                                                                             
*                                                                               
RP52     DS    0H                                                               
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   EXIT                                                             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,RPRT                                                          
*&&DO                                                                           
         LA    R1,PP49RQI                                                       
         OPENR (1)                                                              
*&&                                                                             
*&&OS                                                                           
         OPEN  (PP49RQI,(INPUT))                                                
*&&                                                                             
*                                                                               
RP54     DS    0H                                                               
         LA    R1,PP49RQI                                                       
         LA    R0,REQCRDH                                                       
         GET   (1),(0)                                                          
         MVC   P+1(80),REQCRD                                                   
         BAS   RE,RPRT                                                          
         B     RP54                                                             
*                                                                               
RP56     DS    0H                                                               
*&&DO                                                                           
         LA    R1,PP49RQI                                                       
         CLOSER (1)                                                             
*&&                                                                             
*&&OS                                                                           
         CLOSE (PP49RQI,)                                                       
*&&                                                                             
*                                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
RPRT     NTR1                                                                   
         MVC   HEAD4+01(14),=C'** PRINT N  **'                                  
         MVC   HEAD4+10(2),SYSNAID                                              
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                 CHECKS 72 PROFILES AND RETURNS                
*                                 OUTPUT IN LAST72O                             
GET72OP  NTR1                                                                   
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),=C'P0'                                                   
         MVC   WORK+2(2),=C'72'                                                 
         MVC   WORK+4(6),RQAGY      AGY/MED/CLT                                 
         CLC   LAST72(8),WORK+2                                                 
         BE    GET72X                                                           
         XC    LAST72O,LAST72O                                                  
         MVC   LAST72(8),WORK+2                                                 
         BAS   RE,READCLT          MUST READ CLIENT TO GET OFFICE               
         CLI   PCLTOFF,C' '                                                     
         BNH   GET72C                                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
GET72C   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,L72PROF,DATAMGR                                
         CLI   L72PROF+15,C' '                                                  
         BNH   GET72X                                                           
         CLI   L72PROF+15,C'*'                                                  
         BE    GET72X                                                           
         LA    R4,FORMTAB                                                       
         MVC   WORK(1),L72PROF+15                                               
GET72G   CLC   0(2,R4),=X'FFFF'                                                 
         BE    GET72X             FORM NOT FOUND                                
         CLC   0(1,R4),WORK                                                     
         BE    GET72L                                                           
         LA    R4,7(R4)                                                         
         B     GET72G                                                           
*                                                                               
GET72L   MVC   LAST72O,1(R4)                                                    
         MVI   LAST72O+4,C'K'         KEEP                                      
*                                                                               
GET72X   XIT1                                                                   
         EJECT                                                                  
READCLT  NTR1                    ROUTINE TO READ CLT FROM                       
*                                SORTED RECORD                                  
         MVC   SAVKEYC,KEY       ONLY USED TO SAVE KEY                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),RQAGY        AGY/MED                                      
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),RQAGY+3       CLIENT                                    
         CLC   KEY(10),PCLTREC                                                  
         BE    READC20          RETURN WITHOUT RESTORE OF SEQ READ              
         GOTO1 HIGH                                                             
         XC    PCLTREC(250),PCLTREC       CLEAR                                 
         CLC   KEY(10),KEYSAVE                                                  
         BNE   READC10                    NOT FOUND - DON'T DIE                 
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
READC10  MVC   KEY(64),SAVKEYS      RESTORE SEQ READ                            
         GOTO1 HIGH                                                             
         B     READCX                                                           
*                                                                               
READC20  MVC   KEY(64),SAVKEYC      ONLY NEED TO RESTORE KEY                    
*                                                                               
READCX   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*&&DO                                                                           
PP49RQN  DTFSD BLKSIZE=2128,RECFORM=FIXBLK,RECSIZE=106,DEVICE=3350,    X        
               IOAREA1=WRKFA1,WORKA=YES,TYPEFLE=OUTPUT                          
*&&                                                                             
*&&OS                                                                           
PP49RQN  DCB   DDNAME=PP49RQN,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00106,                                            X        
               BLKSIZE=02120,          DOS BLKSIZE=02120               X        
               MACRF=PM                                                         
*&&                                                                             
*                                                                               
*&&DO                                                                           
PP49RQI  DTFSD BLKSIZE=2120,RECFORM=FIXBLK,RECSIZE=106,DEVICE=3350,    X        
               IOAREA1=WRKFA1,WORKA=YES,TYPEFLE=INPUT,EOFADDR=RP56              
*&&                                                                             
*&&OS                                                                           
PP49RQI  DCB   DDNAME=PP49RQI,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00106,                                            X        
               BLKSIZE=02120,          DOS BLKSIZE=02120               X        
               MACRF=GM,                                               X        
               EODAD=RP56                                                       
*&&                                                                             
         SPACE 3                                                                
*                                                                               
*                                                                               
FORMTAB  DC    C'N',C'&&4AS  '           NEW                                    
         DC    C'L',C'&&CON  '           OLD                                    
         DC    X'FFFF'                                                          
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*&&DO                                                                           
WRKFA1   DS    2150C                                                            
*&&                                                                             
         SPACE 2                                                                
*                                                                               
*                                                                               
PPG1WRKD DSECT                                                                  
RQCNT    DS    PL4                                                              
TRQCNT   DS    PL4                                                              
DATLIM   DS    XL3                                                              
IOSW     DS    X                                                                
*                                                                               
SAVKEYS  DS    CL64             USE TO RESTORE SEQ READ                         
SAVKEYC  DS    CL64             USE TO SAVE KEY AND KEYSAVE                     
*                                                                               
LAST72   DS    CL8                                                              
LAST72O  DS    CL6                                                              
L72PROF  DS    CL16                                                             
*                                                                               
REQCRDH  DS    0CL106                                                           
       ++INCLUDE DMREQHDR                                                       
REQCRD   DS    CL80                                                             
         ORG   REQCRD                                                           
RQPROG   DS    CL2                                                              
RQAGY    DS    CL2                                                              
RQMED    DS    CL1                                                              
RQCLT    DS    CL3                                                              
RQDIV    DS    CL3                                                              
RQPRD    DS    CL3                                                              
         DS    CL6                                                              
RQEST    DS    CL3                                                              
         DS    CL3                                                              
RQPUB    DS    CL11                                                             
RQDATES  DS    CL12                                                             
         DS    CL19                                                             
RQSTR    DS    CL12                                                             
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025PPREPG102 01/13/14'                                      
         END                                                                    
