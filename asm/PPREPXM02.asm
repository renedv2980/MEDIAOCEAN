*          DATA SET PPREPXM02  AT LEVEL 045 AS OF 05/01/02                      
*PHASE PPXM02A,+0                                                               
         TITLE 'PPREPXM02 - ARMED SERVICES PRINTPAK TAPE'                       
PPXM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPXM02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPXMWKD,R8                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,FBUYREQ                                                     
         BE    REQF                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    REQL                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
*************************                                                       
*                       *                                                       
*      QOPT1 = Y        *                                                       
*    (WRITE REPORT)     *                                                       
*                       *                                                       
*************************                                                       
         EJECT                                                                  
*                                                                               
*        RUN FIRST                                                              
         SPACE 2                                                                
RUNF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        REQ FIRST                                                              
         SPACE 2                                                                
REQF     DS    0H                                                               
         ZAP   DOLTOTS,=P'0'                                                    
         ZAP   ANSTOTS,=P'0'                                                    
         SPACE                                                                  
         MVI   FCRDBUY,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   BLOPSW,C'Y'         TEST TAPE OPEN                               
         BE    REQF4                                                            
         MVI   BLOPSW,C'Y'                                                      
         LA    R2,PPXMTP                                                        
         OPEN  ((2),OUTPUT)                                                     
*                                                                               
REQF4    DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* REQUEST LAST                                                                  
         SPACE 2                                                                
REQL     DS    0H                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 REPORT              FOR BLANK LINE                               
         XC    TAPEREC,TAPEREC                                                  
         MVC   TPEST(14),=C'*** TOTALS ***'                                     
         MVC   TPDSPACE(10),=C'INSERTIONS'                                      
         MVC   TPCOST+2(8),EDPTRN1                                              
         ED    TPCOST+2(8),ANSTOTS                                              
         MVC   P(80),TAPEREC                                                    
         GOTO1 REPORT                                                           
         XC    TAPEREC,TAPEREC                                                  
         MVC   TPDSPACE(4),=C'COST'                                             
         MVC   TPDSPACE+13(17),EDPTRN2                                          
         LA    R1,TPDSPACE+29                                                   
         EDMK  TPDSPACE+13(17),DOLTOTS                                          
         BCTR  R1,0                                                             
         MVI   0(R1),C'$'                                                       
         MVC   P(80),TAPEREC                                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
EDPTRN1  DC    X'4020202020202020'                                              
EDPTRN2  DC    X'40202020202020202020202020204B2120'                            
         SPACE 2                                                                
*        RUN LAST                                                               
         SPACE 2                                                                
RUNL     DS    0H                                                               
         CLI   BLOPSW,C'Y'                                                      
         BNE   RUNL6                                                            
         CLOSE (PPXMTP)                                                         
*                                                                               
RUNL6    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        PROC BUY                                                               
*                                                                               
PRBUY    DS    0H                                                               
         MVC   TAPEREC,SPACES                                                   
         MVC   TPAGY,PBUYKAGY                                                   
         MVI   TPMD,C'P'                                                        
         MVC   TPMD+1(1),PBUYKMED                                               
         MVC   TPCLT(3),PBUYKCLT                                                
         MVC   TPPRD,PPRDKPRD                                                   
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,PBUYKEST                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPEST,DUB                                                        
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(0,TPINSDTE)                            
         GOTO1 DATCON,DMCB,(3,PBDIDAT2),(0,WORK)                                
         MVC   TPALTDTE,WORK+4                        ONLY WANT DD              
         SPACE                                                                  
         GOTO1 PUBEDIT,DMCB,(8,PBUYKPUB),(C'S',WORK)                            
         MVC   TPPUB(8),WORK                                                    
         CLI   WORK+9,C'0'                                                      
         BL    PB10                                                             
         MVC   TPPUB+8(2),WORK+9                                                
         MVC   TPED,WORK+12                                                     
         B     PB20                                                             
PB10     MVC   TPED,WORK+9                                                      
         SPACE                                                                  
PB20     DS    0H                                                               
         CLI   QMEDIA,C'O'        IS IT OUTDOOR                                 
         BE    PB22                                                             
         LA    R5,PPBYWORK         SPACE DESCRIPTON                             
         USING PPBYOUTD,R5                                                      
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         MVC   PBYODTCN,DATCON                                                  
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVI   PBYOCTL,X'2A'            ZZZ + COMS + NO LINE NO.                
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
         MVC   TPDSPACE,PBYOSPC                                                 
         B     PB23                                                             
         SPACE                                                                  
PB22     DS    0H                            OUTDOOR MEDIA                      
         CLI   PBDSPACE,X'FF'        ARE THERE NUMBERS FOR BILLBOARDS           
         BNE   PB22A                                                            
         UNPK  TPDSPACE(4),PBDSPACE+1(3)                                        
         OI    TPDSPACE+3,X'F0'                                                 
         UNPK  TPDSPACE+4(4),PBDSPACE+4(3)                                      
         OI    TPDSPACE+7,X'F0'                                                 
         UNPK  TPDSPACE+8(4),PBDSPACE+7(3)                                      
         OI    TPDSPACE+11,X'F0'                                                
         B     PB23                                                             
         SPACE                                                                  
PB22A    UNPK  TPDSPACE(12),=P'0'           NO NUMBERS,DISPLAY ZEROS            
         OI    TPDSPACE+11,X'F0'                                                
         SPACE                                                                  
PB23     L     R3,PYABLE                                                        
         CVD   R3,DUB                                                           
         UNPK  TPCOST,DUB                                                       
         AP    DOLTOTS,DUB         ADD TO COST TOTS                             
         SPACE                                                                  
         MVI   TPANCM,C'1'                                                      
         AP    ANSTOTS,=P'1'       ADD TO INSERT TOTS                           
         SPACE                                                                  
         MVI   ELCODE,X'66'                                                     
         LA    R6,PBUYREC+33                                                    
         CLI   0(R6),X'66'                                                      
         BE    PB25A                                                            
PB25     BAS   RE,NXTELEM                                                       
         BNE   PB27                                                             
PB25A    CLC   =C'LEAD=',2(R6)                                                  
         BNE   PB25                                                             
         MVC   TPLDGEN,7(R6)                                                    
         SPACE                                                                  
PB27     CLI   QOPT1,C'Y'                                                       
         BNE   PB30                                                             
         MVC   P(80),TAPEREC                                                    
         GOTO1 REPORT                                                           
PB30     PUT   PPXMTP,TAPEREC                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
NXTELEM  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    NXTELX                                                           
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DS    H'0'                                                             
         AR    R6,R0                                                            
NXT10    CLC   ELCODE,0(R6)                                                     
         BNE   NXTELEM                                                          
         BR    RE                  EXIT WITH CC =                               
NXTELX   LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT =                           
         SPACE 3                                                                
PPXMTP   DCB   DDNAME=PPXMTP,          DOS SYS008                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=00800,          DOS BLKSIZE=00150               X        
               MACRF=PM                                                         
*                                                                               
ELCODE   DS    CL1                                                              
OUTA     DS    150C                                                             
*                                                                               
         EJECT                                                                  
*                                  ** AREA FOR TAPE RECORD **                   
TAPEREC  DS    0CL80                                                            
TPAGY    DS    CL2                 AGENCY CODE                                  
TPMD     DS    CL2                 MEDIA (PM,PN,PO,PS,PT)                       
TPCLT    DS    CL4                 CLIENT CODE   *                              
TPPRD    DS    CL3                 PRODUCT CODE  *                              
TPEST    DS    CL6                 ESTIMATE CODE *                              
TPINSDTE DS    CL6                 INSERTION DATE (YYMMDD)                      
TPALTDTE DS    CL2                 ALTERNATE DATE (DD)                          
TPPUB    DS    CL12                PUBLICATION NUMBER *                         
TPED     DS    CL3                 EDITION CODE *                               
TPDSPACE DS    CL20                ISNERTION SPACE DESCRIPTION                  
TPCOST   DS    CL9                 COST (9999999.99)                            
TPANCM   DS    CL3                 NUMBER OF ANNOUNCEMENTS(1 FOR PRINT)         
TPLDGEN  DS    CL1                 LEAD GENERATION CODE                         
         DS    CL6                 SPARE                                        
TPBKDTA  DS    CL1                 BACK UP DATA ('1'=YES)                       
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 3                                                                
PPXMWKD  DSECT                                                                  
ANSTOTS  DS    F                                                                
DOLTOTS  DS    D                                                                
BLOPSW   DS    X                                                                
PPBYWORK DS    CL700                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045PPREPXM02 05/01/02'                                      
         END                                                                    
