*          DATA SET PPREPXJ02  AT LEVEL 028 AS OF 05/01/02                      
*PHASE PPXJ02A,+0                                                               
*INCLUDE BINSRCH2                                                               
         TITLE 'PPREPXJ02 - PRINTPAK BUCKET TAPE INTERFACE'                     
PPXJ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPXJ02,RR=5                                                    
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPXJWKD,R8                                                       
         ST    R5,RELO                                                          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,FBUYREQ                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBKT                                                     
         BE    PRBKT                                                            
         CLI   MODE,PROCBIL                                                     
         BE    PRBILL                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        RUN FIRST                                                              
*                                                                               
RUNF     DS    0H                                                               
*                               ** SET BIN PARS **                              
         SR    R0,R0               ADDRS OF INSERT                              
         LA    R1,BINTBL           ADDRS OF TBL                                 
         SR    R2,R2               NUM OF RECS SO FAR                           
         LA    R3,30               L'REC                                        
         LA    R4,2                BYTE 0=KEY DISP,1-3=L'KEY                    
         LA    R5,24               MAX NUM OF RECS                              
         STM   R0,R5,BINPARS                                                    
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 2                                                                
*        REQ FIRST                                                              
         SPACE 2                                                                
REQF     DS    0H                                                               
         MVI   FCRDBKT,C'Y'                                                     
         MVI   FCRDBILL,C'Y'                                                    
         MVI   FCRDEST,C'Y'                                                     
         MVI   FCRDBUY,C'N'                                                     
*                                                                               
         CLI   BLOPSW,C'Y'         TEST TAPE OPEN                               
         BE    REQF4                                                            
         MVI   BLOPSW,C'Y'                                                      
         LA    R2,PPXJTP                                                        
         OPEN  ((2),OUTPUT)                                                     
*                                                                               
REQF4    DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        RUN LAST                                                               
         SPACE 2                                                                
RUNL     DS    0H                                                               
         CLI   BLOPSW,C'Y'                                                      
         BNE   RUNL6                                                            
         CLOSE (PPXJTP)                                                         
*                                                                               
RUNL6    DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        PROC BILL                                                              
         SPACE 2                                                                
PRBILL   DS    0H                                                               
         BAS   RE,CLRWRK                                                        
         MVC   BINDTE,PBILKMOS                                                  
         AP    GRSBILL,PBILLGRS                                                 
         AP    NETBILL,PBILLNET                                                 
         LA    R5,BINDTE                                                        
         GOTO1 BINSRCH,BINPARS,(1,(R5))                                         
         CLI   0(R1),1             WAS REC NOT FOUND AND SO INSERTED            
         BE    EXIT                                                             
         L     R2,0(R1)            NO, REC FOUND, ADD TO TOTALS                 
         AP    16(7,R2),GRSBILL                                                 
         AP    23(7,R2),NETBILL                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        PROC BKT                                                               
         SPACE 2                                                                
PRBKT    DS    0H                                                               
         L     R7,ADBKT                                                         
         USING PBKKEY,R7                                                        
         LA    R6,PBKKEY+33                                                     
         USING BKELEM,R6                                                        
         MVI   ELCODE,X'22'        ONLY X'22' ELEM                              
         CLI   0(R6),X'22'                                                      
         BE    PBK20                                                            
         SPACE                                                                  
PBK10    BAS   RE,NXTELEM                                                       
         BNE   WRTREC                                                           
         SPACE                                                                  
PBK20    BAS   RE,CLRWRK                                                        
         MVC   BINDTE,BKYM                                                      
         AP    GRSORD,BKOGRS                                                    
         AP    NETORD,BKONET                                                    
         SP    NETORD,BKOCD                                                     
         LA    R5,BINDTE                                                        
         GOTO1 BINSRCH,BINPARS,(1,(R5))                                         
         CLI   0(R1),1             WAS REC NOT FOUND AND SO INSERTED            
         BE    PBK10                                                            
         L     R2,0(R1)            NO, REC FOUND, ADD BUCKETS                   
         AP    2(7,R2),GRSORD                                                   
         AP    9(7,R2),NETORD                                                   
         B     PBK10                                                            
         SPACE                                                                  
WRTREC   DS    0H                                                               
         DROP  R6                                                               
         LA    R3,BINTBL                                                        
         L     R4,BINPARS+8        REC COUNT FOR BCT LIMIT                      
         SPACE                                                                  
WR10     MVI   TPSYS,C'P'                                                       
         MVC   TPMD,PCLTKMED                                                    
         MVC   TPCLT,PCLTKCLT                                                   
         MVC   TPCLTNUM,PCLTNUM                                                 
         MVC   TPPRD,PPRDKPRD                                                   
         MVC   TPPRDACT,PPRDACCT                                                
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,PBKKEST                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPEST,DUB                                                        
         SPACE                                                                  
         MVC   WORK(2),0(R3)                                                    
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(0,TPYSRVC)                                 
         UNPK  TPGORD,2(7,R3)                                                   
         UNPK  TPNORD,9(7,R3)                                                   
         UNPK  TPGBLD,16(7,R3)                                                  
         UNPK  TPNBLD,23(7,R3)                                                  
         CLI   QOPT1,C'Y'          WRITE REPORT OPTION                          
         BNE   WR18                                                             
         MVC   P(71),TAPEREC                                                    
         GOTO1 REPORT                                                           
WR18     PUT   PPXJTP,TAPEREC                                                   
WR20     LA    R3,30(R3)                                                        
         BCT   R4,WR10                                                          
         LA    RE,BINTBL              CLEAR BINTBL                              
         LA    RF,720                                                           
         XC    BINPARS+8(4),BINPARS+8                                           
         XCEF                                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* CLEAR WORKAREA TO PACKED ZEROS *                                              
CLRWRK   DS    0H                                                               
         XC    BINDTE,BINDTE                                                    
         LA    R4,4                BCT LIMIT                                    
         LA    R5,GRSORD                                                        
CT12     ZAP   0(7,R5),=P'0'                                                    
         LA    R5,7(R5)                                                         
         BCT   R4,CT12                                                          
         BR    RE                                                               
         SPACE                                                                  
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
PPXJTP   DCB   DDNAME=PPXJTP,          DOS SYS008                      X        
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
TPSYS    DS    CL1                 SYSTEM                                       
TPMD     DS    CL1                 MEDIA                                        
TPCLT    DS    CL3                 CLIENT CODE                                  
TPCLTNUM DS    CL3                 CLIENT NUMBER                                
TPPRD    DS    CL3                 PRD CODE                                     
TPPRDACT DS    CL5                 PRD ACCT                                     
TPEST    DS    CL3                 ESTIMATE                                     
TPYSRVC  DS    CL2                 YEAR OF SERVICE                              
TPMSRVC  DS    CL2                 MONTH OF SERVICE                             
TPGORD   DS    CL12                GROSS ORDERED                                
TPNORD   DS    CL12                NET ORDERED                                  
TPGBLD   DS    CL12                GROSS BILLED                                 
TPNBLD   DS    CL12                NET BILLED                                   
         DS    CL9                                                              
         SPACE                                                                  
*                                                                               
BINPARS  DS    6F                                                               
RELO     DS    F                                                                
*                                                                               
         LTORG                                                                  
         SPACE                                                                  
*                                                                               
PPXJWKD  DSECT                                                                  
BLOPSW   DS    X                                                                
*                                                                               
BINDTE   DS    XL2 *          * DSECT TO COVER EACH RECORD                      
GRSORD   DS    PL7 *              ( 30 BYTES PER RECORD)                        
NETORD   DS    PL7 *                                                            
GRSBILL  DS    PL7 *                                                            
NETBILL  DS    PL7 *                                                            
*                                                                               
BINTBL   DS    0CL720              TABLE TO COVER 2 YEARS                       
*                               (2 X 12 MOS X 30 BYTES PER REC )                
         PRINT OFF                                                              
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PBKREC                                                         
       ++INCLUDE DDBKELEM                                                       
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028PPREPXJ02 05/01/02'                                      
         END                                                                    
