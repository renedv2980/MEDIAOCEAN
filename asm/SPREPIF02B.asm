*          DATA SET SPREPIF02B AT LEVEL 036 AS OF 05/01/02                      
*PHASE SPIF02A                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE COVAIL                                                                 
         TITLE 'SPIF02 - SET MSTAT REC INVOICE ELEMS PAID'                      
         SPACE 2                                                                
***********************************************************************         
*        QOPT1  X=EXCLUDE PRD=POL AND EST=000                                   
*        QOPT2  Y=LIST STATUS RECS                                              
*        QOPT3  Y=LIST INVOICE RECORDS                                          
*        QOPT4  E=TABLE ENTRY ONLY IF ERROR                                     
*        QOPT5  Y=GENERATE I2 REQUESTS                                          
***********************************************************************         
         SPACE 2                                                                
SPIF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPIF02,CLEAR=YES,RR=R2                                         
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         USING SPIF02+4096,R5                                                   
         LA    R8,MINBLK                                                        
         USING MINBLKD,R8                                                       
         LA    R7,P                                                             
         USING LIND,R7                                                          
         LA    RF,HDRHOOK                                                       
         ST    RF,HEADHOOK                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    MLP00                                                            
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
RUNF     DS    0H                                                               
*                                  GET MEMORY FOR TABLES                        
         L     R0,=A(SCTABDL*SCTMAX)                                            
         GOTO1 =V(COVAIL),DMCB,C'GET',(R0),(R0)                                 
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                                                         
         MVC   0(8,RF),=C'*SCTAB**'                                             
         LA    RF,8(RF)                                                         
         ST    RF,ASCTAB                                                        
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
REQF     DS    0H                                                               
         MVI   FCRDBUYS,C'N'                                                    
         MVI   FCRDGOAL,C'N'                                                    
         MVI   FORCEHED,C'Y'                                                    
         LA    RF,HDRHOOK                                                       
         ST    RF,HEADHOOK                                                      
         MVC   NISW,QOPT5                                                       
*                                                                               
         B     EXIT                                                             
*                                                                               
RUNL     DS    0H                                                               
         CLI   QOPT5,C'Y'          CKREQ OPTION                                 
         BNE   EXIT                                                             
         CLOSE CKREQ                                                            
         B     EXIT                                                             
         EJECT                                                                  
* CLTFRST                                                                       
*                                                                               
MLP00    DS    0H                                                               
*                                  SET BSPARS                                   
         SR    R0,R0                                                            
         L     R1,ASCTAB                                                        
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,SCTABDL                                                       
         LA    R4,SCTABKL                                                       
         STM   R0,R4,BSPARS                                                     
         L     RF,=A(SCTMAX)                                                    
         ST    RF,BSPARS+20                                                     
         BAS   RE,INIT                                                          
         EJECT                                                                  
**********************************************************************          
*        STATUS RECORD READ                                                     
**********************************************************************          
*                                                                               
         CLI   QOPT2,C'Y'          ARE WE LISTING THEM                          
         BNE   MLP08                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(18),=C'**STATUS RECORDS**'                                     
         MVI   RPTSW,C'S'                                                       
         GOTO1 REPORT                                                           
*                                                                               
MLP08    DS    0H                                                               
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING MSRKEYD,R2                                                       
*                                                                               
         MVI   MSRKTYPE,MSRKTYPQ                                                
         MVI   MSRKSUB,MSRKSUBQ                                                 
         MVC   MSRKAM,BAGYMD                                                    
         MVC   MSRKCLT,BCLT                                                     
         MVC   XKEYSAVE,XKEY                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     MLP10B                                                           
*                                                                               
MLP10    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
MLP10B   DS    0H                                                               
         CLC   XKEY(5),XKEYSAVE    THRU CLIENT                                  
         BNE   MLP80                                                            
*                                                                               
         CLI   MSRKMINK,X'FF'      ONLY DEAL HERE WITH MINIO 'MASTERS'          
         BNE   MLP10                                                            
*                                                                               
         CLI   MSRKEST,0           SKIP IF EST=0                                
         BE    MLP12                                                            
         CLI   MSRKPRD,X'FF'       OR PRD = POL                                 
         BE    MLP12                                                            
         CLI   MSRKPRD,0           OR NULL                                      
         BE    MLP12                                                            
         B     MLP14                                                            
*                                                                               
MLP12    DS    0H                                                               
         CLI   QOPT1,C'X'          TEST EXCLUDING POL AND EST=000               
         BE    MLP10                                                            
*                                                                               
MLP14    DS    0H                                                               
         MVC   HALF,MSRKMOS        CHECK RIGHT MOS                              
         XC    HALF,=X'FFFF'                                                    
         MVC   BMOS,HALF                                                        
         GOTO1 DATCON,DMCB,(2,HALF),DUB                                         
         MVC   EMOS,DUB                                                         
         CLI   QSTART,C' '                                                      
         BNH   *+14                                                             
         CLC   EMOS,QSTART                                                      
         BL    MLP10                                                            
         CLI   QEND,C' '                                                        
         BNH   *+14                                                             
         CLC   EMOS,QEND                                                        
         BH    MLP10                                                            
*                                                                               
         MVC   SVKEY,XKEY          SAVE KEY FOR SEQS                            
         MVC   SVKEYSAV,XKEYSAVE                                                
         MVC   CKRCARD,SPACES                                                   
*                                                                               
         MVC   MINMKEY,XKEY        SET MINIO KEY                                
         GOTO1 VMINIO,DMCB,('MINOPN',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVHDREL,SVHDREL                                                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'10'       GET HDR ELEM                                 
         MVI   MINFILTL,1                                                       
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0            ALL ERRS AS NOT FOUND                        
         BNE   *+10                                                             
         MVC   SVHDREL,MELEM                                                    
*                                                                               
         XC    SVPAYEL,SVPAYEL                                                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'14'       GET PAY ELEM                                 
         MVI   MINFILTL,1                                                       
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0            ALL ERRS AS NOT FOUND                        
         BNE   *+10                                                             
         MVC   SVPAYEL,MELEM                                                    
*                                                                               
         LA    R4,SVPAYEL+MSRIPDAT-MSRIPELD                                     
         LA    R6,SVHDREL+MSRSTDAT-MSRSTELD                                     
         CLC   0(6,R4),0(R6)       UNLESS PAID AFTER STATUS DATE                
         BNH   MLP78               SKIP                                         
         CLI   SVPAYEL+MSRIPST-MSRIPELD,C'Y'                                    
         BNE   MLP78                                                            
*                                                                               
MLP70    DS    0H                                                               
         LA    R2,MINMKEY                                                       
         CLI   QOPT2,C'Y'          TEST TO LIST STATUS RECS                     
         BNE   MLP71                                                            
         BAS   RE,FMTMST                                                        
         GOTO1 REPORT                                                           
*                                                                               
MLP71    DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'12'       GET INVOICE ELEMS                            
         MVI   MINFILTL,1                                                       
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         B     MLP72B                                                           
*                                                                               
MLP72    DS    0H                                                               
         GOTO1 VMINIO,DMCB,('MINSEQ',MINBLKD)                                   
MLP72B   DS    0H                                                               
         CLI   MINERR,0            ALL ERRS AS NOT FOUND                        
         BNE   MLP76                                                            
*                                                                               
         TM    MELEM+MSRINST-MSRINELD,MSRINPYQ                                  
         BO    MLP72                                                            
*                                                                               
         OI    MELEM+MSRINST-MSRINELD,MSRINPYQ                                  
         GOTO1 VMINIO,DMCB,('MINWRT',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HEXOUT,DMCB,MELEM,P+3,31,=C'N'                                   
         GOTO1 REPORT                                                           
         B     MLP72                                                            
*                                                                               
MLP76    DS    0H                                                               
         GOTO1 VMINIO,DMCB,('MINCLS',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MLP78    DS    0H                                                               
         MVC   XKEY,SVKEY          RESTORE KEY FOR SEQ                          
         MVC   XKEYSAVE,SVKEYSAV                                                
         LA    R2,XKEY                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     MLP10                                                            
*                                                                               
MLP80    DS    0H                                                               
         MVI   MODE,CLTLAST        ON TO NEXT CLIENT                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE MINIO, ETC..                                                
***********************************************************************         
INIT     NTR1                                                                   
         CLI   FRSTSW,0                                                         
         BNE   INITX                                                            
         MVI   FRSTSW,1                                                         
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 LOADER,DMCB,=CL8'T00A74',0,0                                     
         MVC   VMINIO,DMCB+4          SET MINIO ADDRESS                         
*                                                                               
*        INITIALIZE MINIO VALUES                                                
*                                                                               
         MVC   MINRECUP,RECUP        A(RECUP)                                   
         MVC   MINCOMF,ACOMFACS      A(COMFACS)                                 
         MVI   MINOPEN,C'N'          SET NOT OPEN                               
         MVC   MINFIL,=CL8'XSPFIL'   FILE NAME                                  
         MVC   MINDIR,=CL8'XSPDIR'   DIR NAME                                   
         MVI   MINFKLEN,L'MSRKEY     KEY LENGTH                                 
         MVI   MINEKLEN,L'MSRKMINK   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'MSRKMAST   DISPLACEMENT TO ELEMENT KEY                
         MVI   MINNCTL,L'MSRDSTAT    NUMBER OF CONTROL BYTES                    
         MVC   MINFRCLM,=H'3950'     MAXIMUM RECORD LENGTH                      
*                                                                               
         L     RF,=A(MBUFF1)         POINT TO START OF MINIO BUFFERS            
         A     RF,RELO                                                          
         ST    RF,MINBUFF            A(FIRST BUFFER)                            
*                                                                               
         MVI   MINNBUF,2             TWO BUFFERS                                
*                                                                               
         L     RF,=A(MRTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,MINRTAB            A(AREA FOR RECORD TABLE)                   
*                                                                               
         MVC   MINRTABL,=Y(L'MRTAB)  LENGTH OF RECORD TABLE                     
*                                                                               
         LA    RF,MELEM              A(AREA FOR ELEM OR CLUSTER)                
         ST    RF,MINELEM                                                       
*                                                                               
         XC    0(L'MELEM,RF),0(RF)  CLEAR MINELEM AREA                          
*                                                                               
         MVC   MINMAXEL,=Y(L'MELEM)      MAX LENGTH OF ELEM OR CLUSTER          
*                                                                               
         MVI   MINDELSW,C'N'       DON'T PROCESS DELETED RECORDS                
         MVC   MINWRITE,RCWRITE    ENABLE/DISABLE WRITES                        
         MVI   MINRDUP,C'Y'        DO READ FOR UPDATE                           
*                                                                               
         CLI   QOPT5,C'Y'          CKREQ OPTION                                 
         BNE   INITX                                                            
         OPEN  (CKREQ,(OUTPUT))                                                 
*                                                                               
INITX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        FMTMST - STATUS LINE                                                   
***********************************************************************         
FMTMST   NTR1                                                                   
         USING MSRKEYD,R2                                                       
         MVC   LINMED,QMED                                                      
         MVC   LINCLT,CLT                                                       
*                                                                               
         L     RF,ADCLT                                                         
         LA    R0,L'CLIST/4                                                     
         LA    RF,CLIST-CLTHDR(RF)                                              
*                                                                               
FL3      DS    0H                                                               
         CLC   MSRKPRD,3(RF)                                                    
         BE    FL4                                                              
         LA    RF,4(RF)                                                         
         BCT   R0,FL3                                                           
*                                                                               
FL4      DS    0H                                                               
         MVC   PRD,0(RF)                                                        
         MVC   LINPRD,0(RF)                                                     
*                                                                               
         EDIT  (B1,MSRKEST),EST,FILL=0                                          
         MVC   LINEST,EST                                                       
*                                                                               
         GOTO1 MSUNPK,DMCB,MSRKMKT,WORK,STA                                     
         CLI   STA+4,C' '                                                       
         BH    *+8                                                              
         MVI   STA+4,C'T'                                                       
         MVC   LINSTA,STA                                                       
         MVC   LINMKT,WORK                                                      
*                                                                               
         MVC   LINMOS(4),EMOS         MONTH OF SERVICE                          
         GOTO1 HEXOUT,DMCB,36(R2),LINDA,4,=C'N'                                 
*                                                                               
FLX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
*        HDRHOOK - HEADHOOK                                                     
***********************************************************************         
HDRHOOK  NTR1                                                                   
         LA    R7,HEAD4                                                         
         USING LIND,R7                                                          
         CLI   RPTSW,C'T'                                                       
         BNE   HDRHX                                                            
         MVC   LININV+01(18),=C'------STATUS------'                             
         MVC   LININV+21(18),=C'-----INVOICE------'                             
*                                                                               
         MVC   LINICST+1(08),=C'APPROVAL'                                       
         MVC   LINIPST+1(07),=C'PAYMENT'                                        
*                                                                               
HDRHX    DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
CKREQ    DCB   DDNAME=CKREQ,                                           X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=PM,                                                        
*                                                                               
MINBLK   DS    XL(MINBLKL)                                                      
MRTAB    DS    XL(200*(6+L'MSRKMAST))                                           
MBUFF1   DS    XL4000                                                           
MBUFF2   DS    XL4000                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'**SCTAB*'                                                    
SCTAB    DS    0X                                                               
         ORG   *+(SCTABDL*SCTMAX)                                               
         DS    XL1                                                              
*                                                                               
*        SPACE 2                                                                
WORKD    DSECT                                                                  
XKEY     DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
SVKEY    DS    XL(L'XKEY)                                                       
SVKEYSAV DS    XL(L'XKEY)                                                       
*                                                                               
PSTAT    DS    CL1                                                              
A2PAID   DS    CL1                                                              
INVSW    DS    CL1                                                              
FRSTSW   DS    XL1                                                              
EMOS     DS    CL4                                                              
SVICSEL  DS    XL(MSRICLNQ)                                                     
SVPAYEL  DS    XL(MSRIPLNQ)                                                     
SVHDREL  DS    XL(MSRSTLNQ)                                                     
LASTSTA  DS    XL3                                                              
LASTMKT  DS    XL2                                                              
MELEM    DS    XL256                                                            
SVELEM   DS    XL256                                                            
VMINIO   DS    A                                                                
ANXTERR  DS    A                                                                
CKRCARD  DS    CL80                                                             
BSPARS   DS    6F                                                               
BSWORK   DS    XL(SCTABDL)                                                      
BMOS     DS    XL2                                                              
RPTSW    DS    XL1                                                              
STISTAT  DS    CL5                                                              
HLDMKT   DS    H                                                                
ASCTENT  DS    A                                                                
ASCTAB   DS    A                                                                
NISW     DS    CL1                                                              
SREC     DS    XL150                                                            
*                                                                               
         SPACE 2                                                                
LIND     DSECT                     DSECT FOR PRINT LINE                         
LINMED   DS    CL1                                                              
         DS    CL1                                                              
LINCLT   DS    CL3                                                              
         DS    CL1                                                              
LINSTA   DS    CL5                                                              
         DS    CL1                                                              
LINMKT   DS    CL4                                                              
         DS    CL1                                                              
LINPRD   DS    CL3                                                              
         DS    CL1                                                              
LINEST   DS    CL3                                                              
         DS    CL1                                                              
LINMOS   DS    CL5                                                              
         DS    CL1                                                              
LINDA    DS    CL8                                                              
         DS    CL3                                                              
LINSTAT  DS    CL3                                                              
         DS    CL2                                                              
LINERRS  DS    CL19                                                             
         DS    CL1                                                              
LININV   DS    CL40                                                             
         DS    CL1                                                              
LINICST  DS    CL1                                                              
         DS    CL1                                                              
LINICDAT DS    CL8                                                              
         DS    CL1                                                              
LINIPST  DS    CL1                                                              
         DS    CL1                                                              
LINIPDAT DS    CL8                                                              
         SPACE 2                                                                
SCTABD   DSECT                     DSECT FOR PAY TABLE                          
SCTSTA   DS    XL3                                                              
SCTMKT   DS    XL2                                                              
SCTPRD   DS    XL1                                                              
SCTEST   DS    XL1                                                              
SCTMOS   DS    XL2                                                              
SCTABKL  EQU   *-SCTABD                                                         
SCTISPTS DS    XL4                 INV SPOTS                                    
SCTIGRS  DS    XL4                   GROSS                                      
SCTMSPTS DS    XL4                 MSTATUS SPOTS                                
SCTMGRS  DS    XL4                   GROSS                                      
SCTMDA   DS    XL4                   DISK ADDRESS                               
SCTICST  DS    C                                                                
SCTICDAT DS    XL2                                                              
SCTIPST  DS    C                                                                
SCTIPDAT DS    XL2                                                              
SCTABDL  EQU   *-SCTABD                                                         
*                                                                               
SCTMAX   EQU   5000                                                             
*                                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
       ++INCLUDE SPGENMSR                                                       
       ++INCLUDE SPGENSNV                                                       
*                                                                               
*++INCLUDE DDMINBLK/SPREPMODES/SPREPWORKD/SPGENCLT                              
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036SPREPIF02B05/01/02'                                      
         END                                                                    
