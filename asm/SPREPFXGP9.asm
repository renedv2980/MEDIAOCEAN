*          DATA SET SPREPFXGP9 AT LEVEL 014 AS OF 05/01/02                      
*PHASE SPFX02G+0                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'SPFX02 - FIX BUY GUIDELINE STATUS RECS'                         
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,CLEAR=YES,RR=R2                                         
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
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
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
REQF     DS    0H                                                               
         MVI   FCRDBUYS,C'N'                                                    
         MVI   FCRDGOAL,C'N'                                                    
         MVI   FORCEHED,C'Y'                                                    
         LA    RF,HDRHOOK                                                       
         ST    RF,HEADHOOK                                                      
         B     EXIT                                                             
*                                                                               
*        RUNL - RUNLAST                                                         
*                                                                               
RUNL     DS    0H                                                               
         LA    R2,BGREQF                                                        
         CLOSE (R2)                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* CLTFRST                                                                       
*                                                                               
MLP00    DS    0H                                                               
         BAS   RE,INIT                                                          
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING BGRKEYD,R2                                                       
*                                                                               
         MVI   BGRKTYPE,BGRKTYPQ                                                
         MVI   BGRKSUB,BGRKSUBQ                                                 
         MVC   BGRKAM,BAGYMD                                                    
         MVC   BGRKCLT,BCLT                                                     
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
         CLI   BGRKMINK,X'FF'      ONLY DEAL HERE WITH MINIO 'MASTERS'          
         BNE   MLP10                                                            
*                                                                               
         CLI   BGRKEST,0           SKIP IF EST=0                                
         BE    MLP10                                                            
         CLI   BGRKPRD,0           OR PRD=NULL                                  
         BE    MLP10                                                            
         CLI   BGRKPRD,X'FF'       OR PRD=POL                                   
         BE    MLP10                                                            
*                                                                               
         MVC   SVKEY,XKEY          SAVE KEY FOR SEQS                            
*                                                                               
         MVC   MINMKEY,XKEY        SET MINIO KEY                                
*                                                                               
         LA    R7,P                                                             
         LA    R2,MINMKEY                                                       
         BAS   RE,FMTLIN                                                        
*                                                                               
         XC    SVSTELEM,SVSTELEM                                                
         XC    SVMSELEM,SVMSELEM                                                
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINFILTL,0                                                       
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         B     MLP50B                                                           
*                                                                               
MLP50    DS    0H                                                               
         GOTO1 VMINIO,DMCB,('MINSEQ',MINBLKD)                                   
*                                                                               
MLP50B   DS    0H                                                               
         CLI   MINERR,0            ALL ERRS AS EOR                              
         BNE   MLP60                                                            
         LA    R3,MELEM                                                         
         CLI   MELEM,BGRSTELQ      BG STATUS ELEM                               
         BE    MLP54                                                            
         CLI   MELEM,BGRMSELQ      MIS STATUS ELEM                              
         BE    MLP55                                                            
*                                                                               
         B     MLP50               NEXT ELEM                                    
*                                                                               
MLP54    DS    0H                  BG STATUS ELEM                               
         MVC   SVSTELEM,MELEM      SAVE IT                                      
         B     MLP50               NEXT ELEM                                    
*                                                                               
MLP55    DS    0H                  MIS STATUS ELEM                              
         MVC   SVMSELEM,MELEM      SAVE IT                                      
         B     MLP50               NEXT ELEM                                    
*                                                                               
MLP60    DS    0H                  IS RECORD OK?                                
         CLI   QOPT1,C'A'          DO ALL? (TESTING)                            
         BE    MLP62                                                            
*                                                                               
         LA    R3,SVSTELEM                                                      
         USING BGRSTELD,R3                                                      
         CLI   BGRSAST,C'A'                                                     
         BNE   MLP72                                                            
         CLC   BGRSADAT,=C'971216' DO ONLY RECENT ONES                          
         BL    MLP72                                                            
         CLC   BGRSAPER(7),=C'AUTO-BG'                                          
         BNE   MLP72                                                            
*                                                                               
         B     MLP72               **NO-OP MIS STATUS CHECK**                   
*                                                                               
         LA    R3,SVMSELEM                                                      
         USING BGRMSELD,R3                                                      
         CLI   BGRMSELD,0          IF NO MIS STATUS                             
         BE    MLP62               TREAT AS ERROR                               
         CLI   BGRMSTAT,0          OK IF STATUS = 00                            
         BE    MLP72                                                            
*                                                                               
MLP62    DS    0H                                                               
         CLI   SVSTELEM,0          IF HAD STATUS ELEM                           
         BE    MLP64                                                            
         XC    MINEKEY,MINEKEY     FIX IT                                       
         MVI   MINEKEY,BGRSTELQ                                                 
         MVI   MINFILTL,1                                                       
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MELEM                                                         
         USING BGRSTELD,R3                                                      
         MVI   BGRSAST,C'U'        SET TO UNEXAMINED                            
         GOTO1 VMINIO,DMCB,('MINWRT',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MLP64    DS    0H                                                               
         BAS   RE,FMTLIN                                                        
*                                                                               
         MVC   REQCRD+QAREA-QCLT(3),LINCLT                                      
         MVC   REQCRD+QAREA-QPRD(3),LINPRD                                      
         MVC   REQCRD+QAREA-QEST(3),LINEST                                      
         MVC   REQCRD+QAREA-QMKT(4),LINMKT                                      
*                                                                               
         LA    R2,BGREQF                                                        
         PUT   (R2),REQCRD                                                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
MLP72    DS    0H                  NEXT MINIO SET                               
         MVC   XKEY,SVKEY          RESTORE KEY FOR SEQ                          
         LA    R2,XKEY                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     MLP10                                                            
*                                                                               
MLP80    DS    0H                                                               
*                                                                               
MLPX     DS    0H                                                               
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
*                                                                               
         LA    R2,BGREQF                                                        
         OPEN  ((R2),OUTPUT)                                                    
*                                                                               
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
         MVI   MINFKLEN,L'BGRKEY     KEY LENGTH                                 
         MVI   MINEKLEN,L'BGRKMINK   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'BGRKMAST   DISPLACEMENT TO ELEMENT KEY                
         MVI   MINNCTL,L'BGRDSTAT    NUMBER OF CONTROL BYTES                    
         MVC   MINFRCLM,=H'3950'     MAXIMUM RECORD LENGTH                      
*                                                                               
         L     RF,=A(MBUFF1)         POINT TO START OF MINIO BUFFERS            
         A     RF,RELO                                                          
         ST    RF,MINBUFF            A(FIRST BUFFER)                            
*                                                                               
         MVI   MINNBUF,2             TWO BUFFERS                                
*                                                                               
         LA    RF,MRTAB                                                         
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
         MVI   MINDELSW,C'Y'       PROCESS DELETED RECORDS                      
         MVC   MINWRITE,RCWRITE    ENABLE/DISABLE WRITES                        
         MVI   MINRDUP,C'Y'        DO READ FOR UPDATE                           
*                                                                               
INITX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        FMTLIN - FORMAT LINE                                                   
***********************************************************************         
FMTLIN   NTR1                                                                   
         MVC   LINMED,QMED                                                      
         MVC   LINCLT,CLT                                                       
*                                                                               
         L     RF,ADCLT                                                         
         LA    R0,L'CLIST/4                                                     
         LA    RF,CLIST-CLTHDR(RF)                                              
*                                                                               
FL3      DS    0H                                                               
         CLC   BGRKPRD,3(RF)                                                    
         BE    FL4                                                              
         LA    RF,4(RF)                                                         
         BCT   R0,FL3                                                           
*                                                                               
FL4      DS    0H                                                               
         MVC   PRD,0(RF)                                                        
         MVC   LINPRD,0(RF)                                                     
*                                                                               
         EDIT  (B1,BGRKEST),EST,FILL=0                                          
         MVC   LINEST,EST                                                       
*                                                                               
         GOTO1 MSUNPK,DMCB,BGRKMKT,LINMKT,WORK                                  
         CLI   QOPT2,C'T'                                                       
         BNE   FL5                                                              
         GOTO1 HEXOUT,DMCB,BGRDDA,LINDISK,4,=C'N'                               
*                                                                               
FL5      DS    0H                                                               
         LA    R4,LINELEM                                                       
         LA    R3,SVSTELEM                                                      
         USING BGRSTELD,R3                                                      
         MVC   00(01,R4),BGRSAST                                                
         MVC   02(06,R4),BGRSADAT                                               
         MVC   09(07,R4),BGRSAPER                                               
         CLI   QOPT2,C'T'                                                       
         BNE   FLX                                                              
         LA    R3,SVMSELEM                                                      
         USING BGRMSELD,R3                                                      
         CLI   BGRMSELD,0          IF NO MIS STATUS                             
         BE    *+8                 TREAT AS ERROR                               
         MVI   22(R4),C'Y'                                                      
         GOTO1 HEXOUT,DMCB,BGRMSTAT,24(R4),1,=C'N'                              
*                                                                               
FLX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*        HDRHOOK - HEADHOOK                                                     
***********************************************************************         
HDRHOOK  NTR1                                                                   
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
BGREQF   DCB   DDNAME=BGREQF,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=80,                                               X        
               BLKSIZE=2000,                                           X        
               MACRF=PM                                                         
*                                                                               
MINBLK   DS    XL(MINBLKL)                                                      
MRTAB    DS    XL(200*(6+L'BGRKMAST))                                           
MBUFF1   DS    XL4000                                                           
MBUFF2   DS    XL4000                                                           
*        SPACE 2                                                                
WORKD    DSECT                                                                  
XKEY     DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
SVKEY    DS    XL(L'XKEY)                                                       
*                                                                               
FRSTSW   DS    XL1                                                              
EMOS     DS    CL4                                                              
MELEM    DS    XL256                                                            
SVELEM   DS    XL256                                                            
REQCRD   DS    CL80                                                             
*                                                                               
SVSTELEM DS    XL(BGRSTLNQ)                                                     
SVMSELEM DS    XL(BGRMSLNQ)                                                     
*                                                                               
VMINIO   DS    A                                                                
*                                                                               
         DS    0F                                                               
         SPACE 2                                                                
LIND     DSECT                     DSECT FOR PRINT LINE                         
LINMED   DS    CL1                                                              
         DS    CL1                                                              
LINCLT   DS    CL3                                                              
         DS    CL1                                                              
LINPRD   DS    CL3                                                              
         DS    CL1                                                              
LINEST   DS    CL3                                                              
         DS    CL1                                                              
LINMKT   DS    CL4                                                              
         DS    CL1                                                              
LINDISK  DS    CL8                                                              
         DS    CL4                                                              
LINELEM  DS    CL1                                                              
         SPACE 2                                                                
PAYTBD   DSECT                     DSECT FOR PAY TABLE                          
PTBCLT   DS    CL3                                                              
PTBPRD   DS    CL3                                                              
PTBEST   DS    CL3                                                              
PTBSTA   DS    CL5                                                              
PAYTBDKL EQU   *-PAYTBD                                                         
PTBMON1  DS    CL1                                                              
PTBMON2  DS    CL1                                                              
PAYTBDL  EQU   *-PAYTBD                                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENBGR                                                       
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPFXGP905/01/02'                                      
         END                                                                    
