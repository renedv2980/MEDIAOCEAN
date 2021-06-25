*          DATA SET SPREPIC02  AT LEVEL 022 AS OF 05/01/02                      
*PHASE SPIC02A                                                                  
*INCLUDE COVAIL                                                                 
*INCLUDE BINSRCH2                                                               
         TITLE 'SPIC02 - CHECK INVOICE MATCH STATUS RECS'                       
         SPACE 2                                                                
***********************************************************************         
*        QOPT1  X=EXCLUDE PRD=POL AND EST=000                                   
***********************************************************************         
         SPACE 2                                                                
SPIC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPIC02,CLEAR=YES,RR=R2                                         
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
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
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
*                                                                               
         SR    R0,R0               SET BSPARS                                   
         L     R1,ASCTAB                                                        
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,SCTABDL                                                       
         LA    R4,SCTABKL                                                       
         LH    R5,=Y(SCTMAX)                                                    
         STM   R0,R5,BSPARS                                                     
*                                                                               
         BAS   RE,INIT                                                          
         B     EXIT                                                             
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
* REQLAST                                                                       
*                                                                               
REQL     DS    0H                                                               
         BAS   RE,RPT              PRINT REPORT                                 
         B     EXIT                                                             
         EJECT                                                                  
* CLTFRST                                                                       
*                                                                               
MLP00    DS    0H                                                               
*                                                                               
**********************************************************************          
*        STATUS RECORD READ                                                     
**********************************************************************          
*                                                                               
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
MLP14    DS    0H                  CHECK MOS                                    
         MVC   HALF,MSRKMOS                                                     
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
         XC    SVICSEL,SVICSEL                                                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'13'       GET INCH STATUS ELEM                         
         MVI   MINFILTL,1                                                       
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0            ALL ERRS AS NOT FOUND                        
         BNE   *+10                                                             
         MVC   SVICSEL,MELEM                                                    
*                                                                               
         XC    SVPAYEL,SVPAYEL                                                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'14'       GET PAY STATUS ELEM                          
         MVI   MINFILTL,1                                                       
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0            ALL ERRS AS NOT FOUND                        
         BNE   MLP70                                                            
         MVC   SVPAYEL,MELEM                                                    
         LA    R3,SVPAYEL                                                       
         USING MSRIPELD,R3                                                      
         CLI   MSRIPST,C'Y'        IF NOT PAID, CLEAR DATE                      
         BE    *+10                (DON'T NEED TO WORRY ABOUT                   
         MVC   MSRIPDAT,SPACES     'NOT PAID' DATE)                             
*                                                                               
MLP70    DS    0H                                                               
*                                                                               
         CLI   QOPT2,C'T'          TRACE?                                       
         BNE   MLP70D                                                           
         GOTO1 HEXOUT,DMCB,MINMKEY,P,42,=C'N'                                   
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,SVICSEL,P+1,25,=C'N'                                 
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,SVPAYEL,P+1,25,=C'N'                                 
         GOTO1 REPORT                                                           
*                                                                               
MLP70D   DS    0H                                                               
         MVI   INCSTAT,C' '                                                     
         LA    R3,SVICSEL          INCH STATUS ELEM                             
         USING MSRICELD,R3                                                      
         MVC   INCSTAT,MSRICST                                                  
         OI    INCSTAT,C' '                                                     
         CLC   MSRICDAT,TODAY                                                   
         BNE   MLP72                                                            
         CLI   MSRICST,C'A'                                                     
         BNE   MLP71                                                            
         MVI   RPTNO,C'1'       APPROVED TODAY REPORT                           
         BAS   RE,ADDBIN                                                        
         B     MLP72                                                            
*                                                                               
MLP71    DS    0H                                                               
         CLI   MSRICST,C'R'                                                     
         BNE   MLP72                                                            
         MVI   RPTNO,C'2'       REJECTED TODAY REPORT                           
         BAS   RE,ADDBIN                                                        
*                                                                               
MLP72    DS    0H                                                               
         MVI   PAYSTAT,C' '                                                     
         LA    R3,SVPAYEL          PAID STATUS ELEM                             
         USING MSRIPELD,R3                                                      
         MVC   PAYSTAT,MSRIPST                                                  
         OI    PAYSTAT,C' '                                                     
         CLC   MSRIPDAT,TODAY                                                   
         BNE   MLP74                                                            
         CLI   MSRIPST,C'Y'                                                     
         BNE   MLP74                                                            
         MVI   RPTNO,C'3'       PAID TODAY REPORT                               
         BAS   RE,ADDBIN                                                        
*                                                                               
MLP74    DS    0H                                                               
         CLI   INCSTAT,C'A'       IF APPROVED                                   
         BNE   MLP75                                                            
         CLI   PAYSTAT,C'Y'       AND NOT PAID                                  
         BE    MLP75                                                            
         MVI   RPTNO,C'4'          DO APPROVED NOT PAID REPORT                  
         BAS   RE,ADDBIN                                                        
         B     MLP75                                                            
*                                                                               
MLP75    DS    0H                                                               
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
**********************************************************************          
*        ADDBIN - ADD TO BINSRCH TABLE                                          
*                 REPORT NUMBER IN RPTNO                                        
**********************************************************************          
*                                                                               
ADDBIN   NTR1                                                                   
         LA    R2,MINMKEY                                                       
         XC    BSWORK,BSWORK                                                    
         LA    R4,BSWORK                                                        
         USING SCTABD,R4                                                        
         MVC   SCTRPT,RPTNO                                                     
         MVC   SCTCLT,MSRKCLT                                                   
         MVC   SCTPRD,MSRKPRD                                                   
         MVC   SCTEST,MSRKEST                                                   
         MVC   SCTMKT,MSRKMKT                                                   
         MVC   SCTSTA,MSRKSTA                                                   
         MVC   SCTMOS,MSRKMOS                                                   
*                                                                               
         LA    R3,SVHDREL          HDR ELEM                                     
         USING MSRSTELD,R3                                                      
         MVC   SCTSPTS,MSRSTINS     SPOTS                                       
         MVC   SCTGRS,MSRSTING      GROSS                                       
*                                                                               
         LA    R3,SVPAYEL          PAY ELEM                                     
         USING MSRIPELD,R3                                                      
         MVC   SCTPDAT,MSRIPDAT     PAID DATE                                   
*                                                                               
         LA    R3,SVICSEL          INCH STATUS ELEM                             
         USING MSRICELD,R3                                                      
         MVC   SCTSDAT,MSRICDAT     STATUS DATE                                 
*                                                                               
         GOTO1 BINSRCH,BSPARS,(1,BSWORK)                                        
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   0(R1),1             TEST DUPE ENTRY                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADBX     DS    0H                                                               
         CLI   QOPT2,C'T'         TRACE?                                        
         BNE   ADBXX                                                            
         GOTO1 HEXOUT,DMCB,BSWORK,P+2,32,=C'N'                                  
         GOTO1 REPORT                                                           
*                                                                               
ADBXX    DS    0H                                                               
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
*        PRINT REPORT                                                           
**********************************************************************          
*                                                                               
RPT      NTR1                                                                   
         MVI   LASTRPT,0                                                        
         ICM   R3,15,BSPARS+8      TABLE COUNT                                  
         BZ    RPTX                                                             
         L     R4,BSPARS+4                                                      
         USING SCTABD,R4                                                        
*                                                                               
RPT04    DS    0H                                                               
         MVC   RPTNO,SCTRPT                                                     
         CLC   SCTRPT,LASTRPT      CHANGE OF REPORT TYPE?                       
         BE    RPT05                                                            
         MVI   FORCEHED,C'Y'       YES, NEW PAGE                                
         MVC   LASTRPT,SCTRPT                                                   
*                                                                               
RPT05    DS    0H                                                               
         BAS   RE,FMTTAB                                                        
         GOTO1 REPORT                                                           
*                                                                               
RPT07    DS    0H                                                               
         LA    R4,SCTABDL(R4)                                                   
         BCT   R3,RPT04                                                         
*                                                                               
RPTX     DS    0H                                                               
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
         MVI   MINDELSW,C'N'       DON'T PROCESS DELETED RECORDS                
         MVC   MINWRITE,RCWRITE    ENABLE/DISABLE WRITES                        
         MVI   MINRDUP,C'Y'        DO READ FOR UPDATE                           
*                                                                               
INITX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        FMTTAB - FORMAT TABLE ENTRY                                            
***********************************************************************         
FMTTAB   NTR1                                                                   
         USING SCTABD,R4                                                        
         MVC   LINMED,QMED                                                      
         GOTO1 CLUNPK,DMCB,SCTCLT,LINCLT                                        
*                                                                               
         LA    RF,=C'ALL'                                                       
         CLI   SCTPRD,0                                                         
         BE    FT4                                                              
*                                                                               
         L     RF,ADCLT                                                         
         LA    R0,L'CLIST/4                                                     
         LA    RF,CLIST-CLTHDR(RF)                                              
*                                                                               
FT3      DS    0H                                                               
         CLC   SCTPRD,3(RF)                                                     
         BE    FT4                                                              
         LA    RF,4(RF)                                                         
         BCT   R0,FT3                                                           
*                                                                               
FT4      DS    0H                                                               
         MVC   PRD,0(RF)                                                        
         MVC   LINPRD,PRD                                                       
*                                                                               
         EDIT  (B1,SCTEST),EST,FILL=0                                           
         MVC   LINEST,EST                                                       
*                                                                               
         MVC   WORK(2),SCTMKT                                                   
         MVC   WORK+2(3),SCTSTA                                                 
         GOTO1 MSUNPK,DMCB,WORK,LINMKT,STA                                      
         CLI   STA+4,C' '                                                       
         BH    *+8                                                              
         MVI   STA+4,C'T'                                                       
         MVC   LINSTA,STA                                                       
*                                                                               
         MVC   HALF,SCTMOS                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),(5,WORK)                                    
         MVC   LINMOS(3),WORK                                                   
         MVC   LINMOS+3(2),WORK+6                                               
*                                                                               
         CLI   SCTSDAT,C' '        ANY STATUS DATE?                             
         BNH   FT6                                                              
         GOTO1 DATCON,DMCB,(0,SCTSDAT),(5,LINSDAT)                              
*                                                                               
FT6      DS    0H                                                               
         CLI   SCTPDAT,C' '        ANY PAID DATE?                               
         BNH   FT7                                                              
         GOTO1 DATCON,DMCB,(0,SCTPDAT),(5,LINPDAT)                              
*                                                                               
FT7      DS    0H                                                               
         EDIT  (B4,SCTSPTS),(5,LINSPTS),ZERO=NOBLANK                            
         EDIT  (B4,SCTGRS),(12,LINGRS),2,ZERO=NOBLANK                           
*                                                                               
FT20     DS    0H                                                               
FTX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
*        HDRHOOK - HEADHOOK                                                     
***********************************************************************         
HDRHOOK  NTR1                                                                   
         LA    R7,HEAD6                                                         
         USING LIND,R7                                                          
*                                                                               
         MVC   132(32,R7),=C'MED CLT PRD EST MRKT STATN MONTH'                  
         MVC   264(32,R7),=C'--- --- --- --- ---- ----- -----'                  
         MVC   LINSDAT+000(17),=C'--ACTIVITY DATES-'                            
         MVC   LINSDAT+132(17),=C' STATUS    PAID  '                            
         MVC   LINSDAT+264(17),=C'-------- --------'                            
         MVC   LINSPTS+132(18),=C' SPOTS       GROSS'                           
         MVC   LINSPTS+264(18),=C' -----       -----'                           
*                                                                               
         CLI   RPTNO,C'1'          APPROVED TODAY REPORT                        
         BNE   HDH6                                                             
         MVC   HEAD4+3(18),=C'**APPROVED TODAY**'                               
         B     HDH30                                                            
*                                                                               
HDH6     DS    0H                                                               
         CLI   RPTNO,C'2'          REJECTED TODAY REPORT                        
         BNE   HDH6B                                                            
         MVC   HEAD4+3(18),=C'**REJECTED TODAY**'                               
         B     HDH30                                                            
*                                                                               
HDH6B    DS    0H                                                               
         CLI   RPTNO,C'3'          PAID TODAY REPORT                            
         BNE   HDH7                                                             
         MVC   HEAD4+3(14),=C'**PAID TODAY**'                                   
         B     HDH30                                                            
*                                                                               
HDH7     DS    0H                                                               
         CLI   RPTNO,C'4'          APPROVED BUT NOT PAID                        
         BNE   HDH8                                                             
         MVC   HEAD4+3(25),=C'**APPROVED BUT NOT PAID**'                        
         B     HDH30                                                            
*                                                                               
HDH8     DS    0H                                                               
HDH30    DS    0H                                                               
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
*                                                                               
*        SPACE 2                                                                
WORKD    DSECT                                                                  
XKEY     DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
SVKEY    DS    XL(L'XKEY)                                                       
SVKEYSAV DS    XL(L'XKEY)                                                       
*                                                                               
FRSTSW   DS    XL1                                                              
EMOS     DS    CL4                                                              
SVPAYEL  DS    XL(MSRIPLNQ)                                                     
SVICSEL  DS    XL(MSRICLNQ)                                                     
SVHDREL  DS    XL(MSRSTLNQ)                                                     
MELEM    DS    XL256                                                            
SVELEM   DS    XL256                                                            
VMINIO   DS    A                                                                
CKRCARD  DS    CL80                                                             
BSPARS   DS    6F                                                               
BSWORK   DS    XL(SCTABDL)                                                      
BMOS     DS    XL2                                                              
RPTSW    DS    XL1                                                              
STISTAT  DS    CL3                                                              
ASCTAB   DS    A                                                                
ASCTENT  DS    A                                                                
RPTNO    DS    CL1                                                              
INCSTAT  DS    CL1                                                              
PAYSTAT  DS    CL1                                                              
LASTRPT  DS    CL1                                                              
SREC     DS    XL150                                                            
*                                                                               
         SPACE 2                                                                
LIND     DSECT                     DSECT FOR PRINT LINE                         
         DS    CL1                                                              
LINMED   DS    CL1                                                              
         DS    CL2                                                              
LINCLT   DS    CL3                                                              
         DS    CL1                                                              
LINPRD   DS    CL3                                                              
         DS    CL1                                                              
LINEST   DS    CL3                                                              
         DS    CL1                                                              
LINMKT   DS    CL4                                                              
         DS    CL1                                                              
LINSTA   DS    CL5                                                              
         DS    CL1                                                              
LINMOS   DS    CL5                                                              
         DS    CL2                                                              
LINSDAT  DS    CL8                                                              
         DS    CL1                                                              
LINPDAT  DS    CL8                                                              
         DS    CL2                                                              
LINSPTS  DS    CL5                                                              
         DS    CL1                                                              
LINGRS   DS    CL12                                                             
         SPACE 2                                                                
SCTABD   DSECT                     DSECT FOR PAY TABLE                          
SCTRPT   DS    CL1                 'REPORT NUMBER'                              
SCTCLT   DS    XL2                                                              
SCTPRD   DS    XL1                                                              
SCTEST   DS    XL1                                                              
SCTMKT   DS    XL2                                                              
SCTSTA   DS    XL3                                                              
SCTMOS   DS    XL2                                                              
SCTABKL  EQU   *-SCTABD                                                         
SCTSPTS  DS    XL4                 STATUS REC SPOTS                             
SCTGRS   DS    XL4                 STATUS REC GROSS                             
SCTSDAT  DS    CL6                 MATCH STATUS DATE                            
SCTPDAT  DS    CL6                 PAID STATUS DATE                             
SCTABDL  EQU   *-SCTABD                                                         
*                                                                               
SCTMAX   EQU   5000                                                             
*                                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
       ++INCLUDE SPGENMSR                                                       
       ++INCLUDE SPGENSNV                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPREPIC02 05/01/02'                                      
         END                                                                    
