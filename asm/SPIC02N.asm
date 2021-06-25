*          DATA SET SPIC02N    AT LEVEL 006 AS OF 05/01/02                      
*PHASE SPIC02A                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SPIC02 - CHECK INVOICE MATCH STATUS RECS'                       
         SPACE 2                                                                
***********************************************************************         
*        QOPT1  X=EXCLUDE PRD=POL AND EST=000                                   
*        QOPT2  Y=LIST STATUS RECS                                              
*        QOPT3  Y=LIST INVOICE RECORDS                                          
*        QOPT4  E=TABLE ENTRY ONLY IF ERROR                                     
*        QOPT5  Y=GENERATE I2 REQUESTS                                          
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
         MVC   NISW,QOPT5                                                       
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
         L     R1,=A(SCTAB)                                                     
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,SCTABDL                                                       
         LA    R4,SCTABKL                                                       
         LH    R5,=Y(SCTMAX)                                                    
         STM   R0,R5,BSPARS                                                     
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
MLP70    DS    0H                                                               
         LA    R2,MINMKEY                                                       
         CLI   QOPT2,C'Y'          TEST TO LIST STATUS RECS                     
         BNE   MLP71                                                            
         BAS   RE,FMTMST                                                        
         GOTO1 REPORT                                                           
*                                                                               
MLP71    DS    0H                                                               
         MVC   BPRD,MSRKPRD                                                     
         MVC   BEST,MSRKEST                                                     
*                                                                               
         LA    R3,SVHDREL          HDR ELEM                                     
         USING MSRSTELD,R3                                                      
*                                                                               
         XC    BSWORK,BSWORK                                                    
         LA    R4,BSWORK                                                        
         USING SCTABD,R4                                                        
         MVC   SCTPRD,MSRKPRD                                                   
         MVC   SCTEST,MSRKEST                                                   
         MVC   SCTSTA,MSRKSTA                                                   
         MVC   SCTMKT,MSRKMKT                                                   
         MVC   SCTMOS,MSRKMOS                                                   
         MVC   SCTMSPTS,MSRSTINS     SPOTS                                      
         MVC   SCTMGRS,MSRSTING      GROSS                                      
         MVC   SCTMDA,MSRDDA         DISK ADDRESS                               
         GOTO1 BINSRCH,BSPARS,(1,BSWORK)                                        
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   0(R1),1             ALREADY THERE?                               
         BE    MLP72               NO, DONE                                     
*                                                                               
         ICM   RE,15,SCTMGRS-SCTABD(RF)   YES, ADD TO IT                        
         ICM   R0,15,SCTMGRS                                                    
         AR    R0,RE                                                            
         STCM  R0,15,SCTMGRS-SCTABD(RF)                                         
         ICM   RE,15,SCTMSPTS-SCTABD(RF)                                        
         ICM   R0,15,SCTMSPTS                                                   
         AR    R0,RE                                                            
         STCM  R0,15,SCTMSPTS-SCTABD(RF)                                        
*                                                                               
MLP72    DS    0H                                                               
         DROP  R4                                                               
         DROP  R3                                                               
*                                                                               
MLP77    DS    0H                                                               
*                                                                               
MLP78    DS    0H                                                               
         MVC   XKEY,SVKEY          RESTORE KEY FOR SEQ                          
         MVC   XKEYSAVE,SVKEYSAV                                                
         LA    R2,XKEY                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     MLP10                                                            
*                                                                               
MLP80    DS    0H                                                               
         EJECT                                                                  
**********************************************************************          
*        INVOICE READ                                                           
**********************************************************************          
*                                                                               
         CLI   QOPT3,C'Y'          ARE WE LISTING THEM?                         
         BNE   IRP71                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(12),=C'**INVOICES**'                                           
         MVI   RPTSW,C'I'                                                       
         GOTO1 REPORT                                                           
*                                                                               
IRP71    DS    0H                                                               
         XC    XKEY,XKEY                                                        
         USING SNVKEYD,R2                                                       
         LA    R2,XKEY                                                          
         MVC   SNVKTYPE(2),=X'0E03'                                             
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
*                                                                               
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
*                                                                               
IRP71B   DS    0H                                                               
         CLC   XKEY(5),XKEYSAVE                                                 
         BNE   IRP80                                                            
*                                                                               
         MVC   HALF,SNVKMOS        CHECK RIGHT MOS                              
         XC    HALF,=X'FFFF'                                                    
         MVC   BMOS,HALF                                                        
         GOTO1 DATCON,DMCB,(2,HALF),DUB                                         
         MVC   EMOS,DUB                                                         
         CLI   QSTART,C' '                                                      
         BNH   *+14                                                             
         CLC   EMOS,QSTART                                                      
         BL    IRP75                                                            
         CLI   QEND,C' '                                                        
         BNH   *+14                                                             
         CLC   EMOS,QEND                                                        
         BH    IRP75                                                            
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',XKEY+36,AREC,DMWORK           
*                                                                               
         L     R6,AREC                                                          
         LA    R6,SNVELS-SNVKEYD(R6)                                            
*                                                                               
IRP72    DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    IRP75                                                            
         CLI   0(R6),X'10'         HEADER ELEM                                  
         BE    IRP73                                                            
         CLI   0(R6),X'40'         DETAIL ELEM                                  
         BE    IRP74                                                            
*                                                                               
IRP72B   DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     IRP72                                                            
*                                                                               
IRP73    DS    0H                                                               
         USING SNVHDELD,R6                                                      
*                                                                               
         XC    WORK,WORK         GET MARKET VIA STATION                         
         MVC   WORK+2(3),SNVKSTA                                                
         GOTO1 MSUNPK,DMCB,WORK,WORK+10,STA                                     
         CLI   STA+4,C' '                                                       
         BH    *+8                                                              
         MVI   STA+4,C'T'                                                       
*                                                                               
         MVI   SREC,C'0'           GET STATION REC                              
         MVC   SREC+1(L'SREC-1),SREC                                            
         LA    R3,SREC                                                          
         USING STARECD,R3                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,STA                                                     
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT,CLT                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',SREC,SREC                    
         CLI   DMCB+8,0                                                         
         BE    *+10                                                             
         MVC   SMKT,=C'0000'     IF STATION NOT FOUND MKT=0                     
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,HLDMKT                                                        
         DROP  R3                                                               
         CLI   QOPT2,C'Y'          TEST TO LIST INVOICE RECS                    
         BNE   IRP73D                                                           
         BAS   RE,FMTINV                                                        
         GOTO1 REPORT                                                           
*                                                                               
IRP73D   DS    0H                                                               
         XC    BSWORK,BSWORK                                                    
         LA    R4,BSWORK                                                        
         USING SCTABD,R4                                                        
         MVC   SCTPRD,SNVHDPRD                                                  
         MVC   SCTEST,SNVHDEST                                                  
         MVC   SCTSTA,SNVKSTA                                                   
         MVC   SCTMKT,HLDMKT                                                    
         MVC   SCTMOS,SNVKMOS                                                   
         GOTO1 BINSRCH,BSPARS,(1,BSWORK)                                        
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         ST    RF,ASCTENT          A ENTRY                                      
         B     IRP72B              NEXT ELEM                                    
*                                                                               
IRP74    DS    0H                                                               
         USING SNVIDELD,R6                                                      
         L     RF,ASCTENT          A(SCTAB ENTRY)                               
         ICM   RE,15,SCTIGRS-SCTABD(RF)                                         
         ICM   R0,15,SNVIDCST                                                   
         AR    R0,RE                                                            
         STCM  R0,15,SCTIGRS-SCTABD(RF)                                         
         ICM   RE,15,SCTISPTS-SCTABD(RF)                                        
         LA    RE,1(RE)                                                         
         STCM  RE,15,SCTISPTS-SCTABD(RF)                                        
         B     IRP72B                                                           
*                                                                               
         DROP  R4                                                               
         DROP  R6                                                               
*                                                                               
IRP75    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
         B     IRP71B                                                           
*                                                                               
IRP80    DS    0H                                                               
         EJECT                                                                  
**********************************************************************          
*        PRINT REPORT                                                           
**********************************************************************          
*                                                                               
RPT      DS    0H                                                               
         LA    RF,LINERRS                                                       
         ST    RF,ANXTERR                                                       
         XC    LASTSTA(5),LASTSTA                                               
         MVI   RPTSW,C'T'                                                       
         CLC   QOPT2(2),=C'NN'     IF NOT LISTING STATS OR INVS                 
         BE    RPT02               NO NEW PAGE, ETC                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(09),=C'**TABLE**'                                              
         GOTO1 REPORT                                                           
*                                                                               
RPT02    DS    0H                                                               
         ICM   R3,15,BSPARS+8                                                   
         BZ    RPTX                                                             
         L     R4,BSPARS+4                                                      
         USING SCTABD,R4                                                        
*                                                                               
RPT04    DS    0H                                                               
         BAS   RE,FMTTAB                                                        
         CLI   STISTAT,C' '        ERROR?                                       
         BH    RPT06                                                            
         CLI   QOPT4,C'E'          NO, SKIP IF ONLY SHOWING ERRS                
         BE    RPT20                                                            
*                                                                               
RPT06    DS    0H                                                               
         CLC   STISTAT,=C'NI '     FOR NI'S                                     
         BNE   RPT06B                                                           
         CLI   NISW,C'D'           ARE WE DELETING THEM?                        
         BNE   RPT06B                                                           
         MVC   LINSTAT+3(2),=C'-D'                                              
*                                                                               
         OC    SCTMKT,SCTMKT       ANY MARKET?                                  
         BNZ   RPT06A                                                           
         L     RF,ANXTERR                                                       
         MVC   0(3,RF),=C'SNF'     NO, MEANS STATION NOT ON FILE                
         LA    RF,4(RF)                                                         
         ST    RF,ANXTERR                                                       
*                                                                               
RPT06A   DS    0H                                                               
         XC    MINMKEY,MINMKEY                                                  
         LA    R2,MINMKEY                                                       
         USING MSRKEY,R2                                                        
         MVC   MSRKTYPE(2),=X'0E04'                                             
         MVC   MSRKAM,BAGYMD                                                    
         MVC   MSRKCLT,BCLT                                                     
         MVC   MSRKPRD,SCTPRD                                                   
         MVC   MSRKEST,SCTEST                                                   
         MVC   MSRKMKT,SCTMKT                                                   
         MVC   MSRKSTA,SCTSTA                                                   
         MVC   MSRKMOS,SCTMOS                                                   
*                                                                               
         GOTO1 VMINIO,DMCB,('MINDLF',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    RPT06B                                                           
         L     RF,ANXTERR                                                       
         MVC   0(5,RF),=C'DLERR'                                                
         LA    RF,6(RF)                                                         
         ST    RF,ANXTERR                                                       
*                                                                               
RPT06B   DS    0H                                                               
         CLC   SCTSTA,LASTSTA         COMPARE STA TO LAST                       
         BNE   RPT07                                                            
         CLC   SCTMKT,LASTMKT         IF EQUAL AND MKT IS NOT                   
         BE    RPT08                                                            
         L     RF,ANXTERR          HAVE MKT ERROR                               
         MVC   0(4,RF),=C'*MKT'                                                 
         LA    RF,5(RF)                                                         
         ST    RF,ANXTERR                                                       
         B     RPT08                                                            
*                                                                               
RPT07    DS    0H                                                               
         MVC   LASTSTA(5),SCTSTA                                                
*                                                                               
RPT08    DS    0H                                                               
         GOTO1 REPORT                                                           
*                                  WRITE CKREQS                                 
         CLI   QOPT5,C'Y'          CKREQ OPTION                                 
         BNE   RPT20                                                            
         CLI   STISTAT,C' '         ERROR?                                      
         BNH   RPT20               NO, SKIP REQ                                 
         CLC   EST,=C'000'         SKIP EST=000                                 
         BE    RPT20                                                            
         CLC   PRD,=C'POL'         AND PRD=POL                                  
         BE    RPT20                                                            
         CLC   PRD,=C'ALL'         AND PRD=ALL                                  
         BE    RPT20                                                            
         CLI   NISW,C'D'           AND IF DELETING NI MSTATS                    
         BNE   RPT09                                                            
         CLC   STISTAT(3),=C'NI '                                               
         BE    RPT20                                                            
*                                                                               
RPT09    DS    0H                                                               
         MVC   CKRCARD(4),=C'I2CK'                                              
         MVC   CKRCARD+4(2),MED                                                 
         MVC   CKRCARD+5(3),CLT                                                 
         MVC   CKRCARD+11(3),PRD                                                
         MVC   CKRCARD+18(5),STA                                                
         MVC   CKRCARD+23(3),EST                                                
*                                                                               
         MVC   HALF,SCTMOS                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),WORK                                        
         MVC   CKRCARD+37(4),WORK                                               
*                                                                               
         MVC   CKRCARD+68(5),STISTAT                                            
         PUT   CKREQ,CKRCARD                                                    
*                                                                               
RPT20    DS    0H                                                               
         LA    R4,SCTABDL(R4)                                                   
         BCT   R3,RPT04                                                         
*                                                                               
RPTX     DS    0H                                                               
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
         MVC   LINMOS,EMOS         MONTH OF SERVICE                             
         GOTO1 HEXOUT,DMCB,36(R2),LINDA,4,=C'N'                                 
*                                                                               
FLX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        FMTINV - FORMAT INVOICE LINE                                           
***********************************************************************         
FMTINV   NTR1                                                                   
         USING SNVKEYD,R2                                                       
         USING SNVHDELD,R6                                                      
         MVC   LINMED,QMED                                                      
         MVC   LINCLT,CLT                                                       
*                                                                               
         LA    RF,=C'ALL'                                                       
         CLI   SNVHDPRD,0                                                       
         BE    FI4                                                              
*                                                                               
         L     RF,ADCLT                                                         
         LA    R0,L'CLIST/4                                                     
         LA    RF,CLIST-CLTHDR(RF)                                              
*                                                                               
FI3      DS    0H                                                               
         CLC   SNVHDPRD,3(RF)                                                   
         BE    FI4                                                              
         LA    RF,4(RF)                                                         
         BCT   R0,FI3                                                           
*                                                                               
FI4      DS    0H                                                               
         MVC   PRD,0(RF)                                                        
         MVC   LINPRD,PRD                                                       
*                                                                               
         EDIT  (B1,SNVHDEST),EST,FILL=0                                         
         MVC   LINEST,EST                                                       
*                                                                               
         GOTO1 MSUNPK,DMCB,SNVKSTA-2,WORK,STA                                   
         CLI   STA+4,C' '                                                       
         BH    *+8                                                              
         MVI   STA+4,C'T'                                                       
         MVC   LINSTA,STA                                                       
*                                                                               
         LH    R0,HLDMKT                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINMKT,DUB                                                       
*                                                                               
         MVC   HALF,SNVKMOS                                                     
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),WORK                                        
         MVC   LINMOS,WORK                                                      
         MVC   LININV,SNVKINV                                                   
         GOTO1 HEXOUT,DMCB,36(R2),LINDA,4,=C'N'                                 
*                                                                               
FIX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*        FMTTAB - FORMAT TABLE ENTRY                                            
***********************************************************************         
FMTTAB   NTR1                                                                   
         USING SCTABD,R4                                                        
         MVC   LINMED,QMED                                                      
         MVC   LINCLT,CLT                                                       
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
         XC    WORK,WORK                                                        
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
         EDIT  (B4,SCTMSPTS),(8,LININV+00),ZERO=NOBLANK                         
         EDIT  (B4,SCTMGRS),(9,LININV+09),2,ZERO=NOBLANK                        
         EDIT  (B4,SCTISPTS),(8,LININV+20),ZERO=NOBLANK                         
         EDIT  (B4,SCTIGRS),(9,LININV+29),2,ZERO=NOBLANK                        
*                                                                               
         CLC   SCTMSPTS(8),SCTISPTS                                             
         BE    FT20                                                             
         MVC   LINSTAT(3),=C'NI '                                               
         OC    SCTISPTS(8),SCTISPTS                                             
         BZ    FT20                                                             
         MVC   LINSTAT(3),=C'NM '                                               
         OC    SCTMSPTS(8),SCTMSPTS                                             
         BZ    FT20                                                             
         MVC   LINSTAT(3),=C'NEQ'                                               
*                                                                               
FT20     DS    0H                                                               
         MVC   STISTAT,LINSTAT                                                  
         OC    SCTMDA,SCTMDA                                                    
         BZ    FT5                                                              
         GOTO1 HEXOUT,DMCB,SCTMDA,LINDA,4,=C'N'                                 
*                                                                               
FT5      DS    0H                                                               
*                                                                               
FTX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
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
SVPAYEL  DS    XL(MSRIPLNQ)                                                     
SVHDREL  DS    XL(MSRSTLNQ)                                                     
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
LASTSTA  DS    XL3                                                              
LASTMKT  DS    XL2                                                              
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
LINMOS   DS    CL4                                                              
         DS    CL1                                                              
LINDA    DS    CL8                                                              
         DS    CL3                                                              
LINSTAT  DS    CL3                                                              
         DS    CL2                                                              
LINERRS  DS    CL10                                                             
         DS    CL1                                                              
LININV   DS    CL10                                                             
         DS    CL1                                                              
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
**PAN#1  DC    CL21'006SPIC02N   05/01/02'                                      
         END                                                                    
