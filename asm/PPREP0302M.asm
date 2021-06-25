*          DATA SET PPREP0302M AT LEVEL 003 AS OF 09/06/13                      
*PHASE PP0302M                                                                  
*                                                                               
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SET PRISMA FLAG IN ESTIMATE RECORD !                                          
*                                                                               
*        HONORS QCLIENT,QPRODUCT,QEST                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ        BECAUSE RCRQONLY IS SET                      
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RUNF     DS    0H                                                               
         ZAP   TOTALCNT,=P'0'      TOTAL NUMBER OF INSERTION                    
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   FCRDCLOS,C'Y'       PASS BACK CLOSE OUTS                         
         MVI   RCRQONLY,C'Y'       ONLY READ REQUESTS!                          
*                                                                               
         XC    BEST,BEST           INIT BINARY ESTIMATE                         
*                                                                               
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROC     DS    0H                  REQUEST FIRST                                
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AS ESTIMATE KEY                    
         USING PESTKEY,R4                                                       
*                                                                               
         MVC   PESTKAGY,QAGENCY    AGENCY                                       
         MVC   PESTKMED,QMEDIA     MEDIA                                        
         MVI   PESTKRCD,X'07'      RECORD TYPE                                  
*                                                                               
         MVC   PESTKCLT,QCLIENT    CLIENT                                       
*                                                                               
         CLC   QPRODUCT,SPACES     IF PRODUCT GIVEN                             
         BNH   PROC0A                                                           
*                                                                               
         MVC   PESTKPRD,QPRODUCT      USE IT                                    
*                                                                               
         CLC   QEST,SPACES         IF ESTIMATE GIVEN                            
         BNH   PROC0A                                                           
*                                                                               
         PACK  DUB,QEST            CONVERT TO BINARY                            
         CVB   RF,DUB                                                           
         STCM  RF,3,BEST                                                        
*                                                                               
         MVC   PESTKEST,BEST          USE IT                                    
*                                                                               
         B     PROC0A                                                           
*                                                                               
PROC0A   DS    0H                                                               
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    DS    0H                                                               
*                                                                               
         CLC   PESTKEY(PESTKPRD-PESTKEY),KEYSAVE                                
         BNE   PROCX               DONE IF NEW CLIENT                           
*                                                                               
         CLC   QPRODUCT,SPACES     IF PRODUCT GIVEN                             
         BNH   PROC6                                                            
         CLC   PESTKPRD,QPRODUCT      DONE IF NEW PRODUCT                       
         BNE   PROCX                                                            
*                                                                               
         CLC   QEST,SPACES         IF ESTIMATE GIVEN                            
         BNH   PROC6                                                            
         CLC   PESTKEST,BEST          DONE IF NEW ESTIMATE                      
         BNE   PROCX                                                            
*                                                                               
PROC6    DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 GETEST                                                           
         AP    TOTALCNT,=P'1'      TOTAL RECORDS COUNTER                        
*                                                                               
         LA    R6,PESTELEM                                                      
         CLI   0(R6),X'07'         CHECK CODE OF FIRST ELEM                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    PESTTEST,X'20'      TEST PRISMA BIT ON                           
         BZ    PROC10                                                           
         MVC   P+27(20),=CL20'PRISMA SET ALREADY'                               
         BAS   RE,PRTIT                                                         
         B     PROC3                                                            
*                                                                               
PROC10   OI    PESTTEST,X'20'      SET PRISMA BIT                               
         MVC   P+27(20),=CL20'SETTING PRISMA'                                   
         BAS   RE,PRTIT                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'PRTFILE',KEY+27,PESTREC,     X        
               DMWORK                                                           
         B     PROC3                                                            
*                                                                               
PROCX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
PRTIT    NTR1                                                                   
         MVC   P(2),PESTKAGY                                                    
         MVC   P+3(1),PESTKMED                                                  
         MVC   P+5(3),PESTKCLT                                                  
         MVC   P+9(3),PESTKPRD                                                  
         SR    R0,R0                                                            
         ICM   R0,3,PESTKEST                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB                                                      
         LA    R5,KEY+27                                                        
         LA    R0,4                                                             
         GOTO1 HEXOUT,DMCB,(R5),P+17,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         GOTO1 REPORT                                                           
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNL90                                                           
         MVC   P+1(25),8(R4)                                                    
         OI    7(R4),X'0F'                                                      
         UNPK  P+26(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,L'COUNTS(R4)                                                  
         B     RUNL50                                                           
*                                                                               
RUNL90   DS    0H                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RPRT     NTR1                                                                   
         MVI   RCSUBPRG,0                                                       
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
DMPREC   NTR1                                                                   
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
*                                                                               
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CHI   R4,32                                                            
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
OUT      DCB   DDNAME=TAPEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
COUNTS   DS    0CL33                                                            
TOTALCNT DC    PL8'0'                                                           
         DC    CL25'ESTIMATES PROCESSED'                                        
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
BEST     DS    XL2                 BINARY ESTIMATE NUMBER                       
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
BADOPTSW DS    CL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREP0302M09/06/13'                                      
         END                                                                    
