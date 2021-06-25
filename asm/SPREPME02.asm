*          DATA SET SPREPME02  AT LEVEL 144 AS OF 03/19/02                      
*&&      SET   DB=N                                                             
*PHASE SPME02                                                                   
*INCLUDE PRTREC                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE SCANNER                                                                
*                                                                               
* need to sort elements by date as well as daypart                              
* if dupes - there are - add them together                                      
*                                                                               
*                                                                               
*                                                                               
SPME02   TITLE 'SPME02 - CONVERT GOALS FROM TAPE FOR MCCANN'                    
         SPACE 1                                                                
SPME02   CSECT                                                                  
         DS    8192C                                                            
         ORG   *-8192                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPME02,R7                                                      
*                                                                               
         L     RC,ASPMEWRK                                                      
         USING SPMEWORK,RC                                                      
         USING AHRECD,HDRREC                                                    
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
         CLI   MODE,RUNFRST                                                     
         BE    FIRST                                                            
         B     EXITOK                                                           
*                                                                               
ASPMEWRK DC    A(SPMEWORK)                                                      
         EJECT                                                                  
***********************************************************************         
* ONE TIME PROCESSING                                                 *         
***********************************************************************         
         SPACE 1                                                                
FIRST    L     R4,VMASTC           LOAD DEMOVAL                                 
         USING MASTD,R4                                                         
         MVC   MCDUB,SPACES                                                     
         MVC   MCDUB(6),=C'T00AD9'                                              
         GOTO1 MCVLOADM,DMCB,0                                                  
         MVC   VDEMOVAL,4(R1)                                                   
         DROP  R4                                                               
*                                                                               
         OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FILEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (CPEFILE,INPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (MKTFILE,INPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* BUILD CPE LOOKUP TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
BLDCP02  LA    R2,RECIN                                                         
         GET   CPEFILE,0(R2)                                                    
         GOTO1 BINSRCH,BINPAR1,(X'01',RECIN)                                    
         ICM   RF,15,0(R1)                                                      
         BNZ   BLDCP02                                                          
         DC    H'0'                INCREASE TABLE SIZE                          
*                                                                               
BLDCP04  DS    0H                                                               
         CLOSE CPEFILE                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD MKT LOOKUP TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
BLDMK02  LA    R2,RECIN                                                         
         GET   MKTFILE,0(R2)                                                    
         XC    RECIN+1(4),RECIN+5  SWAP DDS AND ADWARE                          
         XC    RECIN+5(4),RECIN+1                                               
         XC    RECIN+1(4),RECIN+5                                               
         GOTO1 BINSRCH,BMKPAR1,(X'01',RECIN)                                    
         ICM   RF,15,0(R1)                                                      
         BNZ   BLDMK02                                                          
         DC    H'0'                INCREASE TABLE SIZE                          
*                                                                               
BLDMK04  DS    0H                                                               
         CLOSE MKTFILE                                                          
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONVERSION ROUTINE                                             *         
***********************************************************************         
         SPACE 1                                                                
MAIN     MVI   ENDSW,C'F'          SET BEFORE FIRST HEADER                      
*                                                                               
MAIN02   GET   FILEIN,RECIN                                                     
         CLC   HDR,RECIN           HEADER OBJECT?                               
         BE    MAIN04              YES                                          
         CLI   ENDSW,C'G'          FORCE SKIP TO NEXT HEADER?                   
         BE    MAIN02              YES                                          
         CLI   ENDSW,C'F'          BEFORE FIRST HEADER?                         
         BE    *+14                YES                                          
         CLC   DTL,RECIN           DETAIL RECORD?                               
         BE    MAIN08              YES                                          
*                                                                               
         MVC   P(L'ERRUNX),ERRUNX                                               
         LHI   RF,53               PRINT THIS BAD OBJECT                        
         CHI   RF,64                                                            
         BNH   *+8                                                              
         LHI   RF,64                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   P+L'ERRUNX(0),RECIN                                              
         GOTO1 REPORT                                                           
         B     MAIN02                                                           
*                                                                               
MAIN04   CLI   ENDSW,C'F'          BEFORE FIRST HEADER?                         
         BE    MAIN06              YES - SAVE IT AND GET DTL RECORDS            
         CLI   ENDSW,C'G'          FORCE GOTO NEXT HEADER?                      
         BE    MAIN06              YES - SAVE IT AND GET DTL RECORDS            
*                                                                               
         CLC   HDRREC,RECIN        THIS HEADER IS SAME AS LAST?                 
         BNE   MAIN10              NO                                           
*&&DB*&& MVC   P+00(08),=CL8'SAMEHDR '                                          
*&&DB*&& MVC   P+10(AHRECLQ),RECIN                                              
*&&DB*&& GOTO1 REPORT                                                           
         B     MAIN02              IGNORE HEADER AND JUST STACK DETAIL          
*                                                                               
MAIN06   MVI   ENDSW,C'N'          SET NORMAL PROCESSING                        
*&&DB*&& MVC   P+00(08),=CL8'VALHDR  '                                          
*&&DB*&& MVC   P+10(AHRECLQ),RECIN                                              
*&&DB*&& GOTO1 REPORT                                                           
*                                                                               
         MVC   HDRREC,RECIN        SAVE HEADER OBJECT                           
         BRAS  RE,PVHDR            PREVALIDATE HEADER OBJECT                    
         BE    *+12                                                             
         MVI   ENDSW,C'G'          SKIP TO NEXT HEADER                          
         B     MAIN02                                                           
*                                                                               
         LA    R2,DTLRECS          R2 = A(DETAIL BLOCK)                         
         LR    R0,R2                                                            
         LHI   R1,DTLDLQ*MAXDTLS                                                
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR DETAIL BLOCK                           
         B     MAIN02                                                           
*                                                                               
MAIN08   DS    0H                                                               
*&&DB*&& MVC   P+00(08),=CL8'VALDTL  '                                          
*&&DB*&& MVC   P+10(ADRECLQ),RECIN                                              
*&&DB*&& GOTO1 REPORT                                                           
*                                                                               
         BRAS  RE,PVDTL            PREVALIDATE DETAIL RECORD                    
         BH    MAIN02              DISCARD IT                                   
         BE    *+12                                                             
         MVI   ENDSW,C'G'          SKIP TO NEXT HEADER                          
         B     MAIN02                                                           
*                                                                               
         AHI   R2,DTLDLQ           NEXT IN TABLE                                
         B     MAIN02                                                           
*                                                                               
MAIN10   BRAS  RE,MAKEGOAL         ADD GOAL RECORDS TO FILE                     
         CLI   ENDSW,C'Y'                                                       
         BNE   MAIN06              PROCESS HDR RECORD IN RECIN                  
         B     ENDREQ                                                           
*                                                                               
MAIN12   GET   FILEIN,RECIN                                                     
         CLC   HDR,RECIN           LOOK FOR FIRST HEADER OBJECT                 
         BE    MAIN04                                                           
         B     MAIN12                                                           
*                                                                               
EOFTIN   CLOSE FILEIN              END OF INPUT TAPE                            
         CLI   ENDSW,C'N'          NORMAL PROCESSING?                           
         BNE   ENDREQ              NO                                           
         MVI   ENDSW,C'Y'          SET END OF TAPE                              
         B     MAIN10                                                           
*                                                                               
ENDREQ   CLOSE FILEOUT                                                          
*                                                                               
         OPEN  (BADCPES,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,=A(BADCPE)       REPORT ON NON-MATCHING CPE ENTRIES           
EREQ02   OC    0(L'BADCPE,R2),0(R2)                                             
         BZ    EREQ04                                                           
         LA    RF,WRK                                                           
         MVC   WRK,SPACES                                                       
         MVC   0(1,RF),0(R2)                                                    
         MVI   1(RF),C'/'                                                       
         AHI   RF,2                                                             
         MVC   0(4,RF),1(R2)                                                    
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(4,RF),5(R2)                                                    
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(4,RF),9(R2)                                                    
         PUT   BADCPES,WRK                                                      
         AHI   R2,13                                                            
         B     EREQ02                                                           
*                                                                               
EREQ04   CLOSE BADCPES                                                          
*                                                                               
EREQ06   L     R2,=A(BADMAP)                                                    
         OPEN  (BADMAPS,OUTPUT)     REPORT ON NON-MAPPING CPE ENTRIES           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EREQ08   OC    0(L'BADMAPS,R2),0(R2)                                            
         BZ    EREQ12                                                           
*                                                                               
         LA    R3,1(R2)                                                         
         USING CPED,R3                                                          
         LA    RF,WRK                                                           
         MVC   WRK,SPACES                                                       
         MVC   0(4,RF),=CL4'Bad '                                               
         MVC   4(8,RF),=CL8'Client'                                             
         CLI   0(R2),C'C'                                                       
         BE    EREQ10                                                           
         MVC   4(8,RF),=CL8'Product'                                            
         CLI   0(R2),C'P'                                                       
         BE    EREQ10                                                           
         MVC   4(8,RF),=CL8'Estimate'                                           
         CLI   0(R2),C'E'                                                       
         BE    EREQ10                                                           
         MVC   4(10,RF),=CL10'Brand Est'                                        
         CLI   0(R2),C'B'                                                       
         BE    EREQ10                                                           
         MVC   4(15,RF),=CL15'Estimate Period'                                  
         CLI   0(R2),C'W'                                                       
         BE    EREQ10                                                           
         MVC   4(10,RF),=CL10'Pool Est'                                         
         CLI   0(R2),C'X'                                                       
         BE    EREQ10                                                           
         MVC   4(15,RF),=CL15'Est Start Date'                                   
         CLI   0(R2),C'Y'                                                       
         BE    EREQ10                                                           
         MVC   4(15,RF),=CL15'Est End Date'                                     
         CLI   0(R2),C'Z'                                                       
         BE    EREQ10                                                           
         MVC   4(15,RF),=CL15'Demo In Est Hdr'                                  
         CLI   0(R2),C'V'                                                       
         BE    EREQ10                                                           
         MVC   4(15,RF),=CL15'Demo In Pol Hdr'                                  
         CLI   0(R2),C'D'                                                       
         BE    EREQ10                                                           
         MVC   4(12,RF),=CL12'????????????'                                     
*                                                                               
EREQ10   AHI   RF,20                                                            
         MVC   0(L'CPEMED,RF),CPEMED                                            
         MVI   L'CPEMED(RF),C'/'                                                
         AHI   RF,L'CPEMED+1                                                    
         MVC   0(L'CPECLT,RF),CPECLT                                            
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(L'CPEPRD,RF),CPEPRD                                            
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(L'CPEEST,RF),CPEEST                                            
*                                                                               
         LA    RF,WRK+40                                                        
         MVC   0(10,RF),=CL10'Mapped to '                                       
         LA    RF,WRK+52                                                        
         MVC   0(L'CPEDCLT,RF),CPEDCLT                                          
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(L'CPEDPRD,RF),CPEDPRD                                          
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(L'CPEDEST,RF),CPEDEST                                          
         AHI   RF,L'CPEDEST+4                                                   
*                                                                               
         OC    24(6,R2),24(R2)                                                  
         BZ    *+10                                                             
         MVC   0(6,RF),24(R2)                                                   
*                                                                               
         PUT   BADMAPS,WRK                                                      
         AHI   R2,L'BADMAP                                                      
         B     EREQ08                                                           
         DROP  R3                                                               
*                                                                               
EREQ12   CLOSE BADMAPS                                                          
*                                                                               
         L     R2,=A(BADMKT)                                                    
         OPEN  (BADMKTS,OUTPUT)    REPORT ON BAD ADWARE MARKET ENTRIES          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EREQ14   OC    0(L'BADMKT,R2),0(R2)                                             
         BZ    EREQ16                                                           
         MVC   WRK,SPACES                                                       
         MVC   WRK(1),0(R2)                                                     
         MVI   WRK+1,C'/'                                                       
         MVC   WRK+2(4),1(R2)                                                   
         PUT   BADMKTS,WRK                                                      
         AHI   R2,L'BADMKT                                                      
         B     EREQ14                                                           
*                                                                               
EREQ16   CLOSE BADMKTS                                                          
         GOTO1 AENDREQ                                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE HDR INPUT RECORD                                           *         
* NTRY  HDRREC = HEADER RECORD                                        *         
* EXIT: CC OK  = VALIDATED WITHOUT ERROR                              *         
***********************************************************************         
         SPACE 1                                                                
PVHDR    NTR1  ,                                                                
         MVC   QMED(76),SPACES     LEAVE QCODE/QAGY                             
         MVC   QMED,AHMED                                                       
         XC    WRK,WRK             GET CONVERSION VALUE                         
X        USING CPED,WRK                                                         
         MVC   X.CPEMED,AHMED                                                   
         MVC   X.CPECLT,AHCLT                                                   
         MVC   X.CPEPRD,AHPRD                                                   
         MVC   X.CPEEST,AHEST                                                   
         GOTO1 BINSRCH,BINPAR1,WRK                                              
         CLI   0(R1),0                                                          
         BE    PVH02                                                            
*                                                                               
         MVC   P(AHRECLQ),HDRREC                                                
         MVC   P2(L'ERRCPE),ERRCPE                                              
         LA    RF,P2+L'ERRCPE                                                   
         MVC   0(1,RF),AHMED                                                    
         MVI   1(RF),C'/'                                                       
         AHI   RF,2                                                             
         MVC   0(L'AHCLT,RF),AHCLT                                              
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(L'AHPRD,RF),AHPRD                                              
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(L'AHEST,RF),AHEST                                              
         BRAS  RE,RFSPACE                                                       
         GOTO1 REPORT                                                           
         GOTO1 BINSRCH,BHPAR1,(X'01',WRK)                                       
         B     EXITL                                                            
         DROP  X                                                                
*                                                                               
PVH02    L     RF,0(R1)            GET ADDRESS OF CPE DATA                      
         USING CPED,RF                                                          
         MVC   SVCPE,0(RF)         SAVE CPE ENTRY IN CASE OF ERROR              
         MVC   QCLT,CPEDCLT                                                     
         MVC   QPRD,CPEDPRD                                                     
         MVC   QEST,CPEDEST                                                     
         DROP  RF                                                               
*                                  MAKE SURE DATES IN Y2K FORMAT                
         GOTO1 DATCON,DMCB,AHSTFL,(3,DUB)                                       
         GOTO1 (RF),(R1),(3,DUB),QSTART                                         
*                                                                               
         GOTO1 (RF),(R1),AHENFL,(3,DUB)                                         
         GOTO1 (RF),(R1),(3,DUB),QEND                                           
*                                                                               
         NI    BAGYMD,X'F0'        DROP MEDIA CODE                              
         OI    BAGYMD,X'01'        SET FOR TV                                   
         CLI   QMED,C'T'                                                        
         BE    PVH04                                                            
         NI    BAGYMD,X'F0'        DROP MEDIA CODE                              
         OI    BAGYMD,X'02'        SET FOR RADIO                                
         CLI   QMED,C'R'                                                        
         BE    PVH04                                                            
         DC    H'0'                                                             
*                                                                               
PVH04    GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PVH06                                                            
         MVC   P(AHRECLQ),HDRREC                                                
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERRCLT),ERRCLT                                     
         MVC   P2+L'ERRINV+L'ERRCLT(L'AHCLT),AHCLT                              
         MVI   P2+L'ERRINV+L'ERRCLT+L'AHCLT,C'='                                
         MVC   P2+L'ERRINV+L'ERRCLT+L'AHCLT+1(L'QCLT),QCLT                      
         GOTO1 REPORT                                                           
         MVI   SVCPX,C'C'                                                       
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         B     EXITL                                                            
*                                                                               
PVH06    GOTO1 GETCLT              GET CLIENT HEADER RECORD                     
         L     R8,ADCLT                                                         
         USING CLTHDRD,R8                                                       
*                                                                               
         LA    R1,CLIST            VALIDATE PRODUCTS                            
PVH08    CLC   0(3,R1),QPRD                                                     
         BE    PVH10                                                            
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    PVH08                                                            
*                                                                               
         MVC   P(AHRECLQ),HDRREC                                                
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERRPRD),ERRPRD                                     
         MVC   P2+L'ERRINV+L'ERRPRD(L'AHPRD),AHPRD                              
         MVI   P2+L'ERRINV+L'ERRPRD+L'AHPRD,C'='                                
         MVC   P2+L'ERRINV+L'ERRPRD+L'AHPRD+1(L'QPRD),QPRD                      
         GOTO1 REPORT                                                           
         MVI   SVCPX,C'P'                                                       
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         B     EXITL                                                            
*                                                                               
PVH10    MVC   BPRD,3(R1)                                                       
*                                                                               
         LA    R4,QEST+2                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R5,QEST                                                          
         SR    R4,R5               GIVES LEN-1                                  
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,QEST(0)                                                      
         CVB   R0,DUB                                                           
         STC   R0,BEST                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
         LTR   R0,R0               VALID ESTIMATE                               
         BZ    PVH12                                                            
         CHI   R0,255              ONE BYTE ONLY                                
         BH    PVH12                                                            
         B     PVH14                                                            
*                                                                               
PVH12    MVC   P(AHRECLQ),HDRREC                                                
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERREST),ERREST                                     
         MVC   P2+L'ERRINV+L'ERREST(L'QEST),QEST                                
         GOTO1 REPORT                                                           
         MVI   SVCPX,C'E'                                                       
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         B     EXITL                                                            
*                                                                               
PVH14    XC    KEY,KEY                                                          
         L     R8,ADCLT                                                         
         MVC   KEY(4),0(R8)        A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),BEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PVH18                                                            
*                                                                               
         MVC   P(AHRECLQ),HDRREC                                                
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERRBEST),ERRBEST                                   
         MVC   P2+L'ERRINV+L'ERRBEST(L'AHEST),AHEST                             
         MVI   P2+L'ERRINV+L'ERRBEST+L'AHEST,C'='                               
         MVC   P2+L'ERRINV+L'ERRBEST+L'AHEST+1(L'QEST),QEST                     
         GOTO1 REPORT                                                           
         MVI   SVCPX,C'B'                                                       
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         B     EXITL                                                            
*                                                                               
PVH18    GOTO1 GETEST              MAKE SURE HDR DATES WITHIN EST DATES         
         MVC   SVSTART,AHSTFL      SAVE PERIOD START/END                        
         MVC   SVEND,AHENFL                                                     
*                                                                               
         L     R8,ADEST                                                         
         USING ESTHDRD,R8                                                       
         GOTO1 DATCON,DMCB,ESTART,(3,BESTART)                                   
         GOTO1 (RF),(R1),EEND,(3,BEEND)                                         
*                                                                               
         GOTO1 CASHVAL,DMCB,(C'N',AHSTFL),L'AHSTFL,0                            
         CLI   0(R1),0                                                          
         BE    PVH20                                                            
*                                                                               
         MVC   P(AHRECLQ),HDRREC                                                
         MVC   P2(L'ERRINV),ERRINV  PERIOD START IS NOT NUMERIC                 
         MVC   P2+L'ERRINV(L'ERRPERST),ERRPERST                                 
         MVC   P2+L'ERRINV+L'ERRPERST(L'AHSTFL),AHSTFL                          
         GOTO1 REPORT                                                           
         MVI   SVCPX,C'Z'                                                       
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         B     EXITL                                                            
*                                                                               
PVH20    GOTO1 CASHVAL,DMCB,(C'N',AHENFL),L'AHENFL,0                            
         CLI   0(R1),0                                                          
         BE    PVH22                                                            
         MVC   P(AHRECLQ),HDRREC                                                
         MVC   P2(L'ERRINV),ERRINV  PERIOD END IS NOT NUMERIC                   
         MVC   P2+L'ERRINV(L'ERRPEREN),ERRPEREN                                 
         MVC   P2+L'ERRINV+L'ERRPEREN(L'AHENFL),AHENFL                          
         GOTO1 REPORT                                                           
         MVI   SVCPX,C'Y'                                                       
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         B     EXITL                                                            
*                                                                               
PVH22    GOTO1 DATCON,DMCB,AHSTFL,(3,DUB)                                       
         GOTO1 (RF),(R1),AHENFL,(3,DUB+3)                                       
*                                                                               
         CLC   BESTART,DUB         EST START PRIOR TO HDR START                 
         BH    PVH24                                                            
         CLC   BEEND,DUB+3         EST END AFTER HDR END                        
         BL    PVH24                                                            
         B     PVH26                                                            
*                                                                               
PVH24    MVC   P(AHRECLQ),HDRREC                                                
         GOTO1 REPORT                                                           
         MVC   P(L'WRNNOIP),WRNNOIP  NOT IN ESTIMATE PERIOD                     
         LA    R2,P+L'WRNNOIP                                                   
         MVI   8(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,BESTART),(11,(R2)),0                              
         GOTO1 (RF),(R1),(3,BEEND),(11,9(R2)),0                                 
         GOTO1 REPORT                                                           
*??      MVI   SVCPX,C'W'                                                       
*??      GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
*??      B     EXITL                                                            
*                                                                               
PVH26    XC    DMCB(12),DMCB                                                    
         MVC   DMCB(3),QAGY        A-M                                          
         MVC   DMCB+3(1),EDAYMENU                                               
         L     RE,ADDPTTAB                                                      
         ST    RE,DMCB+4                                                        
         GOTO1 DPTRD,DMCB                                                       
         DROP  R8                                                               
*                                                                               
PVH28    XC    WRK,WRK             GET MARKET CONVERSION                        
         MVC   WRK+00(1),AHMED                                                  
         LA    R4,AHMKT+3                                                       
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R5,AHMKT                                                         
         SR    R4,R5               GIVES LEN-1                                  
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,AHMKT(0)                                                     
         OI    DUB+7,X'0F'                                                      
         UNPK  AHMKT,DUB                                                        
         MVC   WRK+01(4),AHMKT                                                  
         GOTO1 BINSRCH,BMKPAR1,WRK                                              
         CLI   0(R1),0                                                          
         BE    PVH30                                                            
*                                                                               
         MVC   P(AHRECLQ),HDRREC                                                
         GOTO1 REPORT                                                           
         MVC   P(L'ERRMKT),ERRMKT                                               
         LA    RF,P+L'ERRMKT                                                    
         MVC   0(L'AHMED,RF),AHMED                                              
         MVI   L'AHMED(RF),C'/'                                                 
         AHI   RF,L'AHMED+1                                                     
         MVC   0(L'AHMKT,RF),AHMKT                                              
         GOTO1 REPORT                                                           
         GOTO1 BINSRCH,BBMPAR1,(X'01',WRK)                                      
         B     EXITL                                                            
*                                                                               
PVH30    L     RF,0(R1)            GET ADDRESS OF MKT DATA                      
         MVC   QMKT,5(RF)          SET DDS MARKET                               
         LA    R4,QMKT+3                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R5,QMKT                                                          
         SR    R4,R5               GIVES LEN-1                                  
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,QMKT(0)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         B     PVH34                                                            
*                                                                               
PVH32    MVC   P(AHRECLQ),HDRREC                                                
         GOTO1 REPORT                                                           
         MVC   P(L'ERRINV),ERRINV                                               
         MVC   P+L'ERRINV(L'ERRMKTB),ERRMKTB                                    
         MVC   P+L'ERRINV+L'ERRMKTB(L'QMKT),QMKT                                
         GOTO1 REPORT                                                           
         B     EXITL                                                            
*                                                                               
PVH34    BRAS  RE,VALDEMO                                                       
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE DTL INPUT RECORD                                           *         
* NTRY: R2     = A(OUTPUT FOR DTLD)                                   *         
*       RECIN  = DETAIL RECORD                                        *         
*       HDRREC = HEADER RECORD                                        *         
* EXIT: CC OK  = VALIDATED WITHOUT ERROR                              *         
***********************************************************************         
         SPACE 1                                                                
         USING DTLD,R2                                                          
X        USING ADRECD,RECIN                                                     
PVDTL    NTR1  ,                                                                
*&&DB*&& MVC   P(ADRECLQ),X.ADRECD                                              
*&&DB*&& GOTO1 REPORT                                                           
*                                                                               
         L     RE,ADDPTTAB         DAYPART                                      
         LHI   RF,30                                                            
PVD02    CLC   X.ADDYPT,0(RE)                                                   
         BE    PVD04                                                            
         LA    RE,5(RE)                                                         
         BCT   RF,PVD02                                                         
*                                                                               
         MVC   P(AHRECLQ),HDRREC                                                
         MVC   P+AHRECLQ+1(ADRECLQ),X.ADRECD                                    
         GOTO1 REPORT                                                           
         MVC   P(L'ERRINV),ERRINV                                               
         MVC   P+L'ERRINV(L'ERRDPT),ERRDPT                                      
         MVC   P+L'ERRINV+L'ERRDPT(L'ADDYPT),X.ADDYPT                           
         GOTO1 REPORT                                                           
         B     EXITL                                                            
*                                                                               
PVD04    MVC   DTLDYPT,X.ADDYPT    SET DAYPART                                  
*                                                                               
         MVC   DUB(6),X.ADWOD                                                   
         CLC   X.ADWOD,AHSTFL      MAKE SURE WEEK IS IN FLIGHT DATES            
         BH    *+10                                                             
         MVC   DUB(6),AHSTFL       ELSE SET TO HDR START                        
*                                                                               
         GOTO1 DATCON,DMCB,DUB,(3,DOUBLE)                                       
         CLC   BESTART,DOUBLE      DTL PRIOR TO DTL START                       
         BH    PVD06                                                            
         CLC   BEEND,DOUBLE        OR AFTER EST END                             
         BL    PVD06                                                            
         B     PVD08                                                            
*                                                                               
PVD06    MVC   P(AHRECLQ),AHRECD                                                
         MVC   P+AHRECLQ+1(ADRECLQ),X.ADRECD                                    
         MVC   P2(L'ERRNOIP),ERRNOIP  NOT IN ESTIMATE PERIOD                    
         LA    R2,P2+L'ERRNOIP                                                  
         MVI   8(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,BESTART),(11,(R2)),0                              
         GOTO1 (RF),(R1),(3,BEEND),(11,9(R2)),0                                 
         MVC   20(10,R2),=CL10'***DISCARDED***'                                 
         GOTO1 REPORT                                                           
         MVI   SVCPX,C'W'                                                       
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         B     EXITH                                                            
*                                                                               
PVD08    GOTO1 DATCON,DMCB,DUB,(2,DTLWK),0                                      
         OC    DTLWK,DTLWK         WEEK-OF DATE IS OK?                          
         BNZ   PVD10               YES                                          
*                                                                               
         MVC   P(AHRECLQ),AHRECD                                                
         MVC   P+AHRECLQ+1(ADRECLQ),X.ADRECD                                    
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERRWOD),ERRWOD                                     
         MVC   P2+L'ERRINV+L'ERRWOD(L'ADWOD),X.ADWOD                            
         GOTO1 REPORT                                                           
         B     EXITL                                                            
*                                                                               
PVD10    GOTO1 CASHVAL,DMCB,(C'N',X.ADSLN),L'ADSLN,0                            
         CLI   0(R1),255                                                        
         BE    PVD12                                                            
         ICM   RF,15,4(R1)         SPOT LENGTH                                  
         BZ    PVD12                                                            
         CHI   RF,255              ONE BYTE ONLY                                
         BH    PVD12                                                            
         STC   RF,DTLSLN                                                        
         B     PVD14                                                            
*                                                                               
PVD12    MVC   P(AHRECLQ),AHRECD                                                
         MVC   P+AHRECLQ+1(ADRECLQ),X.ADRECD                                    
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERRSLN),ERRSLN                                     
         MVC   P2+L'ERRINV+L'ERRSLN(L'ADSLN),X.ADSLN                            
         GOTO1 REPORT                                                           
         B     EXITL                                                            
*                                                                               
PVD14    GOTO1 CASHVAL,DMCB,(C'N',X.ADGPS),L'ADGPS,0                            
         CLI   0(R1),255                                                        
         BE    PVD16                                                            
         ICM   R0,15,4(R1)                                                      
         C     R0,=F'9999990'        MAX IS 999999.0                            
         BH    PVD16                                                            
         STCM  R0,15,DTLGRP        GRPS PER SPOT                                
         B     PVD18                                                            
*                                                                               
PVD16    MVC   P(AHRECLQ),AHRECD                                                
         MVC   P+AHRECLQ+1(ADRECLQ),X.ADRECD                                    
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERRGPS),ERRGPS                                     
         MVC   P2+L'ERRINV+L'ERRGPS(L'ADGPS),X.ADGPS                            
         GOTO1 REPORT                                                           
         B     EXITL                                                            
*                                                                               
PVD18    GOTO1 CASHVAL,DMCB,(C'N',X.ADSCST),L'ADSCST,0                          
         CLI   0(R1),255                                                        
         BE    PVD20                                                            
         ICM   R0,15,4(R1)                                                      
         C     R0,=F'999999900'    MAX IS 9999999.00                            
         BH    PVD20                                                            
         STCM  R0,15,DTLCST        SPOT COST                                    
         B     PVD22                                                            
*                                                                               
PVD20    MVC   P(AHRECLQ),AHRECD                                                
         MVC   P+AHRECLQ+1(ADRECLQ),X.ADRECD                                    
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERRCST),ERRCST                                     
         MVC   P2+L'ERRINV+L'ERRCST(L'ADSCST),X.ADSCST                          
         GOTO1 REPORT                                                           
         B     EXITL                                                            
*                                                                               
PVD22    GOTO1 CASHVAL,DMCB,(C'N',X.ADCPP),L'ADCPP,0                            
         CLI   0(R1),255                                                        
         BE    PVD24                                                            
         ICM   R0,15,4(R1)                                                      
         STCM  R0,15,DTLCPP        COST PER POINT                               
         B     PVD26                                                            
*                                                                               
PVD24    MVC   P(AHRECLQ),AHRECD                                                
         MVC   P+AHRECLQ+1(ADRECLQ),X.ADRECD                                    
         MVC   P2(L'ERRINV),ERRINV                                              
         MVC   P2+L'ERRINV(L'ERRCPP),ERRCPP                                     
         MVC   P2+L'ERRINV+L'ERRCPP(L'ADCPP),X.ADCPP                            
         GOTO1 REPORT                                                           
         B     EXITL                                                            
*                                                                               
PVD26    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT OBJECTS AND GENERATE GOAL RECORDS                    *         
***********************************************************************         
         SPACE 1                                                                
MAKEGOAL NTR1  ,                                                                
         XC    SVDTL,SVDTL                                                      
         MVI   TOADD,C'N'                                                       
*                                                                               
         LA    R2,DTLRECS          COUNT NUMBER OF DETAIL RECORDS               
         USING DTLD,R2                                                          
         LHI   R0,MAXDTLS                                                       
         XR    R3,R3                                                            
MG02     OC    0(DTLDLQ,R2),0(R2)                                               
         BZ    MG04                                                             
         AHI   R3,1                                                             
         AHI   R2,DTLDLQ                                                        
         BCT   R0,MG02                                                          
         DC    H'0'                MAKE DTL TABLE BIGGER                        
*                                                                               
MG04     LA    R2,DTLRECS          BACK TO TOP OF DETAIL RECORDS                
         STH   R3,DTLCNT           SET COUNT OF DETAIL LINES                    
         CHI   R3,1                NEED TO SORT?                                
         BE    MG06                NO                                           
         GOTO1 XSORT,DMCB,(0,DTLRECS),(R3),DTLDLQ,2,0                           
*                                                                               
MG06     CLC   SVDTL,DTLDYPT       SAVED DAYPART CHANGED?                       
         BE    MG10                NO                                           
         CLI   TOADD,C'Y'                                                       
         BNE   MG08                                                             
*                                                                               
         MVI   0(R8),X'FE'                                                      
         MVI   1(R8),19                                                         
         MVC   2(17,R8),AHMED                                                   
         AHI   R8,19                                                            
*                                                                               
         L     R4,ADGOAL                                                        
         USING GOALREC,R4                                                       
         SR    R8,R4               SET CORRECT RECORD LENGTH                    
         STCM  R8,3,GLENGTH                                                     
*                                                                               
         BRAS  RE,WRTGOAL                                                       
*&&DB*&& BRAS  RE,PRTGOAL                                                       
         MVI   TOADD,C'N'                                                       
*                                                                               
MG08     L     R4,ADGOAL                                                        
         LR    R0,R4                                                            
         LHI   R1,4000                                                          
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR GOAL RECORD                            
*                                                                               
         MVI   GKEYTYPE,GKEYTYPQ   BUILD RECORD KEY                             
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GKEYCLT,BCLT                                                     
         MVC   GKEYPRD,BPRD                                                     
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,BEST                                                     
         MVC   GKEYDPT,DTLDYPT                                                  
         MVC   GKEYSLN,DTLSLN                                                   
         MVC   GKEYSEC,DTLSLN                                                   
         MVI   GKEYAGY,0                                                        
         MVC   GLENGTH,=AL2(GDELEM-GOALREC)                                     
*                                                                               
         MVI   GOCODE,GDCODEQ                                                   
         MVI   GOLEN,GDLENQ                                                     
         MVC   GREDATE,TODAYP                                                   
         MVC   GACTDATE,TODAYP                                                  
*                                                                               
         LA    R8,GLEMENT          R8 = A(LATEST ELEMENT)                       
X        USING GLEMENT,R8                                                       
*                                                                               
MG10     MVI   TOADD,C'Y'                                                       
         MVC   SVDTL,DTLDYPT       SAVE DAYPART                                 
*                                                                               
         MVI   X.GLCODE,GLCODEQ    BUILD GLEMENT FROM DTL LINE                  
         MVI   X.GLEN,GLEN1Q                                                    
         MVC   X.GLWEEK,DTLWK                                                   
         MVC   X.GLGRP,DTLGRP                                                   
         MVC   X.GLBUDGET,DTLCST                                                
*                                                                               
         AHI   R8,GLEN1Q           NEXT GLEMENT                                 
         MVI   0(R8),0                                                          
         AHI   R2,DTLDLQ           NEXT DTL LINE                                
         BCT   R3,MG06                                                          
*                                                                               
         MVI   0(R8),X'FE'                                                      
         MVI   1(R8),19                                                         
         MVC   2(17,R8),AHMED                                                   
         AHI   R8,19                                                            
*                                                                               
         SR    R8,R4               SET CORRECT RECORD LENGTH                    
         STCM  R8,3,GLENGTH                                                     
         BRAS  RE,WRTGOAL                                                       
*&&DB*&& BRAS  RE,PRTGOAL                                                       
         B     EXITOK                                                           
         DROP  R4,X                                                             
         EJECT                                                                  
***********************************************************************         
* VALDEMO - VALIDATE DEMO VALUE                                       *         
***********************************************************************         
         SPACE 1                                                                
VALDEMO  NTR1  ,                                                                
         XC    WRK,WRK             BUILD DUMMY TWA FIELD FOR DEMOVAL            
         MVI   WRK+0,L'AHPDEMO+8                                                
         MVI   WRK+5,L'AHPDEMO                                                  
         MVC   WRK+8(L'AHPDEMO),AHPDEMO                                         
         CLC   =C'RP',WRK+8                                                     
         BNE   *+10                                                             
         MVC   WRK+8(2),=C'RV'                                                  
*                                                                               
         L     R1,ADBLOCK                                                       
         USING DBLOCKD,R1                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'       ASSUMES NOT CANADA                           
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         DROP  R1                                                               
*                                                                               
         XC    SVDEMCDS,SVDEMCDS                                                
         GOTO1 VDEMOVAL,DMCB,(1,WRK),(14,SVDEMCDS),(C'S',ADBLOCK),0             
         XR    RE,RE                                                            
         ICM   RE,1,4(R1)                                                       
         BNZ   VDEM02                                                           
*                                                                               
         MVC   P(AHRECLQ),HDRREC                                                
         GOTO1 REPORT                                                           
         MVC   P(L'ERRINV),ERRINV                                               
         MVC   P+L'ERRINV(L'ERRDEMO),ERRDEMO                                    
         MVC   P+L'ERRINV+L'ERRDEMO(L'AHPDEMO),AHPDEMO                          
         GOTO1 REPORT                                                           
         MVI   SVCPX,C'D'                                                       
         MVC   SVCPD,AHPDEMO                                                    
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         XC    SVCPD,SVCPD                                                      
         B     EXITL                                                            
*                                                                               
VDEM02   LA    R1,SVDEMCDS         POINT TO LIST UPLOADED                       
         L     RF,ADEST                                                         
         AHI   RF,EDEMOS-ESTHDRD                                                
*                                                                               
VDEM04   CLC   0(3,RF),0(R1)       MATCH DEMO                                   
         BE    EXITOK                                                           
*&&DO                                                                           
         LA    RF,3(RF)                                                         
         CLI   1(RF),0             TEST MORE POL DEMOS                          
         BNE   VDEM04              OK - TRY AGAIN                               
*&&                                                                             
         MVC   P(AHRECLQ),HDRREC                                                
         MVC   P2(L'ERROR),ERROR                                                
         MVC   P2+L'ERROR(L'ERRMDEM),ERRMDEM                                    
         LA    RF,P2+L'ERROR+L'ERRMDEM                                          
         MVC   0(L'AHMED,RF),AHMED                                              
         MVI   L'AHMED(RF),C'/'                                                 
         AHI   RF,L'AHMED+1                                                     
         MVC   0(L'AHCLT,RF),AHCLT                                              
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(L'AHPRD,RF),AHPRD                                              
         BRAS  RE,RFSPACE                                                       
         MVI   0(RF),C'/'                                                       
         AHI   RF,1                                                             
         MVC   0(L'AHEST,RF),AHEST                                              
         AHI   RF,L'AHEST+1                                                     
         MVI   0(RF),C'-'                                                       
         MVC   2(L'AHPDEMO,RF),AHPDEMO                                          
         GOTO1 REPORT                                                           
*                                                                               
         MVI   SVCPX,C'V'                                                       
         MVC   SVCPD,AHPDEMO                                                    
         GOTO1 BINSRCH,BMPAR1,(X'01',SVCPX)                                     
         XC    SVCPD,SVCPD                                                      
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT OUT GOAL RECORDS TO SEQUENTIAL FILE                  *         
***********************************************************************         
         SPACE 1                                                                
WRTGOAL  NTR1  ,                                                                
         L     R4,ADGOAL           R4 = A(RECORD)                               
         USING GOALREC,R4                                                       
         LR    R3,R4                                                            
         AHI   R3,-4                                                            
         MVC   FULL,0(R3)          SAVE 4 BYTES IN FRONT OF RECORD              
*                                                                               
         XC    0(4,R3),0(R3)       SET LENGTH OF DATA                           
         XR    RF,RF                                                            
         ICM   RF,3,GLENGTH                                                     
         AHI   RF,4                                                             
         STH   RF,0(R3)                                                         
*                                                                               
         PUT   FILEOUT,(R3)        PUT RECORD                                   
         MVC   0(4,R3),FULL                                                     
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT OUT GOAL RECORDS AS ADDED TO FILE                  *         
***********************************************************************         
         SPACE 1                                                                
PRTGOAL  NTR1  ,                                                                
         GOTO1 PRTREC,DMCB,(C'E',ADGOAL),(24,13),PRINT,HEXOUT                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* STORAGE ADDRESSED FROM RC ONLY - USED AS WORKING STORAGE            *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R9,RA,RB                                                         
         ORG   SPME02+(((*-SPME02)+15)/16*16)                                   
SPMEWORK DC    CL32'SPREPME02 WORKING STORAGE'                                  
*                                                                               
RFSPACE  CLI   0(RF),C' '          FIND FIRST SPACE CHARACTER AFTER RF          
         BNHR  RE                                                               
         AHI   RF,1                                                             
         B     *-10                                                             
*                                                                               
ON31     O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND EQUATES                                                *         
***********************************************************************         
         SPACE 1                                                                
MAXDTLS  EQU   128                 MAX DETAILS PER HEADER                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND EQUATES                                               *         
***********************************************************************         
         SPACE 1                                                                
BINPAR1  DC    A(0)                                                             
BINPAR2  DC    A(CPETAB)                     A(TABLE)                           
BINPAR3  DC    A(0)                          CURRENT RECORD COUNT               
BINPAR4  DC    A(L'CPETAB)                   RECORD LENGTH                      
BINPAR5  DC    A(13)                         KEYDSPL=0/KEYLEN=13                
BINPAR6  DC    A((CPETABX-CPETAB)/L'CPETAB)  MAX ENTRIES                        
BINPAR7  DC    A(0)                                                             
BINPAR8  DC    A(0)                                                             
*                                                                               
BMKPAR1  DC    A(0)                                                             
BMKPAR2  DC    A(MKTTAB)                     A(TABLE)                           
BMKPAR3  DC    A(0)                          CURRENT RECORD COUNT               
BMKPAR4  DC    A(L'MKTTAB)                   RECORD LENGTH                      
BMKPAR5  DC    A(05)                         KEYDSPL=0/KEYLEN=5                 
BMKPAR6  DC    A((MKTTABX-MKTTAB)/L'MKTTAB)  MAX ENTRIES                        
BMKPAR7  DC    A(0)                                                             
BMKPAR8  DC    A(0)                                                             
*                                                                               
BBMPAR1  DC    A(0)                                                             
BBMPAR2  DC    A(BADMKT)                     A(TABLE)                           
BBMPAR3  DC    A(0)                          CURRENT RECORD COUNT               
BBMPAR4  DC    A(L'BADMKT)                   RECORD LENGTH                      
BBMPAR5  DC    A(05)                         KEYDSPL=0/KEYLEN=5                 
BBMPAR6  DC    A((BADMKTX-BADMKT)/L'BADMKT)  MAX ENTRIES                        
BBMPAR7  DC    A(0)                                                             
BBMPAR8  DC    A(0)                                                             
*                                                                               
BHPAR1   DC    A(0)                                                             
BHPAR2   DC    A(BADCPE)                     A(TABLE)                           
BHPAR3   DC    A(0)                          CURRENT RECORD COUNT               
BHPAR4   DC    A(L'BADCPE)                   RECORD LENGTH                      
BHPAR5   DC    A(13)                         KEYDSPL=0/KEYLEN=13                
BHPAR6   DC    A((BADCPEX-BADCPE)/L'BADCPE)  MAX ENTRIES                        
BHPAR7   DC    A(0)                                                             
BHPAR8   DC    A(0)                                                             
*                                                                               
BMPAR1   DC    A(0)                                                             
BMPAR2   DC    A(BADMAP)                     A(TABLE)                           
BMPAR3   DC    A(0)                          CURRENT RECORD COUNT               
BMPAR4   DC    A(L'BADMAP)                   RECORD LENGTH                      
BMPAR5   DC    A(13)                         KEYDSPL=0/KEYLEN=13                
BMPAR6   DC    A((BADMAPX-BADMAP)/L'BADMAP)  MAX ENTRIES                        
BMPAR7   DC    A(0)                                                             
BMPAR8   DC    A(0)                                                             
*                                                                               
DTLCNT   DS    H                   COUNT OF DETAIL LINES                        
ENDSW    DC    C' '                EODAD SWITCH                                 
TOADD    DC    C'N'                RECORD TO ADD                                
SVDTL    DC    CL2'  '                                                          
HDR      DC    CL3'HDR'                                                         
DTL      DC    CL3'DTL'                                                         
WRK      DS    CL80                                                             
*                                                                               
ERRUNX   DC    C'**ERROR** Unexpected object - '                                
ERRNOIP  DC    C'**ERROR** Not in estimate period - '                           
WRNNOIP  DC    C'*Warning* Not in estimate period - '                           
ERRCPE   DC    C'**ERROR** Not found in CPE Input table - '                     
ERRMKT   DC    C'**ERROR** Not found in Market table - '                        
*                                                                               
ERRINV   DC    C'**ERROR** Invalid '                                            
ERROR    DC    C'**ERROR** '                                                    
WRNINV   DC    C'*WARNING* Invalid '                                            
ERRDPT   DC    C'Daypart - '                                                    
ERRDEMO  DC    C'Demo - '                                                       
ERRMDEM  DC    C'Demo not 1st in Estimate Header - '                            
ERRSLN   DC    C'Spot Length - '                                                
ERRCST   DC    C'Spot Cost - '                                                  
ERRGPS   DC    C'GRPs Value - '                                                 
ERRWOD   DC    C'Week-Of Date - '                                               
ERRWODF  DC    C'Week-Of Date not in flight - '                                 
ERRCPP   DC    C'CPP Value - '                                                  
ERRCLT   DC    C'Client - '                                                     
ERRPRD   DC    C'Product - '                                                    
ERREST   DC    C'Estimate - '                                                   
ERRMKTB  DC    C'Market - '                                                     
ERRBEST  DC    C'Brand Estimate - '                                             
ERRPEST  DC    C'Pool Estimate - '                                              
ERRPERST DC    C'Header start (not numeric) - '                                 
ERRPEREN DC    C'Header end (not numeric) - '                                   
*                                                                               
VDEMOVAL DS    A                                                                
CASHVAL  DC    V(CASHVAL)                                                       
PRTREC   DC    V(PRTREC)                                                        
*                                                                               
BESTART  DS    XL3                                                              
BEEND    DS    XL3                                                              
*                                                                               
SVDEMCDS DS    XL6                                                              
SVCPX    DS    XL1                                                              
SVCPE    DS    XL23                                                             
SVCPD    DS    CL6                                                              
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,MACRF=GM,EODAD=EOFTIN,RECFM=FB,  X        
               LRECL=53                                                         
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=PM,BLKSIZE=25000,RECFM=VB,X        
               LRECL=4004                                                       
CPEFILE  DCB   DDNAME=CPEFILE,DSORG=PS,MACRF=GM,RECFM=FB,EODAD=BLDCP04,X        
               LRECL=22                                                         
MKTFILE  DCB   DDNAME=MKTFILE,DSORG=PS,MACRF=GM,RECFM=FB,EODAD=BLDMK04,X        
               LRECL=10                                                         
BADCPES  DCB   DDNAME=BADCPES,DSORG=PS,MACRF=PM,RECFM=FB,LRECL=80               
BADMAPS  DCB   DDNAME=BADMAPS,DSORG=PS,MACRF=PM,RECFM=FB,LRECL=80               
BADMKTS  DCB   DDNAME=BADMKTS,DSORG=PS,MACRF=PM,RECFM=FB,LRECL=80               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECTS                                                                  
***********************************************************************         
         SPACE 1                                                                
         ORG   SPME02+(((*-SPME02)+15)/16*16)                                   
         DC    CL8'RECIN==>'                                                    
RECINL   DS    F                                                                
RECIN    DS    XL124                                                            
*                                                                               
         ORG   SPME02+(((*-SPME02)+15)/16*16)                                   
         DC    CL8'HDR====>'                                                    
HDRREC   DS    XL(AHRECLQ)                                                      
*                                                                               
         ORG   SPME02+(((*-SPME02)+15)/16*16)                                   
         DC    CL8'DTLS===>'                                                    
DTLRECS  DS    (MAXDTLS)CL(DTLDLQ)                                              
*                                                                               
         ORG   SPME02+(((*-SPME02)+15)/16*16)                                   
         DC    CL8'CPETAB=>'                                                    
CPETAB   DC    5000XL24'00'                                                     
CPETABX  EQU   *                                                                
*                                                                               
         DC    CL8'MKTTAB=>'                                                    
MKTTAB   DC    20000XL10'00'                                                    
MKTTABX  EQU   *                                                                
*                                                                               
         ORG   SPME02+(((*-SPME02)+15)/16*16)                                   
         DC    CL8'BADMAP=>'                                                    
BADMAP   DC    1000XL30'00'                                                     
BADMAPX  EQU   *                                                                
*                                                                               
         ORG   SPME02+(((*-SPME02)+15)/16*16)                                   
         DC    CL8'BADCPE=>'                                                    
BADCPE   DC    5000XL13'00'                                                     
BADCPEX  EQU   *                                                                
*                                                                               
         DC    CL8'BADMKT=>'                                                    
BADMKT   DC    5000XL10'00'                                                     
BADMKTX  EQU   *                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ADWARE CONVERSION INPUT FILE FORMAT DSECTS                          *         
***********************************************************************         
         SPACE 1                                                                
AHRECD   DSECT                     HDR RECORD                                   
AHREC    DS    CL3                                                              
AHCPY    DS    CL2                                                              
AHOFF    DS    CL2                                                              
AHMED    DS    CL1                                                              
AHCLT    DS    CL4                                                              
AHPRD    DS    CL4                                                              
AHEST    DS    CL4                                                              
AHMKT    DS    CL4                                                              
AHSTFL   DS    CL6                                                              
AHENFL   DS    CL6                                                              
AHPDEMO  DS    CL6                                                              
AHRECLQ  EQU   *-AHRECD                                                         
*                                                                               
ADRECD   DSECT                     DTL RECORD                                   
ADREC    DS    CL3                                                              
ADDYPT   DS    C                                                                
ADWOD    DS    CL6                                                              
ADSLN    DS    CL3                                                              
ADGPS    DS    CL5                                                              
ADSCST   DS    CL9                                                              
ADCPP    DS    CL9                                                              
ADRECLQ  EQU   *-ADRECD                                                         
*                                                                               
DTLD     DSECT                                                                  
DTLDYPT  DS    X                   DAYPART                                      
DTLSLN   DS    X                   SPOT LENGTH                                  
DTLWK    DS    XL2                 WEEK                                         
DTLGRP   DS    XL4                 GRPS PER SPOT                                
DTLCST   DS    XL4                 SPOT COST                                    
DTLCPP   DS    XL4                 CPP                                          
DTLDLQ   EQU   *-DTLD                                                           
*                                                                               
CPED     DSECT                     CPE INPUT FILE - BINSRCH RECORD              
CPEMED   DS    C                   MEDIA                                        
CPECLT   DS    CL4                 CLIENT - ADWARE                              
CPEPRD   DS    CL4                 PRODUCT                                      
CPEEST   DS    CL4                 ESTIMATE                                     
CPEDCLT  DS    CL3                 CLIENT - DDS                                 
CPEDPRD  DS    CL3                 PRODUCT                                      
CPEDEST  DS    CL3                 ESTIMATE                                     
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
GOALRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
CLTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
ESTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
STARECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
*SPREPMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
*SPREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
*                                                                               
* DSECT FOR PRINT LINE                                                          
*                                                                               
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PERR     DS    CL10                                                             
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         EJECT                                                                  
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'144SPREPME02 03/19/02'                                      
         END                                                                    
