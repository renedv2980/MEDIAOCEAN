*          DATA SET ACREPZ602  AT LEVEL 162 AS OF 08/16/00                      
*PHASE ACZ602A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'FIX COMPANY CODES'                                              
ACZ602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZ6**,R9                                                    
         L     RA,0(,R1)                                                        
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZ6D,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*        CLI   MODE,LEDGFRST                                                    
*        BE    LDGF                                                             
*        CLI   MODE,PROCLEVA                                                    
*        BE    PRCLVLS                                                          
*        CLI   MODE,PROCLEVB                                                    
*        BE    PRCLVLS                                                          
*        CLI   MODE,PROCLEVC                                                    
*        BE    PRCLVLS                                                          
*        CLI   MODE,PROCLEVD                                                    
*        BE    PRCLVLS                                                          
*        CLI   MODE,PROCTRNS                                                    
*        BE    PTRN                                                             
*        CLI   MODE,LEDGLAST                                                    
*        BE    LDGL                                                             
         B     XIT                                                              
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         ZAP   ACDR,=P'0'                                                       
         ZAP   ACCR,=P'0'                                                       
         ZAP   JACDR,=P'0'                                                      
         ZAP   JACCR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
         ZAP   TOTDR,=P'0'                                                      
         ZAP   JTOTCR,=P'0'                                                     
         ZAP   JTOTDR,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              REQUEST FIRST                                          *         
***********************************************************************         
         USING TRNRECD,R2                                                       
REQF     DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,RQSTART)                               
         GOTO1 DATCON,DMCB,(0,QEND),(1,RQEND)                                   
         MVI   LASTCPY,0                                                        
         LA    R2,IODIR                                                         
         MVC   TRNKEY,SPACES                                                    
         MVI   TRNKCPY,X'40'                                                    
         LA    R3,DMRDHI                                                        
*                                                                               
REQF01   CLI   QOPT1,C'D'                                                       
         BNE   REQF00A                                                          
         GOTO1 PRNTBL,PARM,=C'KEY',IODIR,C'DUMP',A(L'IODIR),=C'1D'              
*                                                                               
REQF00A  GOTO1 DATAMGR,PARM,(R3),ACCDIR,IODIR,IO,IOWRK                          
         TM    8(R1),X'80'                                                      
         BO    XIT                 EOF                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   IODIR,IO                                                         
         CLI   QOPT1,C'D'                                                       
         BNE   REQF00C                                                          
         GOTO1 PRNTBL,PARM,=C'REC',IODIR,C'DUMP',A(L'IODIR),=C'1D'              
*                                                                               
REQF00C  CLI   IODIR,X'FE'                                                      
         BE    XIT                                                              
         LA    R2,IO                                                            
         MVC   IODA,TRNKDA                                                      
         CLC   TRNKUNT(41),SPACES  COMPANY RECORD                               
         BNE   REQF10              NO                                           
         LA    R3,IODA                                                          
         GOTO1 DATAMGR,PARM,DMGET,ACCMST,(R3),IO,IOWRK                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING CPYELD,R4                                                        
         LA    R4,TRNRFST                                                       
         LA    R2,IODIR                                                         
REQF01A  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R4),X'10'                                                      
         BE    REQF01B                                                          
         SR    R1,R1                                                            
         IC    R1,CPYLN                                                         
         AR    R4,R1                                                            
         B     REQF01A                                                          
*                                                                               
REQF01B  MVC   P+1(4),=CL4'AGY='                                                
         MVC   P+6(7),CPYLOGO                                                   
         MVC   P+20(6),=CL6'ALPHA='                                             
         MVC   P+28(2),CPYALPHA                                                 
         GOTO1 ACREPORT                                                         
         MVC   LASTCPY,IODIR                                                    
         MVC   TRNKUNT(2),QUNIT                                                 
         LA    R3,DMRDHI                                                        
         B     REQF01                                                           
         DROP  R4                                                               
*                                                                               
REQF10   CLC   TRNKUNT(2),QUNIT                                                 
         BE    REQF14                                                           
         LA    R2,IODIR                                                         
         MVC   TRNKUNT(L'TRNKEY-1),SPACES                                       
         MVI   TRNKUNT,X'FF'       NEXT COMPANY                                 
         LA    R3,DMRDHI                                                        
         B     REQF01                                                           
*                                                                               
REQF14   CLC   TRNKACT,SPACES                                                   
         BE    REQSEQ                                                           
         CLC   TRNKEY+7(4),SPACES                                               
         BE    REQNXAC             NEXT ACCOUNT                                 
         CLC   TRNKOFF(TRNKSBR-TRNKOFF),SPACES                                  
         BE    REQF20              LOOK AT ACCOUNT                              
         CLC   TRNKDATE,SPACES                                                  
         BNH   REQSEQ                                                           
         CLI   TRNKSTYP,X'22'                                                   
         BNE   REQNXAC                                                          
         MVC   P,SPACES                                                         
         MVC   P+2(9),=CL9'ACCOUNT='                                            
         MVC   P+12(14),TRNKUNT                                                 
         GOTO1 ACREPORT                                                         
         B     REQNXAC                                                          
*                                                                               
REQF20   LA    R3,IODA                                                          
         GOTO1 DATAMGR,PARM,DMGET,ACCMST,(R3),IO,IOWRK                          
         LA    R2,IO                                                            
         LA    R4,TRNRFST                                                       
         LA    R2,IODIR                                                         
*                                                                               
         USING RSTELD,R4                                                        
REQ20A   CLI   0(R4),0                                                          
         BE    REQNXAC                                                          
         CLI   0(R4),RSTELQ        X'30'                                        
         BE    REQF21                                                           
         SR    R1,R1                                                            
         IC    R1,RSTLN                                                         
         AR    R4,R1                                                            
         B     REQ20A                                                           
*                                                                               
REQF21   CLC   RSTBDATE,RQEND                                                   
         BH    REQNXAC                                                          
         CLC   RSTBDATE,RQSTART                                                 
         BL    REQNXAC                                                          
         DROP  R4                                                               
*                                                                               
REQSEQ   LA    R3,DMRSEQ                                                        
         B     REQF01                                                           
*                                                                               
REQNXAC  LA    R2,IODIR                                                         
         MVI   TRNKOFF,X'FF'                                                    
         LA    R3,DMRDHI                                                        
         B     REQF01                                                           
         DROP  R2                                                               
*                                                                               
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
*                                                                               
LDGF     DS    0H                                                               
         MVI   0(R7),X'FF'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   FCRDTRNS,NO         AND TRANSACTIONS                             
         L     R2,ADLEDGER                                                      
         CLC   1(2,R2),=C'SJ'      FOR SJ                                       
         BNE   XIT                                                              
         MVI   FORCEHED,YES                                                     
*        MVI   FCRDTRNS,C'Y'                                                    
         ZAP   CHAREC,=P'0'                                                     
         ZAP   ACDR,=P'0'                                                       
         ZAP   ACCR,=P'0'                                                       
         ZAP   JACDR,=P'0'                                                      
         ZAP   JACCR,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PROCLEVELS                                             *         
***********************************************************************         
         USING TRNRECD,R3                                                       
PRCLVLS  DS    0H                                                               
         L     R3,ADACC                                                         
         MVI   WRITESW,NO                                                       
         CLC   1(2,R3),=C'SJ'      FOR SJ                                       
         BNE   PRCLVLX                                                          
         LR    R4,R3                                                            
         AH    R4,DATADISP                                                      
PRCLV10  CLI   0(R4),0             EOR                                          
         BE    PRCLVLX                                                          
         CLI   0(R4),X'24'                                                      
         BE    PRCLV15                                                          
PRCLV12  SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     PRCLV10                                                          
*                                                                               
         USING PPRELD,R4                                                        
PRCLV15  CLI   PPRRECVC,0                                                       
         BE    PRCLV16                                                          
         CLC   PPRRECVC,RCCOMPFL                                                
         BNE   PRCLV20                                                          
PRCLV16  CLI   PPRCOSTC,0                                                       
         BE    PRCLV12                                                          
         CLC   PPRCOSTC,RCCOMPFL                                                
         BNE   PRCLV20                                                          
         B     PRCLV12                                                          
*                                                                               
PRCLV20  CLI   QOPT1,C'D'          BEFORE                                       
         BNE   PRCLV21                                                          
         GOTO1 DMPREC,DMCB,ADACC                                                
*                                                                               
PRCLV21  CLI   PPRRECVC,0                                                       
         BE    *+10                                                             
         MVC   PPRRECVC,RCCOMPFL                                                
         CLI   PPRCOSTC,0                                                       
         BE    *+10                                                             
         MVC   PPRCOSTC,RCCOMPFL                                                
         MVC   P+1(14),TRNKULA                                                  
         MVC   P+20(14),PPRRECVC+1                                              
         MVC   P+40(14),PPRCOSTC+1                                              
         MVI   WRITESW,YES                                                      
         GOTO1 ACREPORT                                                         
         B     PRCLV12                                                          
*                                                                               
PRCLVLX  CLI   WRITESW,NO                                                       
         BE    XIT                                                              
         AP    TOTCHGS,=P'1'                                                    
         CLI   QOPT2,C'D'          AFTER                                        
         BNE   PRCLVLX2                                                         
         GOTO1 DMPREC,DMCB,ADACC                                                
*                                                                               
PRCLVLX2 CLI   RCWRITE,NO                                                       
         BE    XIT                                                              
         CLI   MODE,PROCLEVA                                                    
         BNE   *+8                                                              
         MVI   MODE,WRITLEVA                                                    
         CLI   MODE,PROCLEVB                                                    
         BNE   *+8                                                              
         MVI   MODE,WRITLEVB                                                    
         CLI   MODE,PROCLEVC                                                    
         BNE   *+8                                                              
         MVI   MODE,WRITLEVC                                                    
*        L     R3,ADACC                                                         
*        GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',(R3),(R3)                         
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'00'                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PROCTRNS                                               *         
***********************************************************************         
PTRN     DS    0H                                                               
         L     R5,ADTRANS                                                       
         USING TRNELD,R5                                                        
         LR    R3,R5                                                            
         SH    R3,DATADISP                                                      
*                                                                               
         USING TRNRECD,R3                                                       
         CLI   QOPT1,C'S'          STRANGE OFFICE CODES ONLY?                   
         BNE   PTRN02                                                           
         LA    RE,OFFCHAR                                                       
         LA    RF,L'OFFCHAR                                                     
PTRN00   CLC   TRNOFFC(1),0(RE)    JUST CHECK 1ST CHAR                          
         BE    XIT                                                              
         LA    RE,2(RE)                                                         
         BCT   RF,PTRN00                                                        
         B     PTRN114                                                          
*                                                                               
PTRN02   CLI   TRNOFFC,C'T'                                                     
         BNE   XIT                                                              
         CLI   QOPT1,C'Y'                                                       
         BE    PTRN114                                                          
*                                                                               
         USING OFTABD,R6                                                        
         LA    R6,OFFLIST                                                       
PTRN102  CLI   0(R6),X'FF'                                                      
         BE    XIT                  NOT A CLIENT I NEED                         
         CLC   OCLIENT,TRNKCACT+9   CONTRA ACCOUNT                              
         BE    PTRN112                                                          
PTRN108  LA    R6,OFLEN(,R6)                                                    
         B     PTRN102                                                          
*                                                                               
PTRN112  MVC   TRNOFFC,OOFFICE                                                  
         B     PTRN114                                                          
*&&DO                                                                           
PTRN112  DS    0H                                                               
         BAS   RE,DMPGET                                                        
         CLI   QOPT2,X'40'                                                      
         BNH   PTRN114                                                          
         MVC   TRNOFFC(1),QOPT2                                                 
         MVI   TRNOFFC+1,C' '                                                   
*                                                                               
*TRN112  MVC   TRNOFFC,=C'  '                                                   
*        MVC   TRNOFFC(1),ACKEYACC+3                                            
*                                                                               
*                                                                               
*TRN112  CLC   ACKEYACC+1(2),=C'SR'                                             
*        BNE   *+14                                                             
*        MVC   TRNOFFC(1),ACKEYACC+4                                            
*        B     PTRN114                                                          
*        CLC   ACKEYACC+1(2),=C'SC'                                             
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        CLC   ACKEYCON+1(2),=C'SR'                                             
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        MVC   TRNOFFC(1),ACKEYCON+4                                            
*        CLC   ACKEYACC+1(2),=C'11'                                             
*        BE    PTRN112A                                                         
*        CLC   ACKEYACC+1(2),=C'12'                                             
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        B     PTRN112E                                                         
*TRN112C CLC   ACKEYCON+1(2),=C'1N'                                             
*        BE    *+6                                                              
*        DC    H'0'                                                             
*TRN112D MVC   TRNOFFC(1),ACKEYACC+3                                            
*TRN112E MVI   TRNOFFC+1,C' '                                                   
*                                                                               
*&&                                                                             
PTRN114  DS    0H                                                               
         MVC   P+1(14),TRNKULA                                                  
         MVC   P+17(14),TRNKULC                                                 
         MVC   P+33(6),TRNKREF                                                  
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(0,P+42)                                
         MVC   P+49(2),TRNOFFC                                                  
         MVC   P+54(6),TRNBTCH                                                  
         EDIT  (B1,TRNTYPE),(2,P+62)                                            
         TM    TRNSTAT,X'80'                                                    
         BZ    PTRN118                                                          
         EDIT  (P6,TRNAMNT),(12,P+68),2,MINUS=YES                               
         AP    ACDR,TRNAMNT                                                     
         B     PTRN120                                                          
*                                                                               
PTRN118  EDIT  (P6,TRNAMNT),(12,P+91),2,MINUS=YES                               
         AP    ACCR,TRNAMNT                                                     
*                                                                               
PTRN120  DS    0H                                                               
         GOTO1 ACREPORT                                                         
         AP    CHAREC,=P'1'                                                     
         BAS   RE,DMPPUT                                                        
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         MVI   MODE,WRITRANS                                                    
         B     XIT                                                              
*                                                                               
PTRN130  GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              LEDGER LAST                                            *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R3                                                       
LDGL     DS    0H                                                               
         L     R3,ADACC                                                         
         GOTO1 ACREPORT                                                         
         MVC   P(7),=C'LEDGER '                                                 
         MVC   P+8(1),TRNKLDG                                                   
         MVC   P+10(6),=C'TOTAL '                                               
         EDIT  (P4,TOTCHGS),(14,P+20),MINUS=YES                                 
*        EDIT  (P6,ACDR),(12,P+68),2,MINUS=YES                                  
*        EDIT  (P6,ACCR),(12,P+91),2,MINUS=YES                                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DUMP OUT RECORDS                                       *         
***********************************************************************         
*                                                                               
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         SR    R8,R8                                                            
         ICM   R8,3,TRNRLEN                                                     
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
XIT      XIT1                                                                   
         EJECT                                                                  
         USING TRNRECD,R3                                                       
DMPREC   NTR1                                                                   
         L     R3,0(,R1)                                                        
         SR    R8,R8                                                            
         ICM   R8,3,TRNRLEN                                                     
         GOTO1 PRNTBL,DMCB,=C'REC',(R3),C'DUMP',(R8),=C'2D'                     
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
*                                                                               
CHAREC   DC    PL6'0'                                                           
*                                                                               
*                                                                               
ACDR     DC    PL6'0'                                                           
ACCR     DC    PL6'0'                                                           
JACDR    DC    PL6'0'                                                           
JACCR    DC    PL6'0'                                                           
*                                                                               
TOTDR    DC    PL6'0'                                                           
TOTCR    DC    PL6'0'                                                           
JTOTDR   DC    PL6'0'                                                           
JTOTCR   DC    PL6'0'                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'50'                                                          
*                                                                               
TOTCHGS  DC    PL4'0'                                                           
         EJECT                                                                  
*OFFLIST  DS     0H                                                             
         DC     CL2'CA'                                                         
         DC     CL2'CM'                                                         
         DC     CL2'CO'                                                         
         DC     CL2'CS'                                                         
         DC     CL2'HA'                                                         
         DC     CL2'LA'                                                         
         DC     CL2'LC'                                                         
         DC     CL2'LM'                                                         
         DC     CL2'LP'                                                         
         DC     CL2'LR'                                                         
         DC     CL2'NA'                                                         
         DC     CL2'NC'                                                         
         DC     CL2'NP'                                                         
         DC     CL2'NR'                                                         
         DC     CL2'NS'                                                         
         DC     CL2'TA'                                                         
         DC     CL2'UA'                                                         
         DC     CL2'WA'                                                         
         DC     CL2'XA'                                                         
         DC     CL2'YA'                                                         
         DC     CL2'ZA'                                                         
         DC     X'FF'                                                           
*                                                                               
OFFLIST  DC     CL3'BNL',CL2'M '                                                
         DC     CL3'IP ',CL2'M '                                                
         DC     CL3'MCI',CL2'M '                                                
         DC     X'FF'                                                           
*                                                                               
*        ALL VALID 1 CHARACTER OFFICES(IGNORE BLANK)                            
OFFCHAR  DC    CL73'A B C D E F G H I J K L M N O P Q R S T U V W X Y YX        
                1 2 3 4 5 6 7 8 9 9   '                                         
         EJECT                                                                  
         LTORG                                                                  
OFTABLE  DC   60XL(OFFLNQ)'00'                                                  
*                                                                               
CHKTAB   DC    CL2'SP',C'101942',C'101955',X'970530',C'C '                      
         DC    CL2'SP',C'655794',C'655796',X'970520',C'D '                      
         DC    CL2'SP',C'655927',C'655928',X'970523',C'D '                      
         DC    CL2'SP',C'655948',C'655950',X'970528',C'D '                      
         DC    CL2'SP',C'655800',C'655832',X'970530',C'D '                      
         DC    CL2'SP',C'655959',C'655966',X'970530',C'D '                      
         DC    CL2'SP',C'655968',C'655977',X'970530',C'D '                      
         DC    CL2'SP',C'102552',C'102552',X'970523',C'E '                      
         DC    CL2'SP',C'102687',C'102690',X'970528',C'E '                      
         DC    CL2'SP',C'101956',C'101964',X'970530',C'E '                      
         DC    CL2'SP',C'102806',C'102808',X'970530',C'E '                      
         DC    CL2'SP',C'102667',C'102667',X'970523',C'G '                      
         DC    CL2'SP',C'101584',C'101586',X'970520',C'K '                      
         DC    CL2'SP',C'102547',C'102547',X'970523',C'K '                      
         DC    CL2'SP',C'102570',C'102590',X'970523',C'K '                      
         DC    CL2'SP',C'102692',C'102705',X'970528',C'K '                      
         DC    CL2'SP',C'102141',C'102150',X'970530',C'K '                      
         DC    CL2'SP',C'102153',C'102166',X'970530',C'K '                      
         DC    CL2'SP',C'102862',C'102885',X'970530',C'K '                      
         DC    CL2'SP',C'655929',C'655930',X'970523',C'L '                      
         DC    CL2'SP',C'655833',C'655863',X'970530',C'L '                      
         DC    CL2'SP',C'655978',C'655979',X'970530',C'L '                      
         DC    CL2'SP',C'655797',C'655797',X'970520',C'M '                      
         DC    CL2'SP',C'655931',C'655931',X'970523',C'M '                      
         DC    CL2'SP',C'655951',C'655951',X'970528',C'M '                      
         DC    CL2'SP',C'655954',C'655955',X'970528',C'M '                      
         DC    CL2'SP',C'655864',C'655868',X'970530',C'M '                      
         DC    CL2'SP',C'655980',C'655985',X'970530',C'M '                      
         DC    CL2'SP',C'655798',C'655799',X'970520',C'N '                      
         DC    CL2'SP',C'655932',C'655933',X'970523',C'N '                      
         DC    CL2'SP',C'655952',C'655953',X'970528',C'N '                      
         DC    CL2'SP',C'655956',C'655957',X'970528',C'N '                      
         DC    CL2'SP',C'655869',C'655886',X'970530',C'N '                      
         DC    CL2'SP',C'655986',C'656017',X'970530',C'N '                      
         DC    CL2'SP',C'101587',C'101590',X'970520',C'P '                      
         DC    CL2'SP',C'102591',C'102592',X'970523',C'P '                      
         DC    CL2'SP',C'102677',C'102677',X'970527',C'P '                      
         DC    CL2'SP',C'102706',C'102706',X'970528',C'P '                      
         DC    CL2'SP',C'102167',C'102167',X'970530',C'P '                      
         DC    CL2'SP',C'102886',C'102886',X'970530',C'P '                      
         DC    CL2'SP',C'655958',C'655958',X'970528',C'S '                      
         DC    CL2'SP',C'655887',C'655894',X'970530',C'S '                      
         DC    CL2'SP',C'102168',C'102168',X'970530',C'W '                      
         DC    CL2'SP',C'101552',C'101552',X'970520',C'X '                      
         DC    CL2'SP',C'101591',C'101616',X'970520',C'X '                      
         DC    CL2'SP',C'101635',C'101658',X'970520',C'X '                      
         DC    CL2'SP',C'101660',C'101667',X'970520',C'X '                      
         DC    CL2'SP',C'101686',C'101772',X'970520',C'X '                      
         DC    CL2'SP',C'101774',C'101905',X'970520',C'X '                      
         DC    CL2'SP',C'101911',C'101941',X'970520',C'X '                      
         DC    CL2'SP',C'102521',C'102528',X'970521',C'X '                      
         DC    CL2'SP',C'102535',C'102535',X'970522',C'X '                      
         DC    CL2'SP',C'102538',C'102540',X'970522',C'X '                      
         DC    CL2'SP',C'102545',C'102545',X'970523',C'X '                      
         DC    CL2'SP',C'102549',C'102550',X'970523',C'X '                      
         DC    CL2'SP',C'102593',C'102597',X'970523',C'X '                      
         DC    CL2'SP',C'102604',C'102606',X'970523',C'X '                      
         DC    CL2'SP',C'102619',C'102634',X'970523',C'X '                      
         DC    CL2'SP',C'102678',C'102679',X'970527',C'X '                      
         DC    CL2'SP',C'102686',C'102686',X'970528',C'X '                      
         DC    CL2'SP',C'102707',C'102721',X'970528',C'X '                      
         DC    CL2'SP',C'103114',C'103124',X'970529',C'X '                      
         DC    CL2'SP',C'102887',C'102963',X'970530',C'X '                      
         DC    CL2'SP',C'102965',C'103005',X'970530',C'X '                      
         DC    CL2'SP',C'103007',C'103025',X'970530',C'X '                      
         DC    CL2'SP',C'521409',C'521429',X'970521',C'Y '                      
         DC    CL2'SP',C'101617',C'101617',X'970520',C'Z '                      
         DC    CL2'SP',C'102548',C'102548',X'970523',C'Z '                      
         DC    CL2'SP',C'102635',C'102635',X'970523',C'Z '                      
         DC    CL2'SP',C'102169',C'102194',X'970530',C'Z '                      
         DC    CL2'SP',C'103026',C'103036',X'970530',C'Z '                      
         DC    CL2'SP',C'102195',C'102195',X'970530',C'1 '                      
         DC    CL2'SP',C'103037',C'103047',X'970530',C'1 '                      
         DC    CL2'SP',C'101618',C'101618',X'970520',C'2 '                      
         DC    CL2'SP',C'101668',C'101671',X'970520',C'2 '                      
         DC    CL2'SP',C'102536',C'102537',X'970522',C'2 '                      
         DC    CL2'SP',C'102636',C'102647',X'970523',C'2 '                      
         DC    CL2'SP',C'102722',C'102727',X'970528',C'2 '                      
         DC    CL2'SP',C'102196',C'102239',X'970530',C'2 '                      
         DC    CL2'SP',C'103048',C'103079',X'970530',C'2 '                      
         DC    CL2'SP',C'521336',C'521357',X'970520',C'3 '                      
         DC    CL2'SP',C'521408',C'521408',X'970521',C'3 '                      
         DC    CL2'SP',C'521434',C'521434',X'970522',C'3 '                      
         DC    CL2'SP',C'521438',C'521458',X'970523',C'3 '                      
         DC    CL2'SP',C'521462',C'521462',X'970527',C'3 '                      
         DC    CL2'SP',C'521466',C'521467',X'970528',C'3 '                      
         DC    CL2'SP',C'521358',C'521403',X'970530',C'3 '                      
         DC    CL2'SP',C'521468',C'521483',X'970530',C'3 '                      
         DC    CL2'SP',C'102648',C'102654',X'970523',C'4 '                      
         DC    CL2'SP',C'102728',C'102728',X'970528',C'4 '                      
         DC    CL2'SP',C'101672',C'101672',X'970520',C'5 '                      
         DC    CL2'SP',C'102240',C'102272',X'970530',C'5 '                      
         DC    CL2'SP',C'103080',C'103082',X'970530',C'5 '                      
         DC    CL2'SP',C'101619',C'101622',X'970520',C'6 '                      
         DC    CL2'SP',C'101673',C'101682',X'970520',C'6 '                      
         DC    CL2'SP',C'102544',C'102544',X'970523',C'6 '                      
         DC    CL2'SP',C'102546',C'102546',X'970523',C'6 '                      
         DC    CL2'SP',C'102551',C'102551',X'970523',C'6 '                      
         DC    CL2'SP',C'102655',C'102662',X'970523',C'6 '                      
         DC    CL2'SP',C'102729',C'102734',X'970528',C'6 '                      
         DC    CL2'SP',C'102273',C'102468',X'970530',C'6 '                      
         DC    CL2'SP',C'103083',C'103094',X'970530',C'6 '                      
         DC    CL2'SP',C'101683',C'101685',X'970520',C'7 '                      
         DC    CL2'SP',C'102663',C'102666',X'970523',C'7 '                      
         DC    CL2'SP',C'102469',C'102516',X'970530',C'7 '                      
         DC    CL2'SP',C'103095',C'103109',X'970530',C'7 '                      
         DC    CL2'SP',C'101553',C'101553',X'970520',C'9 '                      
         DC    CL2'SP',C'102529',C'102530',X'970521',C'9 '                      
         DC    CL2'SP',C'102735',C'102805',X'970528',C'9 '                      
*                                                                               
         DC    CL2'SS',C'385474',C'385477',X'970521',C'G '                      
         DC    CL2'SS',C'387338',C'387339',X'970521',C'G '                      
         DC    CL2'SS',C'388505',C'388519',X'970530',C'G '                      
         DC    CL2'SS',C'385462',C'385462',X'970520',C'H '                      
         DC    CL2'SS',C'385478',C'385478',X'970521',C'H '                      
         DC    CL2'SS',C'385713',C'385714',X'970530',C'H '                      
         DC    CL2'SS',C'385716',C'385716',X'970530',C'H '                      
         DC    CL2'SS',C'655898',C'655910',X'970521',C'S '                      
         DC    CL2'SS',C'655937',C'655943',X'970527',C'S '                      
         DC    CL2'SS',C'656022',C'656303',X'970529',C'S '                      
         DC    CL2'SS',C'655911',C'655917',X'970530',C'S '                      
         DC    CL2'SS',C'655944',C'655944',X'970530',C'S '                      
         DC    CL2'SS',C'385609',C'385610',X'970521',C'U '                      
         DC    CL2'SS',C'388114',C'388115',X'970527',C'U '                      
         DC    CL2'SS',C'386376',C'386395',X'970530',C'U '                      
         DC    CL2'SS',C'386396',C'386396',X'970530',C'X '                      
         DC    CL2'SS',C'385611',C'385611',X'970521',C'Z '                      
         DC    CL2'SS',C'385612',C'385631',X'970521',C'2 '                      
         DC    CL2'SS',C'387621',C'387627',X'970527',C'2 '                      
         DC    CL2'SS',C'386397',C'386462',X'970530',C'2 '                      
         DC    CL2'SS',C'388165',C'388225',X'970530',C'2 '                      
         DC    CL2'SS',C'388467',C'388467',X'970530',C'2 '                      
         DC    CL2'SS',C'385632',C'385650',X'970521',C'3 '                      
         DC    CL2'SS',C'385652',C'385660',X'970521',C'3 '                      
         DC    CL2'SS',C'387371',C'387372',X'970527',C'3 '                      
         DC    CL2'SS',C'387628',C'387662',X'970527',C'3 '                      
         DC    CL2'SS',C'388594',C'388598',X'970529',C'3 '                      
         DC    CL2'SS',C'386463',C'386750',X'970530',C'3 '                      
         DC    CL2'SS',C'386752',C'386829',X'970530',C'3 '                      
         DC    CL2'SS',C'388226',C'388285',X'970530',C'3 '                      
         DC    CL2'SS',C'388468',C'388484',X'970530',C'3 '                      
         DC    CL2'SS',C'385473',C'385473',X'970521',C'4 '                      
         DC    CL2'SS',C'385661',C'385671',X'970521',C'4 '                      
         DC    CL2'SS',C'387344',C'387365',X'970522',C'4 '                      
         DC    CL2'SS',C'387663',C'387667',X'970527',C'4 '                      
         DC    CL2'SS',C'388599',C'388609',X'970529',C'4 '                      
         DC    CL2'SS',C'388611',C'388631',X'970529',C'4 '                      
         DC    CL2'SS',C'386830',C'387029',X'970530',C'4 '                      
         DC    CL2'SS',C'385672',C'385686',X'970521',C'5 '                      
         DC    CL2'SS',C'387668',C'387677',X'970527',C'5 '                      
         DC    CL2'SS',C'387030',C'387143',X'970530',C'5 '                      
         DC    CL2'SS',C'388286',C'388319',X'970530',C'5 '                      
         DC    CL2'SS',C'388485',C'388489',X'970530',C'5 '                      
         DC    CL2'SS',C'385463',C'385466',X'970520',C'6 '                      
         DC    CL2'SS',C'385472',C'385472',X'970521',C'6 '                      
         DC    CL2'SS',C'385687',C'385712',X'970521',C'6 '                      
         DC    CL2'SS',C'387678',C'387690',X'970527',C'6 '                      
         DC    CL2'SS',C'388632',C'388632',X'970529',C'6 '                      
         DC    CL2'SS',C'387144',C'387297',X'970530',C'6 '                      
         DC    CL2'SS',C'388320',C'388331',X'970530',C'6 '                      
         DC    CL2'SS',C'388490',C'388500',X'970530',C'6 '                      
         DC    CL2'SS',C'388116',C'388164',X'970527',C'7 '                      
         DC    CL2'SS',C'388633',C'388633',X'970529',C'7 '                      
         DC    CL2'SS',C'387298',C'387337',X'970530',C'7 '                      
         DC    CL2'SS',C'388501',C'388504',X'970530',C'7 '                      
*                                                                               
         DC    CL2'SU',C'655921',C'655922',X'970523',C'L '                      
         DC    CL2'SU',C'111444',C'111444',X'970527',C'Z '                      
         DC    CL2'SU',C'111352',C'111352',X'970520',C'2 '                      
         DC    CL2'SU',C'111360',C'111368',X'970523',C'2 '                      
         DC    CL2'SU',C'111415',C'111418',X'970523',C'2 '                      
         DC    CL2'SU',C'111435',C'111435',X'970527',C'2 '                      
         DC    CL2'SU',C'111473',C'111473',X'970529',C'2 '                      
         DC    CL2'SU',C'111484',C'111484',X'970529',C'2 '                      
         DC    CL2'SU',C'111369',C'111382',X'970523',C'3 '                      
         DC    CL2'SU',C'111419',C'111422',X'970523',C'3 '                      
         DC    CL2'SU',C'111445',C'111450',X'970527',C'3 '                      
         DC    CL2'SU',C'111464',C'111464',X'970527',C'3 '                      
         DC    CL2'SU',C'111474',C'111474',X'970529',C'3 '                      
         DC    CL2'SU',C'111477',C'111477',X'970529',C'3 '                      
         DC    CL2'SU',C'111480',C'111480',X'970529',C'3 '                      
         DC    CL2'SU',C'111383',C'111393',X'970523',C'4 '                      
         DC    CL2'SU',C'111423',C'111426',X'970523',C'4 '                      
         DC    CL2'SU',C'111451',C'111453',X'970527',C'4 '                      
         DC    CL2'SU',C'111479',C'111479',X'970529',C'4 '                      
         DC    CL2'SU',C'111485',C'111489',X'970529',C'4 '                      
         DC    CL2'SU',C'111356',C'111356',X'970521',C'6 '                      
         DC    CL2'SU',C'111394',C'111414',X'970523',C'6 '                      
         DC    CL2'SU',C'111427',C'111430',X'970523',C'6 '                      
         DC    CL2'SU',C'111434',C'111434',X'970527',C'6 '                      
         DC    CL2'SU',C'111436',C'111436',X'970527',C'6 '                      
         DC    CL2'SU',C'111454',C'111463',X'970527',C'6 '                      
         DC    CL2'SU',C'111468',C'111468',X'970528',C'6 '                      
         DC    CL2'SU',C'111472',C'111472',X'970529',C'6 '                      
         DC    CL2'SU',C'111475',C'111476',X'970529',C'6 '                      
         DC    CL2'SU',C'111478',C'111478',X'970529',C'6 '                      
         DC    CL2'SU',C'111481',C'111482',X'970529',C'6 '                      
         DC    CL2'SU',C'111490',C'111498',X'970529',C'6 '                      
         DC    CL2'  '                                                          
                                                                                
         EJECT                                                                  
ACZ6D    DSECT                                                                  
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
PARM     DS    6F                                                               
RQSTART  DS    XL3                                                              
RQEND    DS    XL3                                                              
TODAY2   DS    CL2                                                              
DMPSW    DS    CL1                                                              
LASTCPY  DS    XL1                                                              
ELCODE   DS    CL1                                                              
NOPUT    DS    CL1                                                              
WRITESW  DS    CL1                 YES/NO                                       
IOWRK    DS    12D                                                              
IODA     DS    A                                                                
IODIR    DS    XL56                                                             
IOKEY    DS    XL42                                                             
IO       DS    CL2000                                                           
         EJECT                                                                  
CHKNUMD  DSECT                                                                  
CHKCUL   DS    CL2                 CONTRA UNIT LEDGER                           
CHKNUMLO DS    CL6                 LOW CHECK NUM                                
CHKNUMHI DS    CL6                 LOW CHECK NUM                                
CHKDATE  DS    PL3                 CHECK DATE                                   
CHKOFF   DS    CL2                 NEW OFFICE CODE                              
CHKNUMQ  EQU   *-CHKNUMD                                                        
         SPACE 3                                                                
OFFD     DSECT                                                                  
OFFCODE   DS    CL2                                                             
OFFDEBIT  DS    PL8                                                             
OFFCREDT  DS    PL8                                                             
OFFTOTAL  DS    PL8                                                             
OFFLNQ   EQU   *-OFFD                                                           
         SPACE 3                                                                
OFTABD   DSECT                                                                  
OCLIENT  DS    CL3                 CLIENT CODE                                  
OOFFICE  DS    CL2                 CLIENT OFFICE OVERRRIDE                      
OFLEN    EQU   *-OFTABD                                                         
         SPACE 2                                                                
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'162ACREPZ602 08/16/00'                                      
         END                                                                    
