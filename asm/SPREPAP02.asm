*          DATA SET SPREPAP02  AT LEVEL 002 AS OF 10/19/98                      
*PHASE SPAP02A                                                                  
*INCLUDE HEXOUT                                                                 
         SPACE 1                                                                
         TITLE 'SPAP02 - LOCKS AND COPIES APPROVED BG STATUS RECS'              
SPAP02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPAP02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPAP02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    AP00                                                             
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
*        OPEN OUTPUT FILES FOR BY REQUESTS AND SL REQUESTS                      
***********************************************************************         
*                                                                               
AP00     DS    0H                                                               
         OPEN  (BYRQFIL,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (SLRQFIL,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
*        READ BUYING GUIDELINE STATUS RECS - ON XSPDIR                          
***********************************************************************         
*                                                                               
         MVC   AGYHEX,BAGYMD                                                    
         NI    AGYHEX,X'F0'        DROP MEDIA                                   
         ZAP   COUNT,=P'0'                                                      
         XC    XKEY,XKEY                                                        
         MVC   XKEY(2),=X'0E05'    BGL STATUS RECS                              
         MVC   XKEY+2(1),AGYHEX    A/M                                          
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     AP20                                                             
AP20SEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
AP20     CLC   XKEY(2),XKEYSAVE                                                 
         BNE   APX                                                              
         MVC   BYTE,XKEY+2         A/M                                          
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,AGYHEX                                                      
         BNE   APX                                                              
*                                                                               
         LA    R3,XKEY                                                          
         USING BGRKEYD,R3                                                       
         TM    BGRDSTA2,BGRDSTA2_APP   BUY APPROVED                             
         BNO   AP20SEQ                 NO, GET NEXT                             
         CLC   BGRKMINK,=X'FFFFFF'     MORE THAN ONE MINIO REC                  
         BNE   AP70                    THEN JUST TURN OFF APP BIT               
*                                                                               
         CLC   LASTCLT,BGRKCLT     SAME CLIENT                                  
         BE    AP25                                                             
         BAS   RE,RDCLT            GET CLIENT REC FOR CHAR PRDS                 
         MVC   LASTCLT,BGRKCLT                                                  
*                                                                               
***********************************************************************         
*        ADD BY REQUEST                                                         
***********************************************************************         
*                                                                               
AP25     MVC   REQST,SPACES        BUILD BY REQUEST                             
         LA    R4,REQST                                                         
BY       USING QRECORD,R4                                                       
         MVC   BY.QCODE,=C'BY'                                                  
         MVC   BY.QAGY,QAGY                                                     
*                                                                               
         MVC   BYTE,BGRKAM         A/M                                          
         NI    BYTE,X'0F'          JUST MEDIA                                   
         LA    R5,MEDTAB           FIND CHAR MEDIA                              
AP30     CLC   BYTE,1(R5)                                                       
         BE    AP32                                                             
         LA    R5,2(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   AP30                                                             
         DC    H'0'                                                             
AP32     MVC   BY.QMED,0(R5)                                                    
*                                                                               
         GOTO1 CLUNPK,DMCB,BGRKCLT,BY.QCLT                                      
*                                                                               
         CLC   BGRKPRD,BPRDCODE    SAME AS LAST PRODUCT                         
         BE    *+8                                                              
         BAS   RE,FINDPRD          GET CHAR PRD FROM CLT REC                    
         MVC   BY.QPRD,PRDCODE                                                  
*                                                                               
         EDIT  (B1,BGRKEST),(3,BY.QEST),FILL=0                                  
         EDIT  (B2,BGRKMKT),(4,BY.QMKT),FILL=0                                  
*                                                                               
         L     R1,=A(BYRQFIL)                                                   
         LA    R0,REQST                                                         
         PUT   (1),(0)                                                          
*                                                                               
         DROP  BY                                                               
***********************************************************************         
*        ADD SL REQUEST                                                         
***********************************************************************         
*                                                                               
         MVC   REQST,SPACES        BUILD SL REQUEST                             
         LA    R4,REQST                                                         
SL       USING QRECORD,R4                                                       
         MVC   SL.QCODE,=C'SL'                                                  
         MVC   SL.QAGY,QAGY                                                     
         MVC   4(16,R4),=C'*.SL.REP..OV,ASL'                                    
         MVC   20(7,R4),=C'.......'                                             
*                                                                               
         MVC   BYTE,BGRKAM         A/M                                          
         NI    BYTE,X'0F'          JUST MEDIA                                   
         LA    R5,MEDTAB           FIND CHAR MEDIA                              
AP50     CLC   BYTE,1(R5)                                                       
         BE    AP52                                                             
         LA    R5,2(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   AP50                                                             
         DC    H'0'                                                             
AP52     MVC   27(1,R4),0(R5)      MEDIA                                        
         MVI   28(R4),C'.'                                                      
*                                                                               
         GOTO1 CLUNPK,DMCB,BGRKCLT,29(R4)                                       
         MVI   32(R4),C'.'                                                      
*                                                                               
         CLC   BGRKPRD,BPRDCODE    SAME AS LAST PRODUCT                         
         BE    *+8                                                              
         BAS   RE,FINDPRD          GET CHAR PRD FROM CLT REC                    
         MVC   33(3,R4),PRDCODE                                                 
         MVI   36(R4),C'.'                                                      
*                                                                               
         EDIT  (B1,BGRKEST),(3,37(R4)),FILL=0                                   
         MVI   40(R4),C'.'                                                      
         EDIT  (B2,BGRKMKT),(4,41(R4)),FILL=0                                   
         MVC   45(2,R4),=C'.*'                                                  
         DROP  SL                                                               
*                                                                               
         L     R1,=A(SLRQFIL)                                                   
         LA    R0,REQST                                                         
         PUT   (1),(0)                                                          
*                                                                               
***********************************************************************         
*        TURN OFF APPROVED FLAG                                                 
***********************************************************************         
*                                                                               
         USING BGRKEYD,R3                                                       
AP70     LA    R3,XKEY             TURN OFF BUT IN RECORD                       
         MVC   XKEYDA,BGRDDA                                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',XKEYDA,XREC,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,XREC                                                          
         NI    BGRRSTAT+1,X'FF'-BGRDSTA2_APP                                    
         CLI   RCWRITE,C'N'                                                     
         BE    AP80                                                             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',XKEYDA,XREC,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
AP80     LA    R3,XKEY             TURN OFF IN KEY TOO                          
         NI    BGRDSTA2,X'FF'-BGRDSTA2_APP                                      
         CLI   RCWRITE,C'N'                                                     
         BE    AP90                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR',XKEY,XKEY,0                    
*                                                                               
AP90     AP    COUNT,=P'1'                                                      
         B     AP20SEQ             NEXT STATUS REC                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        CLOSE OUTPUT FILES FOR BY REQUESTS AND SL REQUESTS                     
***********************************************************************         
*                                                                               
APX      DS    0H                                                               
         MVC   WORK(80),SPACES                                                  
         MVC   WORK(2),=C'/*'      END OF BY REQUESTS                           
         L     R1,=A(BYRQFIL)                                                   
         LA    R0,WORK                                                          
         PUT   (1),(0)                                                          
         CLOSE (BYRQFIL)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(80),SPACES                                                  
         MVC   WORK(2),=C'/*'      END OF SL REQUESTS                           
         L     R1,=A(SLRQFIL)                                                   
         LA    R0,WORK                                                          
         PUT   (1),(0)                                                          
         CLOSE (SLRQFIL)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(34),=C'NUMBER OF STATUS RECORDS PROCESSED='                    
         EDIT  (P6,COUNT),(6,P+40),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 AENDREQ             FINISHED!                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        READ CLIENT RECORD TO HAVE CHARACTER PRODUCTS                          
***********************************************************************         
*                                                                               
RDCLT    NTR1                                                                   
         LA    R3,XKEY                                                          
         USING BGRKEYD,R3                                                       
         LA    R2,KEY                                                           
         USING CLTHDR,R2                                                        
         XC    KEY,KEY                                                          
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BGRKAM       A/M                                          
         MVC   CKEYCLT,BGRKCLT                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETBUY                                                           
         XC    BPRDCODE,BPRDCODE                                                
*                                  RESTORE STATUS REC SEQ                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        GET CHAR PRODUCT FROM CLIENT REC IN ADBUY                              
***********************************************************************         
*                                                                               
FINDPRD  NTR1                                                                   
         LA    R3,XKEY                                                          
         USING BGRKEYD,R3                                                       
         L     R2,ADBUY                                                         
         USING CLTHDR,R2                                                        
*                                                                               
         CLC   BGRKCLT,CKEYCLT     CORRECT CLIENT                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,CLIST                                                         
FP10     CLC   BGRKPRD,3(R4)                                                    
         BE    FP20                                                             
         LA    R4,4(R4)                                                         
         CLI   3(R4),X'FF'                                                      
         BNE   FP10                                                             
         DC    H'0'                                                             
*                                                                               
FP20     MVC   PRDCODE,0(R4)                                                    
         MVC   BPRDCODE,3(R4)                                                   
         XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        STUFF                                                                  
***********************************************************************         
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
BYRQFIL  DCB   DDNAME=BYRQFIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
SLRQFIL  DCB   DDNAME=SLRQFIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
*                                                                               
MEDTAB   DC    CL1'T',X'01'                                                     
         DC    CL1'R',X'02'                                                     
         DC    CL1'N',X'03'                                                     
         DC    CL1'X',X'04'                                                     
         DC    CL1'C',X'08'                                                     
         DC    X'FF'                                                            
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
* WORKING STORAGE AREA                                                          
*                                                                               
XKEY     DS    CL50                                                             
XKEYSAVE DS    CL50                                                             
XKEYDA   DS    XL4                                                              
AGYHEX   DS    XL1                                                              
ELCODE   DS    XL1                                                              
LASTCLT  DS    XL2                                                              
BPRDCODE DS    CL3                                                              
PRDCODE  DS    CL3                                                              
COUNT    DS    PL6                                                              
REQST    DS    XL80                                                             
XREC     DS    XL2000                                                           
*                                                                               
         EJECT                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBGR                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPAP02 10/19/98'                                      
         END                                                                    
